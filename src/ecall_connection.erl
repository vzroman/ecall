
-module(ecall_connection).
-moduledoc false.

-include("ecall.hrl").

%%=================================================================
%% API
%%=================================================================
-export([
  send/2,
  cast/4,
  call/4,
  call_reply/2   % used by ecall_receive for the replies of remote calls
]).

%%=================================================================
%% SERVICE API
%%=================================================================
-export([
  connect/1,
  connect/2,
  connection_info/1,
  disconnect/1
]).

%%=================================================================
%% OTP API
%%=================================================================
-export([
  start_link/1
]).

-define(KEY(Node), {?MODULE, Node}).

-type proxy_pool() :: #{non_neg_integer() => pid()}.
-type connection_state() :: request_pool | register | connected | wait_join.

% The record published in persistent_term under {?MODULE, Node}, only while
% the pool is up. It is written only by the connection master of that node.
-record(connection,{
  node :: node(),
  master :: pid(),
  pool :: proxy_pool(),
  size :: pos_integer(),
  incarnation :: pid(),       % incarnation process guarding this pool
  remote_incarnation :: pid() % remote receive master the pool is bound to
}).

% connection master state
-record(state,{
  node :: node(),
  parent :: pid(),
  state :: connection_state(),
  remote :: pid() | undefined,      % remote receive master of the current pool
  incarnation :: pid() | undefined,
  monitor :: reference() | undefined,
  pool :: proxy_pool() | undefined,
  pending_join :: pid() | undefined % join while connected, acted on in wait_join
}).

% The connection master is a state machine, one master_loop/1 clause per state:
%   request_pool: entered at start and after a join. Asks the registered
%     ecall_receive on Node for its master and workers through erpc.
%     Success: build_connection spawns the incarnation and the pool -> register.
%     Any error or exception -> wait_join.
%   register: the single persistent_term:put of the pool -> connected.
%   connected: the pool is in use. The incarnation DOWN tears the pool down
%     -> wait_join. The parent exit and a worker exit tear it down and exit.
%     A join with the current remote pid is ignored. A join with another pid
%     is kept in pending_join: the incarnation DOWN and a successor's join
%     reach the master by different paths, so their order is not guaranteed,
%     and a join queued during request_pool is only received here.
%     Everything else is dropped.
%   wait_join: no pool. A pending join -> request_pool at once. Otherwise
%     a join -> request_pool, the parent exit -> exit, everything else is
%     dropped.
-define(wait_join, wait_join).
-define(request_pool, request_pool).
-define(register, register).
-define(connected, connected).

%%=================================================================
%% API
%%=================================================================
-spec send(erlang:send_destination(), Message) -> Message when Message :: term().
send( To, Message )->
  case get_proxy( To ) of
    undefined ->
      To ! Message;
    {Proxy, RemoteTo} ->
      Proxy ! {do, {send, RemoteTo, Message}},
      Message
  end.

-spec cast(node(), module(), atom(), [term()]) -> ok.
cast(Node, Module, Function, Args)->
  case get_node_proxy( Node ) of
    undefined ->
      erpc:cast( Node, Module, Function, Args );
    Proxy ->
      Proxy ! {do, {cast, Module, Function, Args}},
      ok
  end.

% Make a reference and send:
%   {do, {call, Ref, self(), M, F, As}}
% to a proxy.
% The receiver spawns a process monitored with the tag {'DOWN', Ref, ClientPID},
% so it keeps nothing in its state.
% The spawned process executes M:F(As) and sends the result to
% ClientPID as {Ref, Result}, or {'DOWN', Ref, Reason} if M:F(As) raises.
% On finishing of the spawned process the receiver gets
%   {{'DOWN', Ref, ClientPID}, MonRef, process, SpawnedPID, Reason}
% and if the Reason is not 'normal' (the process was killed) sends the
%   {'DOWN', Ref, Reason} to ClientPID.
% Both replies go through call_reply/2 on the receiving node, which checks
% the incarnation of its pool back to ClientPID and goes natively when the
% pool is stale, so a reply is never lost in a pool of a dead incarnation.
% The caller monitors its proxy: the proxies of a connection are killed with
% reason noconnection when the connection goes down, so a call never hangs on
% a dead connection, it returns {error, {badrpc, noconnection}}.
-spec call(node(), module(), atom(), [term()]) ->
  {ok, term()} | {error, term()}.
call(Node, Module, Function, Args)->
  case get_node_proxy( Node ) of
    undefined ->
      try
        case erpc:call(Node, Module, Function, Args) of
          {error,_}=CallError -> CallError;
          CallResult -> {ok, CallResult}
        end
      catch
        throw:Error -> {error, Error};
        exit:{_,Reason} -> {error,{exit, Reason}};
        error:{exception, Error, _Stack}-> {error, {exit,Error}};
        error:{erpc, Reason}->{error,{badrpc, Reason}};
        _:Error-> {error,{unexpected, Error}}
      end;
    Proxy ->
      Ref = erlang:monitor( process, Proxy ),
      try
        Proxy ! {do, {call, Ref, self(),  Module, Function, Args}},
        receive
          {Ref, {error, _} = Error} -> Error;
          {Ref, Result} -> {ok, Result};
          {'DOWN', Ref, Reason}-> {error, {exit,Reason}};
          {'DOWN', Ref, process, _, Reason}->{error,{badrpc, Reason}}
        end
      after
        erlang:demonitor(Ref, [flush])
      end
  end.

% Reply of a remote call. A reply lost in a stale pool hangs the caller
% forever, so this is the one path that checks the pool's incarnation:
% a dead incarnation means the pool is stale and the reply goes natively,
% routed by the pid's own creation.
-spec call_reply(pid(), term()) -> ok.
call_reply( ClientPID, Reply )->
  case persistent_term:get( ?KEY(node(ClientPID)), undefined ) of
    #connection{ incarnation = Incarnation } = Connection ->
      case is_process_alive( Incarnation ) of
        true -> pick_worker( Connection ) ! {do, {send, ClientPID, Reply}};
        false -> ClientPID ! Reply
      end;
    undefined ->
      ClientPID ! Reply
  end,
  ok.

%%=================================================================
%% SERVICE API
%%=================================================================
% Ensures a connection master for the node. Asynchronous: the master asks the
% registered ecall_receive on the node for its pool in its own process, poll
% connection_info/1 for the result.
-spec connect(node()) -> ok | {error, term()}.
connect( Node )->
  case ecall_connection_sup:start_connection( Node ) of
    {ok, _Master}-> ok;
    {error, _} = Error-> Error
  end.

% Called by ecall_pg_monitor with the remote ecall_receive master pid from
% a pg join. Ensures the master and forwards the join; the master acts on a
% join only in wait_join, while it has no pool.
-spec connect(node(), pid()) -> ok | {error, term()}.
connect( Node, RemoteMaster )->
  case ecall_connection_sup:start_connection( Node ) of
    {ok, Master}->
      Master ! {join, RemoteMaster},
      ok;
    {error, _} = Error-> Error
  end.

% connected: a pool is published and in use. down: a connection master
% exists but no pool is published. not_connected: no master.
-spec connection_info(node()) ->
  {ok, #{
    status := connected,
    connection_pid := pid(),
    proxy_count := pos_integer()
  }}
  | {ok, #{ status := down, connection_pid := pid() }}
  | {error, not_connected}.
connection_info( Node )->
  case persistent_term:get(?KEY(Node), undefined) of
    #connection{ master = Master, size = Size }->
      {ok, #{
        status => connected,
        connection_pid => Master,
        proxy_count => Size
      }};
    undefined->
      case ecall_connection_sup:connection_master( Node ) of
        Master when is_pid( Master )->
          {ok, #{ status => down, connection_pid => Master }};
        undefined->
          {error, not_connected}
      end
  end.

% The only stop path: the master tears the pool down and erases its entry
% while the supervisor waits for it, the erase here covers a dead master.
-spec disconnect(node()) -> ok.
disconnect( Node )->
  case ecall_connection_sup:stop_connection( Node ) of
    ok->
      persistent_term:erase( ?KEY(Node) );
    {error, _}->
      % a join racing the stop restarted the master, its entry stays
      ok
  end,
  ok.

%%=================================================================
%% OTP API
%%=================================================================
-spec start_link(node()) -> {ok, pid()}.
start_link( Node )->
  Parent = self(),
  {ok, spawn_link(fun()-> init_master( Node, Parent ) end)}.

%%=================================================================
%% CONNECTION MASTER
%%=================================================================
-spec init_master(node(), pid()) -> no_return().
init_master( Node, Parent )->

  process_flag( trap_exit, true ),
  process_flag( priority, high ),

  % a no-op on a fresh start; after a master crash it removes the stale
  % entry of the dead master before the first pool request
  persistent_term:erase( ?KEY(Node) ),

  master_loop(#state{
    node = Node,
    parent = Parent,
    state = ?request_pool
  }).

-spec master_loop(#state{}) -> no_return().
master_loop(#state{
  state = ?request_pool,
  node = Node
} =State)->
  % Entered at start (a supervisor restart or an explicit connect/1, neither
  % of which produces a join) and after a join. Always asks the registered
  % ecall_receive on Node, never the joined pid; any failure -> wait_join.
  % The erpc call has no timeout: it is bounded by the connection setup toward
  % an unreachable node and by the net tick toward a hung one, and nothing is
  % published meanwhile.
  ?LOGINFO("requesting receive pool info from ~p",[Node]),
  case
    try erpc:call(Node, ecall_receive, get_pool, [])
    catch _:E->  {error, E}
  end of
    {ok, {RemoteMaster, Workers}}->
      ?LOGINFO("received pool info from ~p, activating connection",[Node]),
      master_loop( build_connection(RemoteMaster, Workers, State) );
    {error, Error}->
      ?LOGWARNING("unable to get pool info from ~p, error: ~p",[Node, Error]),
      master_loop(State#state{state = ?wait_join})
  end;

master_loop(#state{
  state = ?register,
  node = Node,
  pool = Pool,
  incarnation = Incarnation,
  remote = Remote
} =State)->
  persistent_term:put(?KEY(Node), #connection{
    node = Node,
    master = self(),
    pool = Pool,
    size = map_size( Pool ),
    incarnation = Incarnation,
    remote_incarnation = Remote
  }),
  master_loop(State#state{
    state = ?connected
  });
master_loop(#state{
  state = ?connected,
  monitor = Monitor,
  parent = Parent,
  remote = Remote
} =State)->
  receive
    {'DOWN', Monitor, process, _Incarnation, _Reason}->
      teardown(State),
      master_loop(State#state{
        state = ?wait_join,
        remote = undefined,
        incarnation = undefined,
        monitor = undefined,
        pool = undefined
      });
    {join, Remote}->
      master_loop(State);
    {join, UnexpectedRemote}->
      master_loop(State#state{
        pending_join = UnexpectedRemote
      });
    {'EXIT', Parent, Reason}->
      teardown( State ),
      exit( Reason );
    {'EXIT', Worker, Reason}->
      teardown( State ),
      exit( {worker_down, Worker, Reason} );
    _Stale->
      master_loop( State )
  end;
master_loop(#state{
  state = ?wait_join,
  pending_join = Remote,
  node = Node
} =State) when is_pid(Remote)->
  ?LOGINFO("handling pending join from ~p, activating connection...",[Node]),
  master_loop(State#state{
    state = ?request_pool,
    remote = Remote,
    pending_join = undefined
  });
master_loop(#state{
  state = ?wait_join,
  parent = Parent,
  node = Node
} =State)->
  receive
    {join, Remote}->
      ?LOGINFO("~p joined, activating connection...",[Node]),
      master_loop(State#state{
        state = ?request_pool,
        remote = Remote
      });
    {'EXIT', Parent, Reason}->
      exit( Reason );
    _Stale->
      master_loop( State )
  end.

-spec build_connection(pid(), nonempty_list(pid()), #state{}) -> #state{}.
build_connection(RemoteMaster, Workers, State)->
  case spawn_incarnation( RemoteMaster ) of
    {ok, Incarnation, Monitor}->
      Pool = build_pool(Workers),
      State#state{
        state = ?register,
        remote = RemoteMaster,
        incarnation = Incarnation,
        monitor = Monitor,
        pool = Pool
      };
    {error, Reason}->
      % the remote master is already dead, wait for the next join
      ?LOGWARNING("unable to activate connection to ~p, reason: ~p",[
        State#state.node, Reason
      ]),
      State#state{state = ?wait_join}
  end.

-spec build_pool(nonempty_list(pid())) -> proxy_pool().
build_pool(RemoteWorkers)->
  BatchSize = application:get_env(ecall, batch_size, ?BATCH_SIZE),
  LocalWorkers =
    [spawn_opt(
        fun()->
          worker_loop(W, BatchSize)
        end,
        [link, {message_queue_data, off_heap}]
      ) || W <- RemoteWorkers
    ],
  maps:from_list(lists:zip(lists:seq(0, length(LocalWorkers)-1), LocalWorkers)).

% The entry is erased before the pool is killed, so get_proxy stops handing
% out proxies before they die. The incarnation is not touched: on the DOWN
% path it is already dead, on the exit paths it dies through its monitor on
% the master.
-spec teardown(#state{pool :: proxy_pool()}) -> ok.
teardown( #state{
  node = Node,
  pool = Pool
} )->
  ?LOGWARNING("stop connection to ~p",[Node]),
  persistent_term:erase( ?KEY(Node) ),
  kill_pool( Pool ).

% Callers waiting in call/4 monitor their proxy and get
% {error, {badrpc, noconnection}}, the same as erpc on a lost connection.
-spec kill_pool(proxy_pool()) -> ok.
kill_pool( Pool )->
  [ begin
      unlink( W ),
      exit( W, noconnection )
    end || W <- maps:values( Pool ) ],
  ok.

%%=================================================================
%% INCARNATION
%%=================================================================
% The wait for ready is local and bounded (the incarnation either reports
% or dies) and belongs to the request_pool -> register transition.
-spec spawn_incarnation(pid()) ->
  {ok, pid(), reference()} | {error, term()}.
spawn_incarnation( RemoteMaster )->
  Master = self(),
  {Incarnation, Monitor} =
    spawn_opt(
      fun()->
        % Lives exactly as long as the remote receive master is reachable.
        % A link exit terminates this process inside signal handling, so
        % is_process_alive callers queued behind the exit signal get false.
        % It must never receive messages: the is_process_alive fast path
        % needs an empty signal queue.
        MasterMonitor = erlang:monitor( process, Master ),
        link( RemoteMaster ),
        Master ! {ready, self()},
        receive
          {'DOWN', MasterMonitor, process, Master, _Reason}->
            exit( shutdown )
        end
      end,
      [monitor, {priority, high}]
    ),
  receive
    {ready, Incarnation}->
      {ok, Incarnation, Monitor};
    {'DOWN', Monitor, process, Incarnation, Reason}->
      {error, Reason}
  end.

%%=================================================================
%% WORKER LOOP
%%=================================================================
-spec worker_loop(pid(), pos_integer()) -> no_return().
worker_loop( Remote, BatchSize )->
  erlang:garbage_collect(self()),
  Requests = collect_requests( _Count = 0, BatchSize ),
  Remote ! {batch, Requests},
  worker_loop( Remote, BatchSize ).

-spec collect_requests(non_neg_integer(), pos_integer()) ->
  [ecall_receive:request()].
collect_requests( Count, BatchSize ) when 0 < Count, Count < BatchSize->
  receive
    {do, Request}-> [Request| collect_requests( Count + 1, BatchSize)]
  after
    0 -> []
  end;
collect_requests( _Count = 0, BatchSize )->
  receive
    {do, Request}-> [Request| collect_requests( 1, BatchSize)]
  end;
collect_requests( _Count, _BatchSize )->
  [].

%%=================================================================
%% UTILITIES
%%=================================================================
-spec get_proxy(erlang:send_destination()) ->
  {pid(), pid() | atom()} | undefined.
get_proxy({ Service, Node }) ->
  case get_node_proxy( Node ) of
    undefined ->
      undefined;
    Proxy ->
      { Proxy, Service }
  end;
get_proxy( To ) when is_pid( To )->
  case get_node_proxy( node(To) ) of
    undefined ->
      undefined;
    Proxy ->
      { Proxy, To }
  end;
get_proxy( _To )->
  undefined.

% Plain lookup: the entry exists only while the pool is up.
-spec get_node_proxy(node()) -> pid() | undefined.
get_node_proxy( Node )->
  case persistent_term:get(?KEY(Node), undefined) of
    #connection{} = Connection ->
      pick_worker( Connection );
    undefined ->
      undefined
  end.

-spec pick_worker(#connection{}) -> pid().
pick_worker(#connection{ size = Size, pool = Pool })->
  I = erlang:phash2(self(), Size),
  maps:get(I, Pool ).
