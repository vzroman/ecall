
-module(ecall_connection).

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

% The record published in persistent_term under {?MODULE, Node}, only while
% the pool is up. It is written only by the connection master of that node.
-record(connection,{
  node,
  master,
  pool,
  size,
  batch_size,
  incarnation,        % pid of the incarnation process guarding this pool
  remote_incarnation  % remote ecall_receive master pid the pool is bound to
}).

% connection master state
-record(state,{
  node,
  parent,
  state,        % idle | building | up
  batch_size,
  ref,          % ref of the get_workers request in flight, building only
  remote,       % remote master pid of the current pool
  incarnation,  % incarnation pid
  monitor,      % monitor ref on the incarnation
  pool          % #{ Index => Worker }
}).

%%=================================================================
%% API
%%=================================================================
send( To, Message )->
  case get_proxy( To ) of
    undefined ->
      To ! Message;
    {Proxy, RemoteTo} ->
      Proxy ! {do, {send, RemoteTo, Message}},
      Message
  end.

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
% Ensures a connection master for the node. Asynchronous: the master fetches
% the remote worker list in its loop, poll connection_info/1 for the result.
-spec connect(node()) -> ok | {error, term()}.
connect( Node )->
  case ecall_connection_sup:start_connection( Node ) of
    {ok, _Master}-> ok;
    {error, _} = Error-> Error
  end.

% Called by ecall_pg_monitor with the remote ecall_receive master pid from
% a pg join. The master decides whether the pool has to be (re)built.
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
    proxy_count := pos_integer(),
    batch_size := pos_integer()
  }}
  | {ok, #{ status := down, connection_pid := pid() }}
  | {error, not_connected}.
connection_info( Node )->
  case persistent_term:get(?KEY(Node), undefined) of
    #connection{ master = Master, size = Size, batch_size = BatchSize }->
      {ok, #{
        status => connected,
        connection_pid => Master,
        proxy_count => Size,
        batch_size => BatchSize
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
init_master( Node, Parent )->

  process_flag( trap_exit, true ),
  process_flag( priority, high ),

  % a no-op on a fresh start; after a master crash it removes the stale
  % entry of the dead master before the bootstrap
  persistent_term:erase( ?KEY(Node) ),

  State = #state{ node = Node, parent = Parent, state = idle },

  % bootstrap: works after a supervisor restart and for an explicit connect/1,
  % neither of which produces a pg join. No reply means idle until a join.
  master_loop( request_workers( {ecall_receive, Node}, State ) ).

master_loop( #state{ state = idle } = State )-> idle( State );
master_loop( #state{ state = building } = State )-> building( State );
master_loop( #state{ state = up } = State )-> up( State ).

% idle: no pool, no request in flight. Handles join and the parent exit;
% replies and DOWNs cannot belong to this state and are dropped.
idle( #state{ parent = Parent } = State )->
  receive
    {join, RemoteMaster} when is_pid( RemoteMaster )->
      master_loop( request_workers( RemoteMaster, State ) );
    {'EXIT', Parent, Reason}->
      exit( Reason );
    _Stale->
      idle( State )
  end.

% building: one get_workers request in flight. Handles the reply carrying
% the current ref, join (a new request replaces the pending one) and the
% parent exit; replies with a stale ref are dropped.
building( #state{ parent = Parent, ref = Ref } = State )->
  receive
    {Ref, RemoteMaster, Workers} when is_pid( RemoteMaster ), is_list( Workers )->
      master_loop( build( RemoteMaster, Workers, State ) );
    {join, RemoteMaster} when is_pid( RemoteMaster )->
      master_loop( request_workers( RemoteMaster, State ) );
    {'EXIT', Parent, Reason}->
      exit( Reason );
    _Stale->
      building( State )
  end.

% up: pool published. Handles join (same remote master: nothing to do,
% another one: rebuild), the incarnation DOWN, the parent exit and a
% worker exit (fatal, the supervisor restarts the master).
up( #state{ parent = Parent, monitor = Monitor, remote = Remote } = State )->
  receive
    {join, Remote}->
      up( State );
    {join, RemoteMaster} when is_pid( RemoteMaster )->
      master_loop( request_workers( RemoteMaster, invalidate( State ) ) );
    {'DOWN', Monitor, process, _Incarnation, _Reason}->
      master_loop( invalidate( State#state{ monitor = undefined } ) );
    {'EXIT', Parent, Reason}->
      teardown( State ),
      exit( Reason );
    {'EXIT', Worker, Reason}->
      teardown( State ),
      exit( {worker_down, Worker, Reason} );
    _Stale->
      up( State )
  end.

request_workers( To, State )->
  Ref = make_ref(),
  catch erlang:send( To, {get_workers, Ref, self()}, [noconnect] ),
  State#state{ state = building, ref = Ref }.

% An empty remote pool (pool_size 0) cannot be routed to: stay idle on the
% native path and wait for the next join.
build( _RemoteMaster, [], State )->
  State#state{ state = idle, ref = undefined };
build( RemoteMaster, Workers, State )->
  case spawn_incarnation( RemoteMaster ) of
    {ok, Incarnation, Monitor}->
      BatchSize = application:get_env(ecall, batch_size, ?BATCH_SIZE),
      Pool =
        maps:from_list(
          [ {I,spawn_opt(fun()->worker_loop(W, BatchSize) end,
                         [link, {message_queue_data, off_heap}])}
            || {I, W} <-
                 lists:zip(lists:seq(0, length(Workers)-1), Workers) ]),
      State1 =
        State#state{
          state = up,
          ref = undefined,
          remote = RemoteMaster,
          incarnation = Incarnation,
          monitor = Monitor,
          pool = Pool,
          batch_size = BatchSize
        },
      publish( State1 ),
      State1;
    {error, _Reason}->
      % the remote master is already dead, wait for the next join
      State#state{ state = idle, ref = undefined }
  end.

% Invalidate the current pool and go idle.
invalidate( State )->
  teardown( State ),
  State#state{
    state = idle,
    remote = undefined,
    incarnation = undefined,
    monitor = undefined,
    pool = undefined
  }.

% The incarnation goes first: exit/2 is asynchronous, so while it is still
% monitored (its DOWN not consumed yet) wait for the DOWN. The wait is bounded,
% kill is untrappable, and belongs to the transition. Then the entry is
% erased, so get_proxy stops handing out proxies before they die.
teardown( #state{
  node = Node,
  incarnation = Incarnation,
  monitor = Monitor,
  pool = Pool
} )->
  if
    is_reference( Monitor )->
      exit( Incarnation, kill ),
      receive
        {'DOWN', Monitor, process, Incarnation, _Reason}-> ok
      end;
    true->
      ok
  end,
  persistent_term:erase( ?KEY(Node) ),
  kill_pool( Pool ).

% Callers waiting in call/4 monitor their proxy and get
% {error, {badrpc, noconnection}}, the same as erpc on a lost connection.
kill_pool( Pool )->
  [ begin unlink( W ), exit( W, noconnection ) end || W <- maps:values( Pool ) ],
  ok.

publish( #state{
  node = Node,
  pool = Pool,
  batch_size = BatchSize,
  incarnation = Incarnation,
  remote = Remote
} )->
  persistent_term:put(?KEY(Node), #connection{
    node = Node,
    master = self(),
    pool = Pool,
    size = map_size( Pool ),
    batch_size = BatchSize,
    incarnation = Incarnation,
    remote_incarnation = Remote
  }).

%%=================================================================
%% INCARNATION
%%=================================================================
% The wait for ready is local and bounded (the incarnation either reports
% or dies) and belongs to the building -> up transition.
spawn_incarnation( RemoteMaster )->
  Master = self(),
  {Incarnation, Monitor} =
    spawn_opt(fun()-> incarnation( Master, RemoteMaster ) end,
              [monitor, {priority, high}]),
  receive
    {ready, Incarnation}->
      {ok, Incarnation, Monitor};
    {'DOWN', Monitor, process, Incarnation, Reason}->
      {error, Reason}
  end.

% Lives exactly as long as the remote receive master is reachable.
% A link, not a monitor: a link exit terminates this process inside signal
% handling, so is_process_alive callers queued behind the exit signal get false.
% It must never receive messages: the is_process_alive fast path needs an
% empty signal queue.
incarnation( Master, RemoteMaster )->
  MasterMonitor = erlang:monitor( process, Master ),
  link( RemoteMaster ),
  Master ! {ready, self()},
  receive
    {'DOWN', MasterMonitor, process, Master, _Reason}->
      exit( shutdown )
  end.

%%=================================================================
%% WORKER LOOP
%%=================================================================
worker_loop( Remote, BatchSize )->
  erlang:garbage_collect(self()),
  Requests = collect_requests( _Count = 0, BatchSize ),
  catch Remote ! {batch, Requests},
  worker_loop( Remote, BatchSize ).

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
get_node_proxy( Node )->
  case persistent_term:get(?KEY(Node), undefined) of
    #connection{} = Connection ->
      pick_worker( Connection );
    undefined ->
      undefined
  end.

pick_worker(#connection{ size = Size, pool = Pool })->
  I = erlang:phash2(self(), Size),
  maps:get(I, Pool ).
