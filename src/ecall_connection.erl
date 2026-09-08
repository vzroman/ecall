
-module(ecall_connection).

-include("ecall.hrl").

%%=================================================================
%% API
%%=================================================================
-export([
  send/2,
  cast/4,
  call/4
]).

%%=================================================================
%% SERVICE API
%%=================================================================
-export([
  connect/1,
  connection_info/1,
  disconnect/1
]).

%%=================================================================
%% OTP API
%%=================================================================
-export([
  start_link/1
]).

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
% The receiver spawns a monitored process and keeps its PID
% as #{ PID => {Ref, ClientPID }}
% The spawned process executes M:F(As) and sends the result to
% ClientPID as {Ref, Result}.
% On finishing of the spawned process the receiver gets {'DOWN',_,SpawnedPID, Reason}.
% It takes corresponding {Ref, ClientPID} and if the Reason is not 'normal' sends the
%   {'DOWN', Ref, Reason} to ClientPID.
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


-record(connection,{
  node,
  master,
  pool,
  size,
  batch_size
}).

%%=================================================================
%% SERVICE API
%%=================================================================
-spec connect(node()) -> ok | {error, term()}.
connect( Node )->
  case connection_info(Node) of
    {ok, _Info} ->
      ok;
    {error, not_connected} ->
      ecall_connection_sup:start_connection(Node)
  end.

-spec connection_info(node()) ->
  {ok, #{
    status := connected,
    connection_pid := pid(),
    proxy_count := pos_integer(),
    batch_size := pos_integer()
  }}
  | {error, not_connected}.
connection_info( Node )->
  case persistent_term:get(?MODULE, #{}) of
    #{ Node := Connection }->
      #connection{
        master = Master,
        size = Size,
        batch_size = BatchSize
      } = Connection,
      {ok, #{
        status => connected,
        connection_pid => Master,
        proxy_count => Size,
        batch_size => BatchSize
      }};
  _->
      {error, not_connected}
  end.

-spec disconnect(node()) -> ok | {error, term()}.
disconnect( Node )->
  unregister_connection( Node ),
  ecall_connection_sup:stop_connection( Node ).

%%=================================================================
%% OTP API
%%=================================================================
-spec start_link(node()) -> {ok, pid()} | {error, term()}.
start_link( Node )->
  Sup = self(),
  Master = spawn_link(fun()->init_connection(Node, Sup) end),
  receive
    {ready, Master}->
      {ok, Master};
    {error, Master, Error}->
      {error, Error}
  end.

init_connection(Node, Sup)->

  unregister_connection( Node ),

  case get_remote_workers(Node) of
    {ok, Workers}->
      BatchSize = application:get_env(ecall, batch_size, ?BATCH_SIZE),
      Pool =
        maps:from_list(
          [ {I,spawn_opt(fun()->worker_loop(W, BatchSize) end,
                         [link, {message_queue_data, off_heap}])}
            || {I, W} <-
                 lists:zip(lists:seq(0, length(Workers)-1), Workers) ]),

      Connection =
        #connection{ node = Node,
                     master = self(),
                     pool = Pool,
                     size = map_size(Pool),
                     batch_size = BatchSize },
      register_connection( Node, Connection ),

      Sup ! {ready, self()},

      timer:sleep(infinity);
    {error, Error}->
      unlink(Sup),
      Sup ! {error, self(), Error}
  end.

register_connection( Node, Connection )->
  Connections = persistent_term:get( ?MODULE, #{}),
  persistent_term:put(?MODULE, Connections#{ Node => Connection }),
  ok.

unregister_connection( Node )->
  Connections = persistent_term:get( ?MODULE, #{}),
  persistent_term:put(?MODULE, maps:remove( Node, Connections )),
  ok.

get_remote_workers( Node )->
  Ref = make_ref(),
  {ecall_receive, Node} ! {get_workers, Ref, self()},
  receive
    {Ref, Workers}-> {ok, Workers}
  after
    ?CONNECT_TIMEOUT->{error, timeout}
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

get_node_proxy( Node )->
  case persistent_term:get(?MODULE, undefined) of
    #{ Node := Connection }->
      pick_worker( Connection );
    _ ->
      undefined
  end.

pick_worker(#connection{ size = Size, pool = Pool })->
  I = erlang:phash2(self(), Size),
  maps:get(I, Pool ).
