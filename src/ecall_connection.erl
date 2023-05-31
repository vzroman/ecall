
-module(ecall_connection).

-include("ecall.hrl").

%%=================================================================
%% API
%%=================================================================
-export([
  send/2,
  call/4
]).

%%=================================================================
%% SERVICE API
%%=================================================================
-export([
  connect/1,
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
      ecall_erpc:call( Node, Module, Function, Args );
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


-record(connection,{ master, pool, counter }).
%%=================================================================
%% SERVICE API
%%=================================================================
connect( Node )->
  case persistent_term:get(?MODULE, #{}) of
    #{Node := _}->
      throw( already_connected );
    _->
      case supervisor:start_child(ecall_connection_sup,[Node]) of
        {ok,_}-> ok;
        {error,Error} -> throw(Error)
      end
  end.

disconnect( Node )->
  case persistent_term:get(?MODULE, #{}) of
    #{Node := #connection{master = Master}}->
      case supervisor:terminate_child(ecall_connection_sup, Master) of
        ok -> ok;
        {error, Error} -> throw( Error )
      end;
    _->
      throw( not_connected )
  end.

%%=================================================================
%% OTP API
%%=================================================================
start_link( Node )->

  Sup = self(),
  Master = spawn_link(fun()->init_connection(Node, Sup) end),
  receive
    {ready, Master}-> {ok, Master};
    {error, Master, Error}->
      {error, Error}
  end.

init_connection(Node, Sup)->

  process_flag(trap_exit,true),

  Self = self(),

  Ref = make_ref(),
  {ecall_receive, Node} ! {get_workers, Ref, self()},

  receive
    {Ref, Workers}->
      Counter = atomics:new(1,[{signed,false}]),
      Pool =
        maps:from_list([ {I,spawn_link(fun()->worker_loop(W) end)} || {I, W} <- lists:zip( lists:seq(0, length(Workers)-1), Workers) ]),
      Connections = persistent_term:get( ?MODULE, #{}),

      Connection = #connection{ master = Self, pool = Pool, counter = Counter },
      persistent_term:put(?MODULE, Connections#{ Node => Connection }),

      Sup ! {ready, Self},

      master_loop( Connection )

  after
    ?CONNECT_TIMEOUT->
      unlink(Sup),
      Sup ! {error, Self, connect_timeout}
  end.

master_loop( Connection )->
  receive
    {'EXIT',_, Reason}->
      exit( Reason );
    _->
      master_loop( Connection )
  end.

%%=================================================================
%% WORKER LOOP
%%=================================================================
worker_loop( Remote )->
  Requests = collect_requests( _Count = 0 ),
  catch Remote ! {batch, node(), Requests},
  worker_loop( Remote ).

collect_requests( Count ) when 0 < Count, Count < ?BATCH_SIZE->
  receive
    {do, Request}-> [Request| collect_requests( Count + 1)]
  after
    0 -> []
  end;
collect_requests( _Count = 0 )->
  receive
    {do, Request}-> [Request| collect_requests( 1 )]
  end;
collect_requests( _Count )->
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

pick_worker( #connection{ counter = Counter, pool = Pool } )->
  Size = map_size( Pool ),

  I = atomics:add_get( Counter, 1, 1 ),
  Index = (I rem Size),

  if
    I =:= Size -> atomics:sub(Counter,1, Size);
    true -> ignore
  end,

  maps:get( Index, Pool ).
