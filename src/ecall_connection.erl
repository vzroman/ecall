
-module(ecall_connection).

-include("ecall.hrl").

%%=================================================================
%% API
%%=================================================================
-export([
  send/2
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
      Proxy ! {do, {send, RemoteTo, Message}}
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
        maps:from_list([ {I,spawn_link(fun()->init_worker(W, Self) end)} || {I, W} <- lists:zip( lists:seq(0, length(Workers)-1), Workers) ]),
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

-record(state,{remote, master}).
init_worker( Remote, Master )->

  process_flag(trap_exit,true),

  worker_loop(#state{ master = Master, remote = Remote } ).


%%=================================================================
%% WORKER API
%%=================================================================
worker_loop( State )->
  case try collect_buffer( 0 ) catch _:_Reason->{exit, _Reason} end of
    Buffer when is_list( Buffer )->
      State1 = do_send(Buffer, State),
      worker_loop( State1 );
    {exit, Reason}->
      % TODO. Handle active monitors
      exit( Reason )
  end.


collect_buffer( Count ) when Count < ?BATCH_SIZE->
  Timeout =
    if
      Count =:= 0-> infinity;
      true -> 0
    end,
  receive
    {do, Request}->
      [Request | collect_buffer( Count + 1 )];
    {'EXIT', _, Reason} ->
      throw( Reason )
  after
    Timeout -> []
  end.

do_send(Buffer, #state{ remote = Remote } = State)->
  Remote ! Buffer,
  State.

%%=================================================================
%% UTILITIES
%%=================================================================
get_proxy({ Service, Node }) ->
  case persistent_term:get(?MODULE, undefined) of
    #{ Node := Connection }->
      { pick_worker( Connection ), Service};
    _ ->
      undefined
  end;
get_proxy( To ) when is_pid( To )->
  Node = node( To ),
  case persistent_term:get(?MODULE, undefined) of
    #{ Node := Connection }->
      { pick_worker( Connection ), To};
    _ ->
      undefined
  end;
get_proxy( _To )->
  undefined.

pick_worker( #connection{ counter = Counter, pool = Pool } )->
  Size = map_size( Pool ),

  I = atomics:add_get( Counter, 1, 1 ),
  Index = (I rem Size),

  if
    I =:= Size -> atomics:sub(Counter,1, Size);
    true -> ignore
  end,

  maps:get( Index, Pool ).
