
-module(ecall_receive).

-include("ecall.hrl").

%%=================================================================
%% OTP API
%%=================================================================
-export([
  start_link/0
]).

-record(state,{}).

%%=================================================================
%% OTP API
%%=================================================================
start_link()->
  case whereis( ?MODULE ) of
    PID when is_pid( PID )->
      {error, {already_started, PID}};
    _->
      {ok, spawn_link(fun init_pool/0)}
  end.

init_pool()->

  register( ?MODULE, self() ),

  PoolSize = pool_size(),

  Workers =
    [ spawn_opt(fun()-> worker_loop(#state{}) end,
                [link, {message_queue_data, off_heap}])
      || _ <- lists:seq(1, PoolSize)],

  pg:join(?pg_scope, ?pg_group, self() ),

  master_loop( Workers ).

pool_size()->
  case application:get_env(ecall, pool_size) of
    {ok, PoolSize} when is_integer(PoolSize)->
      PoolSize;
    _->
      erlang:system_info(logical_processors)
  end.     


master_loop( Workers )->
  receive
    {get_workers, Ref, From}->
      catch From ! {Ref, Workers},
      master_loop( Workers );
    _->
      master_loop( Workers )
  end.


worker_loop( State )->
  receive
    {batch, Batch}->
      State1 = handle_batch(Batch, State),
      worker_loop( State1 );
    _Unexpected->
      worker_loop( State )
  end.

%%=================================================================
%% REMOTE API
%%=================================================================
handle_batch([{send, To, Message}| Rest], State)->
  catch To ! Message,
  handle_batch( Rest, State );
handle_batch([{cast, Module, Function, Args}| Rest], State)->
  spawn(Module, Function, Args ),
  handle_batch( Rest, State);
handle_batch([{call, Ref, ClientPID,  Module, Function, Args}| Rest], State)->
  spawn(fun()->
    try
      Result = apply(Module, Function, Args),
      ecall_connection:send( ClientPID, {Ref, Result} )
    catch
      _:Reason->
        ecall_connection:send(ClientPID, {'DOWN', Ref, Reason})
    end
  end),
  handle_batch( Rest, State);
handle_batch([], State)->
  State.
