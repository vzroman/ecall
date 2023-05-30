
-module(ecall_receive).

-include("ecall.hrl").

%%=================================================================
%% OTP API
%%=================================================================
-export([
  start_link/0
]).

-record(state,{ ref2pid = #{}, pid2ref = #{}}).

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

  PoolSize = erlang:system_info(logical_processors),

  Workers =
    [ spawn_link(fun()-> worker_loop(#state{}) end) || _ <- lists:seq(1, PoolSize)],

  pg:join(?pg_scope, ?pg_group, self() ),

  master_loop( Workers ).

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
    {batch, _Node, Batch}->
      State1 = handle_batch(Batch, State),
      worker_loop( State1 );
    _->
      worker_loop( State )
  end.

%%=================================================================
%% REMOTE API
%%=================================================================
handle_batch([{send, To, Message}| Rest], State)->
  catch To ! Message,
  handle_batch( Rest, State );
handle_batch([], State)->
  State.

