
-module(ecall_receive).

-include("ecall.hrl").

%%=================================================================
%% OTP API
%%=================================================================
-export([
  start_link/0
]).

-record(state,{ ref2pid = #{}, pid2ref = #{}}).
-record(p2r,{ ref, pid, type}).

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
    {'DOWN', _, process, PID, Reason}->
      case maps:take(PID, State#state.pid2ref) of
        { #p2r{ref = Ref, pid = ClientPID }, P2R }->
          if
            Reason =/= normal->
              catch ecall_connection:send(ClientPID, {'DOWN', Ref, Reason});
            true ->
              ignore
          end,
          worker_loop( State#state{ pid2ref = P2R, ref2pid = maps:remove( Ref, State#state.ref2pid )} );
        _->
          % Who was that?
          worker_loop( State )
      end;
    _Unexpected->
      worker_loop( State )
  end.

%%=================================================================
%% REMOTE API
%%=================================================================
handle_batch([{send, To, Message}| Rest], State)->
  catch To ! Message,
  handle_batch( Rest, State );
handle_batch([{call, Ref, ClientPID,  Module, Function, Args}| Rest], #state{ref2pid = R2P, pid2ref = P2R} = State)->
  {PID,_} =
    spawn_monitor(fun()->
      Result = apply(Module, Function, Args),
      ecall_connection:send( ClientPID, {Ref, Result} )
    end),
  State1 = State#state{
    ref2pid = R2P#{ Ref => PID},
    pid2ref = P2R#{ PID => #p2r{ ref = Ref, pid = ClientPID, type = call }}
  },
  handle_batch( Rest, State1);
handle_batch([], State)->
  State.
