-module(ecall_receive_SUITE).

-include_lib("common_test/include/ct.hrl").

-define(POOL_SIZE, 2).

%%=================================================================
%% COMMON TEST API
%%=================================================================
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

%%=================================================================
%% TEST CASES
%%=================================================================
-export([
  reply_carries_master_pid/1,
  master_survives_peer_incarnation_exits/1,
  worker_death_restarts_whole_pool/1,
  parent_exit_propagates_reason/1
]).

%%=================================================================
%% COMMON TEST API
%%=================================================================
all() ->
  [
    reply_carries_master_pid,
    master_survives_peer_incarnation_exits,
    worker_death_restarts_whole_pool,
    parent_exit_propagates_reason
  ].

init_per_suite(Config) ->
  case pg:start_link(ecall) of
    {ok, Pg} ->
      unlink(Pg),
      [{pg, Pg} | Config];
    {error, {already_started, _Pg}} ->
      Config
  end.

end_per_suite(Config) ->
  case whereis(ecall_receive) of
    Master when is_pid(Master) ->
      exit(Master, kill),
      wait_until_dead(Master);
    undefined ->
      ok
  end,
  case proplists:get_value(pg, Config) of
    Pg when is_pid(Pg) ->
      gen_server:stop(Pg);
    undefined ->
      ok
  end,
  ok.

init_per_testcase(_, Config) ->
  case whereis(ecall_receive) of
    Master when is_pid(Master) ->
      exit(Master, kill),
      wait_until_dead(Master),
      wait_until_unregistered(ecall_receive);
    undefined ->
      ok
  end,
  Config.

end_per_testcase(_, _Config) ->
  ok.

%%=================================================================
%% TEST CASES
%%=================================================================
reply_carries_master_pid(_Config) ->
  Master = start_receive(),
  Workers = get_workers(Master),
  ?POOL_SIZE = length(Workers),
  true = lists:all(fun erlang:is_process_alive/1, Workers),

  shutdown_receive(Master),
  [ wait_until_dead(Worker) || Worker <- Workers ],
  ok.

master_survives_peer_incarnation_exits(_Config) ->
  Master = start_receive(),
  Workers = get_workers(Master),

  spawn(fun() -> link(Master), exit(killed) end),
  spawn(fun() -> link(Master), exit(noconnection) end),
  spawn(fun() -> link(Master), exit(normal) end),
  timer:sleep(50),

  true = erlang:is_process_alive(Master),
  Workers = get_workers(Master),

  shutdown_receive(Master).

worker_death_restarts_whole_pool(_Config) ->
  Master = start_receive(),
  [Worker | Others] = get_workers(Master),

  exit(Worker, kill),
  receive
    {'EXIT', Master, {worker_down, Worker, killed}} ->
      ok;
    {'EXIT', Master, Other} ->
      ct:fail({unexpected_master_exit, Other})
  after
    1000 ->
      ct:fail(master_did_not_exit)
  end,
  [ wait_until_dead(Pid) || Pid <- Others ],
  ok.

parent_exit_propagates_reason(_Config) ->
  Master = start_receive(),
  shutdown_receive(Master).

%%=================================================================
%% UTILITIES
%%=================================================================
start_receive() ->
  process_flag(trap_exit, true),
  {ok, Master} = ecall_receive:start_link(?POOL_SIZE),
  wait_until_registered(ecall_receive, Master),
  Master.

get_workers(Master) ->
  Ref = make_ref(),
  Master ! {get_workers, Ref, self()},
  receive
    {Ref, Master, Workers} ->
      Workers;
    {Ref, Other} ->
      ct:fail({reply_without_master_pid, Other})
  after
    1000 ->
      ct:fail(get_workers_timeout)
  end.

shutdown_receive(Master) ->
  exit(Master, shutdown),
  receive
    {'EXIT', Master, shutdown} ->
      ok;
    {'EXIT', Master, Other} ->
      ct:fail({unexpected_master_exit, Other})
  after
    1000 ->
      ct:fail(master_did_not_exit)
  end.

wait_until_dead(Pid) ->
  wait_until_dead(Pid, _Attempts = 50).

wait_until_dead(Pid, Attempts) when Attempts > 0 ->
  case erlang:is_process_alive(Pid) of
    false ->
      ok;
    true ->
      timer:sleep(20),
      wait_until_dead(Pid, Attempts - 1)
  end;
wait_until_dead(Pid, 0) ->
  ct:fail({process_still_alive, Pid}).

wait_until_registered(Name, Pid) ->
  wait_until_registered(Name, Pid, _Attempts = 50).

wait_until_registered(Name, Pid, Attempts) when Attempts > 0 ->
  case whereis(Name) of
    Pid ->
      ok;
    _Other ->
      timer:sleep(20),
      wait_until_registered(Name, Pid, Attempts - 1)
  end;
wait_until_registered(Name, Pid, 0) ->
  ct:fail({not_registered, Name, Pid}).

wait_until_unregistered(Name) ->
  wait_until_unregistered(Name, _Attempts = 50).

wait_until_unregistered(Name, Attempts) when Attempts > 0 ->
  case whereis(Name) of
    undefined ->
      ok;
    _Pid ->
      timer:sleep(20),
      wait_until_unregistered(Name, Attempts - 1)
  end;
wait_until_unregistered(Name, 0) ->
  ct:fail({name_still_registered, Name}).
