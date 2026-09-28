-module(ecall_app_SUITE).

%% pool_size is read and validated once, at application start.

-include_lib("common_test/include/ct.hrl").

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
  invalid_pool_size_fails_start/1,
  disabled_pool_starts_without_receive/1,
  default_pool_size_is_logical_processors/1
]).

%%=================================================================
%% COMMON TEST API
%%=================================================================
all() ->
  [
    invalid_pool_size_fails_start,
    disabled_pool_starts_without_receive,
    default_pool_size_is_logical_processors
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_, Config) ->
  reset_application(),
  Config.

end_per_testcase(_, _Config) ->
  reset_application(),
  ok.

%%=================================================================
%% TEST CASES
%%=================================================================
invalid_pool_size_fails_start(_Config) ->
  [ begin
      ok = application:set_env(ecall, pool_size, Value),
      {error, {{invalid_pool_size, Value}, {ecall_app, start, [normal, []]}}} =
        application:start(ecall),
      undefined = whereis(ecall_receive)
    end || Value <- [0, -1, foo] ],
  ok.

disabled_pool_starts_without_receive(_Config) ->
  ok = application:set_env(ecall, pool_size, disabled),
  ok = application:start(ecall),
  undefined = whereis(ecall_receive),
  [] = pg:get_members(ecall, nodes),

  probe = ecall:send(self(), probe),
  receive
    probe ->
      ok
  after
    1000 ->
      ct:fail(native_send_not_delivered)
  end,
  {error, not_connected} = ecall:connection_info('nobody@nowhere').

default_pool_size_is_logical_processors(_Config) ->
  ok = application:start(ecall),
  Master = wait_until_registered(ecall_receive),
  {ok, {Master, Workers}} = ecall_receive:get_pool(),
  Expected =
    case erlang:system_info(logical_processors) of
      unknown ->
        erlang:system_info(schedulers);
      LogicalProcessors ->
        LogicalProcessors
    end,
  Expected = length(Workers).

%%=================================================================
%% UTILITIES
%%=================================================================
reset_application() ->
  case application:stop(ecall) of
    ok ->
      ok;
    {error, {not_started, ecall}} ->
      ok
  end,
  application:unset_env(ecall, pool_size),
  ok.

wait_until_registered(Name) ->
  wait_until_registered(Name, _Attempts = 50).

wait_until_registered(Name, Attempts) when Attempts > 0 ->
  case whereis(Name) of
    Pid when is_pid(Pid) ->
      Pid;
    undefined ->
      timer:sleep(20),
      wait_until_registered(Name, Attempts - 1)
  end;
wait_until_registered(Name, 0) ->
  ct:fail({not_registered, Name}).
