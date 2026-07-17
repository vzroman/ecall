-module(performance_orchestration_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

-export([
  sender_receiver_spawn_monitor/1
]).

-export([
  sender_role/2,
  receiver_role/2
]).

-define(MESSAGE_COUNT, 10).
-define(TIMEOUT, 10000).

all() ->
  [
    sender_receiver_spawn_monitor
  ].

init_per_suite(Config0) ->
  RoleConfig = distributed_tests_util:role_config(Config0),
  SenderNode = distributed_tests_util:start_node(sender, RoleConfig),
  ReceiverNode = distributed_tests_util:start_node(receiver, RoleConfig),
  ok = distributed_tests_util:connect(SenderNode, ReceiverNode),
  [
    {role_config, distributed_tests_util:public_role_config(RoleConfig)},
    {sender, SenderNode},
    {receiver, ReceiverNode}
    | Config0
  ].

end_per_suite(_Config) ->
  distributed_tests_util:stop_all().

sender_receiver_spawn_monitor(Config) ->
  Parent = self(),
  SenderNode = ?config(sender, Config),
  ReceiverNode = ?config(receiver, Config),

  {ReceiverPid, ReceiverMon} =
    spawn_monitor(ReceiverNode, ?MODULE, receiver_role, [Parent, Config]),
  {SenderPid, SenderMon} =
    spawn_monitor(SenderNode, ?MODULE, sender_role, [Parent, Config]),

  wait_ready(ReceiverPid, ReceiverMon),
  wait_ready(SenderPid, SenderMon),

  SenderPid ! {start, ReceiverPid, ?MESSAGE_COUNT},

  wait_result(SenderPid, SenderMon, {sent, ?MESSAGE_COUNT}),
  wait_result(ReceiverPid, ReceiverMon, {received, ?MESSAGE_COUNT}),
  wait_down(SenderPid, SenderMon),
  wait_down(ReceiverPid, ReceiverMon).

sender_role(Parent, _Config) ->
  Parent ! {self(), ready},
  receive
    {start, ReceiverPid, Count} ->
      [ReceiverPid ! {performance_payload, self(), I}
       || I <- lists:seq(1, Count)],
      ReceiverPid ! {performance_done, self(), Count},
      Parent ! {self(), {sent, Count}}
  after ?TIMEOUT ->
      exit(sender_timeout)
  end.

receiver_role(Parent, _Config) ->
  Parent ! {self(), ready},
  receiver_loop(Parent, 0).

receiver_loop(Parent, Count) ->
  receive
    {performance_payload, _SenderPid, _Payload} ->
      receiver_loop(Parent, Count + 1);
    {performance_done, _SenderPid, Count} ->
      Parent ! {self(), {received, Count}};
    {performance_done, _SenderPid, Expected} ->
      exit({unexpected_received_count, Expected, Count})
  after ?TIMEOUT ->
      exit({receiver_timeout, Count})
  end.

wait_ready(Pid, Mon) ->
  receive
    {Pid, ready} ->
      ok;
    {'DOWN', Mon, process, Pid, Reason} ->
      ct:fail({role_exited_before_ready, Pid, Reason})
  after ?TIMEOUT ->
      ct:fail({ready_timeout, Pid})
  end.

wait_result(Pid, Mon, Expected) ->
  receive
    {Pid, Expected} ->
      ok;
    {'DOWN', Mon, process, Pid, Reason} ->
      ct:fail({role_exited_before_result, Pid, Expected, Reason});
    {Pid, Other} ->
      ct:fail({unexpected_role_result, Pid, Expected, Other})
  after ?TIMEOUT ->
      ct:fail({result_timeout, Pid, Expected})
  end.

wait_down(Pid, Mon) ->
  receive
    {'DOWN', Mon, process, Pid, normal} ->
      ok;
    {'DOWN', Mon, process, Pid, Reason} ->
      ct:fail({role_exited, Pid, Reason})
  after ?TIMEOUT ->
      ct:fail({down_timeout, Pid})
  end.
