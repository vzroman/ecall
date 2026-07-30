-module(performance_send_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test API
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test cases
-export([
  native_test/1,
  ecall_test/1
]).

-define(TAG, ?MODULE).
-define(RPC_TIMEOUT, 30000).

-record(point, {
  run_ref,
  path,
  receiver_node,
  target,
  payload_profile,
  payload,
  writer_count,
  messages_per_writer,
  pace_ms,
  expected,
  metadata = #{}
}).

-record(state, {
  point,
  target_pids = [],
  target_set = #{},
  target_monitors = #{},
  target_ready = 0,
  target_done = 0,
  target_count = 0,
  writer_pids = [],
  ready = 0,
  writer_completed = 0,
  writer_down = 0,
  completed = 0
}).


%%====================================================================
%% Common Test API
%%====================================================================

all() ->
  [native_test, ecall_test].

init_per_suite(Config) ->
  Performance = performance_settings(),
  RoleConfig = role_config(),
  [SenderNode, ReceiverNode] =
    distributed_tests_utils:start_nodes(node_configs(RoleConfig)),
  [
    {performance_nodes, [SenderNode, ReceiverNode]},
    {sender_node, SenderNode},
    {receiver_node, ReceiverNode},
    {performance, Performance}
    | Config
  ].

end_per_suite(Config) ->
  distributed_tests_utils:stop_nodes(?config(performance_nodes, Config)).


%%====================================================================
%% Test cases
%%====================================================================

native_test(Config) ->
  _ = ct:timetrap(infinity),
  run_payloads(native, Config).

ecall_test(Config) ->
  _ = ct:timetrap(infinity),
  run_payloads(ecall, Config).


%%====================================================================
%% Test matrix
%%====================================================================

run_payloads(Path, Config) ->
  [
    run_writer_counts(Path, PayloadProfile, Config)
    || PayloadProfile <- payload_profiles()
  ],
  ok.

run_writer_counts(Path, PayloadProfile, Config) ->
  Performance = ?config(performance, Config),
  [
    test_point(Path, PayloadProfile, WriterCount, Config)
    || WriterCount <- maps:get(writer_counts, Performance)
  ],
  ok.

test_point(native, PayloadProfile, WriterCount, Config) ->
  native_test_point(PayloadProfile, WriterCount, Config);
test_point(ecall, PayloadProfile, WriterCount, Config) ->
  ecall_test_point(PayloadProfile, WriterCount, Config).

native_test_point(PayloadProfile, WriterCount, Config) ->
  run_send_test_point(native, PayloadProfile, WriterCount, Config).

ecall_test_point(PayloadProfile, WriterCount, Config) ->
  run_send_test_point(ecall, PayloadProfile, WriterCount, Config).

run_send_test_point(Path, PayloadProfile, WriterCount, Config) ->
  Performance = ?config(performance, Config),
  PointConfig = #{
    path => Path,
    receiver_node => ?config(receiver_node, Config),
    payload_profile => PayloadProfile,
    writer_count => WriterCount,
    messages_per_writer => maps:get(messages_per_writer, Performance),
    pace_ms => maps:get(pace_ms, Performance),
    metadata => point_metadata(Path)
  },
  Result =
    run_on_sender(
      ?config(sender_node, Config),
      fun() -> run_send_point(PointConfig) end),
  ct:pal("Send performance point completed: ~p", [Result]),
  ok.

point_metadata(native) ->
  #{};
point_metadata(ecall) ->
  #{}.


%%====================================================================
%% Point runner
%%====================================================================

run_send_point(Config) ->
  Point = send_point(Config),
  State0 = start_participants(Point),
  try
    State1 = await_ready(State0),
    StartedAt = erlang:monotonic_time(millisecond),
    release_writers(State1),
    State2 = await_completion(State1),
    ElapsedMs = erlang:monotonic_time(millisecond) - StartedAt,
    ok = stop_targets(State2),
    result_map(Point, State2, ElapsedMs)
  catch
    Class:Reason:Stack ->
      cleanup_point(State0),
      erlang:raise(Class, Reason, Stack)
  end.

send_point(Config) ->
  WriterCount = maps:get(writer_count, Config),
  MessagesPerWriter = maps:get(messages_per_writer, Config),
  #point{
    run_ref = make_ref(),
    path = maps:get(path, Config),
    receiver_node = maps:get(receiver_node, Config),
    payload_profile = maps:get(payload_profile, Config),
    payload = performance_payloads:new(maps:get(payload_profile, Config)),
    writer_count = WriterCount,
    messages_per_writer = MessagesPerWriter,
    pace_ms = maps:get(pace_ms, Config),
    expected = WriterCount * MessagesPerWriter,
    metadata = maps:get(metadata, Config, #{})
  }.

start_participants(Point) ->
  Targets = start_send_targets(Point),
  TargetMonitors = monitor_targets(Targets),
  WriterPids = start_writers(Targets, Point),
  #state{
    point = Point,
    target_pids = Targets,
    target_set = target_set(Targets),
    target_monitors = TargetMonitors,
    writer_pids = WriterPids
  }.

start_send_targets(#point{
    run_ref = RunRef,
    receiver_node = Node,
    writer_count = WriterCount,
    messages_per_writer = MessagesPerWriter}) ->
  Coordinator = self(),
  erpc:call(
    Node,
    fun() ->
      [
        spawn(fun() ->
          Coordinator ! {?TAG, RunRef, target_ready, self()},
          send_target_loop(RunRef, Coordinator, MessagesPerWriter, 0)
        end)
        || _ <- lists:seq(1, WriterCount)
      ]
    end,
    ?RPC_TIMEOUT).

monitor_targets(Targets) ->
  maps:from_list([{erlang:monitor(process, Target), Target}
                  || Target <- Targets]).

target_set(Targets) ->
  maps:from_list([{Target, true} || Target <- Targets]).

start_writers(Targets, Point) ->
  start_writers(Targets, Point, self(), []).

start_writers([], _Point, _Coordinator, Acc) ->
  Acc;
start_writers([Target | Targets], Point, Coordinator, Acc) ->
  WriterPoint = Point#point{target = Target},
  {Pid, _Mon} =
    spawn_monitor(fun() -> writer_loop(WriterPoint, Coordinator) end),
  start_writers(Targets, Point, Coordinator, [Pid | Acc]).

await_ready(#state{
    point = #point{writer_count = WriterCount},
    ready = WriterCount,
    target_ready = WriterCount} = State) ->
  State;
await_ready(State) ->
  receive
    Message ->
      await_ready(handle_ready_message(Message, State))
  end.

handle_ready_message({?TAG, RunRef, target_ready, Target}, State)
    when RunRef =:= (State#state.point)#point.run_ref,
         is_pid(Target) ->
  true = maps:is_key(Target, State#state.target_set),
  State#state{target_ready = State#state.target_ready + 1};
handle_ready_message({?TAG, RunRef, writer_ready, _Pid}, State)
    when RunRef =:= (State#state.point)#point.run_ref ->
  State#state{ready = State#state.ready + 1};
handle_ready_message({'DOWN', _Mon, process, Pid, Reason}, _State) ->
  exit({process_down_before_ready, Pid, Reason});
handle_ready_message(Message, _State) ->
  exit({unexpected_ready_message, Message}).

release_writers(#state{
    point = #point{run_ref = RunRef},
    writer_pids = WriterPids}) ->
  [Pid ! {?TAG, RunRef, start} || Pid <- WriterPids],
  ok.

await_completion(#state{
    point = #point{writer_count = WriterCount, expected = Expected},
    writer_completed = WriterCount,
    writer_down = WriterCount,
    target_done = WriterCount,
    target_count = Expected,
    completed = Expected} = State) ->
  State;
await_completion(State) ->
  receive
    Message ->
      await_completion(handle_completion_message(Message, State))
  end.

handle_completion_message({?TAG, RunRef, target_completed, Target, Count}, State)
    when RunRef =:= (State#state.point)#point.run_ref,
         is_pid(Target) ->
  true = maps:is_key(Target, State#state.target_set),
  Count = (State#state.point)#point.messages_per_writer,
  State#state{
    target_done = State#state.target_done + 1,
    target_count = State#state.target_count + Count
  };
handle_completion_message({?TAG, RunRef, writer_completed, _Pid, Count}, State)
    when RunRef =:= (State#state.point)#point.run_ref ->
  Count = (State#state.point)#point.messages_per_writer,
  State#state{
    writer_completed = State#state.writer_completed + 1,
    completed = State#state.completed + Count
  };
handle_completion_message({'DOWN', Mon, process, Pid, Reason}, State) ->
  handle_down(Mon, Pid, Reason, State);
handle_completion_message(Message, _State) ->
  exit({unexpected_completion_message, Message}).

handle_down(Mon, Pid, Reason, #state{target_monitors = TargetMonitors} = State) ->
  case maps:is_key(Mon, TargetMonitors) of
    true ->
      exit({target_down_before_stop, Pid, Reason});
    false ->
      handle_writer_down(Pid, Reason, State)
  end.

handle_writer_down(_Pid, normal, State) ->
  State#state{writer_down = State#state.writer_down + 1};
handle_writer_down(Pid, Reason, _State) ->
  exit({process_failed, Pid, Reason}).


%%====================================================================
%% Writers and target
%%====================================================================

writer_loop(Point, Coordinator) ->
  RunRef = Point#point.run_ref,
  Coordinator ! {?TAG, RunRef, writer_ready, self()},
  receive
    {?TAG, RunRef, start} ->
      Completed = writer_operations(1, 0, Point),
      Coordinator ! {?TAG, RunRef, writer_completed, self(), Completed};
    Message ->
      exit({unexpected_writer_message, Message})
  end.

writer_operations(Seq, Completed, #point{messages_per_writer = Max})
    when Seq > Max ->
  Completed;
writer_operations(Seq, Completed, Point)
    when Seq =:= Point#point.messages_per_writer ->
  ok = send_operation(Point),
  writer_operations(Seq + 1, Completed + 1, Point);
writer_operations(Seq, Completed, #point{pace_ms = PaceMs, run_ref = RunRef} = Point) ->
  TimerRef = erlang:start_timer(PaceMs, self(), {?TAG, RunRef, pace, Seq}),
  ok = send_operation(Point),
  receive
    {timeout, TimerRef, {?TAG, RunRef, pace, Seq}} ->
      writer_operations(Seq + 1, Completed + 1, Point);
    Message ->
      exit({unexpected_pace_message, Message})
  end.

send_operation(#point{path = native, target = Target, payload = Payload}) ->
  Target ! Payload,
  ok;
send_operation(#point{path = ecall, target = Target, payload = Payload}) ->
  _ = ecall:send(Target, Payload),
  ok.

send_target_loop(RunRef, Coordinator, Expected, Count) ->
  receive
    {?TAG, RunRef, stop, From} ->
      From ! {?TAG, RunRef, target_stopped, self(), Count},
      ok;
    _Payload ->
      Count1 = Count + 1,
      case Count1 of
        Expected ->
          Coordinator ! {?TAG, RunRef, target_completed, self(), Count1},
          send_target_loop(RunRef, Coordinator, Expected, Count1);
        _ when Count1 < Expected ->
          send_target_loop(RunRef, Coordinator, Expected, Count1);
        _ ->
          exit({too_many_messages, Count1, Expected})
      end
  end.


%%====================================================================
%% Cleanup and reporting
%%====================================================================

stop_targets(#state{
    point = #point{run_ref = RunRef},
    target_pids = Targets,
    target_set = TargetSet,
    target_monitors = TargetMonitors}) ->
  [Target ! {?TAG, RunRef, stop, self()} || Target <- Targets],
  wait_targets_stopped(RunRef, TargetSet, TargetMonitors).

wait_targets_stopped(_RunRef, PendingAcks, PendingDowns)
    when map_size(PendingAcks) =:= 0,
         map_size(PendingDowns) =:= 0 ->
  ok;
wait_targets_stopped(RunRef, PendingAcks, PendingDowns) ->
  receive
    {?TAG, RunRef, target_stopped, Target, _Count} ->
      wait_targets_stopped(
        RunRef,
        maps:remove(Target, PendingAcks),
        PendingDowns);
    {'DOWN', Mon, process, Target, normal} ->
      {Target, PendingDowns1} = maps:take(Mon, PendingDowns),
      wait_targets_stopped(RunRef, PendingAcks, PendingDowns1);
    Message ->
      exit({unexpected_stop_message, Message})
  end.

cleanup_point(#state{target_pids = Targets, target_monitors = TargetMonitors,
                     writer_pids = WriterPids}) ->
  [exit(Pid, kill) || Pid <- WriterPids],
  [exit(Target, kill) || Target <- Targets],
  maps:foreach(
    fun(TargetMon, _Target) ->
      erlang:demonitor(TargetMon, [flush])
    end,
    TargetMonitors),
  ok.

result_map(Point, State, ElapsedMs) ->
  Base = #{
    suite => ?MODULE,
    operation => send,
    path => Point#point.path,
    receiver_node => Point#point.receiver_node,
    payload => Point#point.payload_profile,
    writer_count => Point#point.writer_count,
    messages_per_writer => Point#point.messages_per_writer,
    pace_ms => Point#point.pace_ms,
    expected => Point#point.expected,
    completed => State#state.completed,
    elapsed_ms => ElapsedMs,
    completed_per_second => completed_per_second(State#state.completed, ElapsedMs)
  },
  maps:merge(Base, Point#point.metadata).

completed_per_second(_Completed, 0) ->
  0.0;
completed_per_second(Completed, ElapsedMs) ->
  (Completed * 1000) / ElapsedMs.


%%====================================================================
%% Suite setup helpers
%%====================================================================

performance_settings() ->
  maps:merge(default_performance_settings(), ct:get_config(performance, #{})).

default_performance_settings() ->
  #{
    pace_ms => 100,
    messages_per_writer => 1000,
    writer_counts => [1000, 10000, 100000, 500000, 1000000]
  }.

role_config() ->
  ct:get_config(role_config, #{sender => local, receiver => local}).

node_configs(RoleConfig) ->
  [
    node_config(sender, maps:get(sender, RoleConfig, local)),
    node_config(receiver, maps:get(receiver, RoleConfig, local))
  ].

node_config(Name, Location) ->
  #{
    name => Name,
    location => Location
  }.

run_on_sender(SenderNode, Fun) ->
  Controller = self(),
  Ref = make_ref(),
  {Pid, Mon} =
    spawn_monitor(
      SenderNode,
      fun() ->
        Controller ! {?TAG, Ref, self(), Fun()}
      end),
  receive
    {?TAG, Ref, Pid, Result} ->
      erlang:demonitor(Mon, [flush]),
      Result;
    {'DOWN', Mon, process, Pid, Reason} ->
      exit({sender_point_failed, Reason})
  end.


%%====================================================================
%% Payloads
%%====================================================================

payload_profiles() ->
  performance_payloads:profiles().
