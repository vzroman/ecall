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
-define(PAYLOAD_KEY, {?MODULE, payload}).

-record(point, {
  run_ref,
  path,
  receiver_node,
  payload_profile,
  writer_count,
  messages_per_writer,
  pace_ms,
  expected,
  metadata = #{}
}).

-record(writer, {
  run_ref,
  path,
  receiver_node,
  receiver,
  messages_per_writer,
  pace_ms
}).

-record(receiver, {
  run_ref,
  controller,
  writer,
  expected,
  count = 0,
  started_at
}).

-record(state, {
  point,
  receiver_done = 0,
  receiver_count = 0,
  receiver_elapsed_us = 0
}).


%%====================================================================
%% Common Test API
%%====================================================================

all() ->
  [native_test, ecall_test].
  %[ecall_test].

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
  Performance = ?config(performance, Config),
  [
    run_writer_counts(Path, PayloadProfile, Config)
    || PayloadProfile <- maps:get(payloads, Performance)
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
  ok = performance_metrics:point(Config, Result).

point_metadata(native) ->
  #{};
point_metadata(ecall) ->
  #{}.


%%====================================================================
%% Point runner
%%====================================================================

run_send_point(Config) ->
  Point = send_point(Config),
  persistent_term:put(
    ?PAYLOAD_KEY,
    performance_payloads:new(Point#point.payload_profile)),
  try
    Metrics = performance_metrics:start(),
    try
      ok = performance_metrics:begin_point(Metrics),
      State0 = start_writers(Point),
      State1 = await_completion(State0),
      MetricResults = performance_metrics:finish(Metrics),
      result_map(Point, State1, MetricResults)
    after
      performance_metrics:abort(Metrics)
    end
  after
    persistent_term:erase(?PAYLOAD_KEY)
  end.

send_point(Config) ->
  WriterCount = maps:get(writer_count, Config),
  MessagesPerWriter = maps:get(messages_per_writer, Config),
  #point{
    run_ref = make_ref(),
    path = maps:get(path, Config),
    receiver_node = maps:get(receiver_node, Config),
    payload_profile = maps:get(payload_profile, Config),
    writer_count = WriterCount,
    messages_per_writer = MessagesPerWriter,
    pace_ms = maps:get(pace_ms, Config),
    expected = WriterCount * MessagesPerWriter,
    metadata = maps:get(metadata, Config, #{})
  }.

start_writers(#point{writer_count = WriterCount} = Point) ->
  Coordinator = self(),
  Writer = #writer{
    run_ref = Point#point.run_ref,
    path = Point#point.path,
    receiver_node = Point#point.receiver_node,
    messages_per_writer = Point#point.messages_per_writer,
    pace_ms = Point#point.pace_ms
  },
  ok = start_writers(WriterCount, Writer, Coordinator),
  #state{point = Point}.

start_writers(0, _Writer, _Coordinator) ->
  ok;
start_writers(Count, Writer, Coordinator) ->
  _ = spawn_link(fun() -> writer_loop(Writer, Coordinator) end),
  start_writers(Count - 1, Writer, Coordinator).

await_completion(#state{
    point = #point{writer_count = WriterCount, expected = Expected},
    receiver_done = WriterCount,
    receiver_count = Expected} = State) ->
  State;
await_completion(State) ->
  receive
    Message ->
      await_completion(handle_completion_message(Message, State))
  end.

handle_completion_message(
    {?TAG, RunRef, receiver_completed, Receiver, Count, ElapsedUs},
    State)
    when RunRef =:= (State#state.point)#point.run_ref,
         is_pid(Receiver),
         is_integer(ElapsedUs),
         ElapsedUs >= 0 ->
  Count = (State#state.point)#point.messages_per_writer,
  State#state{
    receiver_done = State#state.receiver_done + 1,
    receiver_count = State#state.receiver_count + Count,
    receiver_elapsed_us = State#state.receiver_elapsed_us + ElapsedUs
  };
handle_completion_message(Message, _State) ->
  exit({unexpected_completion_message, Message}).


%%====================================================================
%% Writers and receivers
%%====================================================================

writer_loop(Writer, Coordinator) ->
  Receiver = start_receiver(Writer, Coordinator),
  ok = writer_operations(1, Writer#writer{receiver = Receiver}),
  await_receiver(Writer#writer.run_ref, Receiver).

start_receiver(#writer{
    run_ref = RunRef,
    receiver_node = ReceiverNode,
    messages_per_writer = Expected}, Coordinator) ->
  Receiver = #receiver{
    run_ref = RunRef,
    controller = Coordinator,
    writer = self(),
    expected = Expected
  },
  spawn_link(
    ReceiverNode,
    fun() -> receiver_loop(Receiver) end).

await_receiver(RunRef, Receiver) ->
  receive
    {?TAG, RunRef, receiver_finished, Receiver} ->
      ok;
    Message ->
      exit({unexpected_writer_message, Message})
  end.

writer_operations(Seq, #writer{messages_per_writer = Max})
    when Seq > Max ->
  ok;
writer_operations(Seq, Writer)
    when Seq =:= Writer#writer.messages_per_writer ->
  ok = send_operation(Writer),
  writer_operations(Seq + 1, Writer);
writer_operations(
    Seq,
    #writer{pace_ms = PaceMs} = Writer) ->
  ok = send_operation(Writer),
  ok = timer:sleep(PaceMs),
  writer_operations(Seq + 1, Writer).

send_operation(#writer{path = native, receiver = Receiver}) ->
  Payload = persistent_term:get(?PAYLOAD_KEY),
  Receiver ! Payload,
  ok;
send_operation(#writer{path = ecall, receiver = Receiver}) ->
  Payload = persistent_term:get(?PAYLOAD_KEY),
  _ = ecall:send(Receiver, Payload),
  ok.

receiver_loop(#receiver{
    run_ref = RunRef,
    controller = Controller,
    writer = Writer,
    expected = Expected,
    count = Count,
    started_at = StartedAt} = Receiver) ->
  receive
    _Payload ->
      ReceivedAt = erlang:monotonic_time(microsecond),
      StartedAt1 = receiver_started_at(Count, StartedAt, ReceivedAt),
      Count1 = Count + 1,
      case Count1 of
        Expected ->
          ElapsedUs = ReceivedAt - StartedAt1,
          Controller !
            {?TAG, RunRef, receiver_completed, self(), Count1, ElapsedUs},
          Writer ! {?TAG, RunRef, receiver_finished, self()},
          ok;
        _ when Count1 < Expected ->
          receiver_loop(
            Receiver#receiver{count = Count1, started_at = StartedAt1});
        _ ->
          exit({too_many_messages, Count1, Expected})
      end
  end.

receiver_started_at(0, undefined, ReceivedAt) ->
  ReceivedAt;
receiver_started_at(_Count, StartedAt, _ReceivedAt) ->
  StartedAt.


%%====================================================================
%% Reporting
%%====================================================================

result_map(Point, State, Metrics) ->
  ElapsedMs = average_elapsed_ms(State),
  Base = #{
    suite => ?MODULE,
    operation => send,
    path => Point#point.path,
    receiver_node => Point#point.receiver_node,
    payload => Point#point.payload_profile,
    writer_count => Point#point.writer_count,
    messages_per_writer => Point#point.messages_per_writer,
    pace_ms => Point#point.pace_ms,
    elapsed_ms => ElapsedMs,
    performance_percent =>
      performance_percent(
        State#state.receiver_count,
        Point#point.writer_count,
        Point#point.pace_ms,
        ElapsedMs),
    metrics => Metrics
  },
  maps:merge(Base, Point#point.metadata).

average_elapsed_ms(#state{
    point = #point{writer_count = WriterCount},
    receiver_elapsed_us = ElapsedUs}) ->
  ElapsedUs / (WriterCount * 1000).

performance_percent(_Completed, _WriterCount, _PaceMs, ElapsedMs)
    when ElapsedMs == 0 ->
  0.0;
performance_percent(Completed, WriterCount, PaceMs, ElapsedMs) ->
  OperationsPerSecond =
    operations_per_second(Completed, WriterCount, ElapsedMs),
  ExpectedOperationsPerSecond = 1000 / PaceMs,
  (OperationsPerSecond * 100) / ExpectedOperationsPerSecond.

operations_per_second(Completed, WriterCount, ElapsedMs) ->
  (Completed * 1000) / (WriterCount * ElapsedMs).


%%====================================================================
%% Suite setup helpers
%%====================================================================

performance_settings() ->
  maps:merge(default_performance_settings(), ct:get_config(performance, #{})).

default_performance_settings() ->
  #{
    pace_ms => 100,
    messages_per_writer => 1000,
    writer_counts => [1000, 10000, 100000, 500000, 1000000],
    payloads => [tiny, data, binary_10kib, binary_100kib, binary_1mib]
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
  Pid =
    spawn_link(
      SenderNode,
      fun() -> Controller ! {?TAG, Ref, self(), Fun()} end),
  receive
    {?TAG, Ref, Pid, Result} ->
      Result;
    {'EXIT', Pid, Reason} ->
      exit(Reason)
  end.
