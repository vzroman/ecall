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
  path,
  receiver,
  messages_per_writer,
  pace_ms
}).

-record(receiver, {
  controller,
  expected,
  count = 0,
  started_at
}).

-record(receiver_controller, {
  run_ref,
  controller,
  expected,
  pending,
  completed = 0,
  message_count = 0,
  elapsed_us = 0
}).

-record(receiver_results, {
  message_count,
  average_elapsed_us
}).


%%====================================================================
%% Common Test API
%%====================================================================

all() ->
  %[native_test, ecall_test].
  [ecall_test].

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
    ReceiverController = start_receiver_controller(Point),
    Receivers = await_receivers_started(Point, ReceiverController),
    Metrics = performance_metrics:start(),
    try
      ok = performance_metrics:begin_point(Metrics),
      WriterController = start_writer_controller(Point, Receivers),
      ReceiverResults =
        await_receiver_results(Point, ReceiverController),
      ok = finish_writer_controller(Point, WriterController),
      MetricResults = performance_metrics:finish(Metrics),
      result_map(Point, ReceiverResults, MetricResults)
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

start_receiver_controller(#point{
    run_ref = RunRef,
    receiver_node = ReceiverNode,
    writer_count = ReceiverCount,
    messages_per_writer = MessagesPerReceiver}) ->
  Controller = self(),
  spawn_link(
    fun() ->
      receiver_controller(
        Controller,
        RunRef,
        ReceiverNode,
        ReceiverCount,
        MessagesPerReceiver)
    end).

await_receivers_started(
    #point{run_ref = RunRef, writer_count = WriterCount},
    ReceiverController) ->
  receive
    {?TAG, RunRef, ReceiverController, receivers_started, Receivers}
        when length(Receivers) =:= WriterCount ->
      Receivers;
    Message ->
      exit({unexpected_point_message, Message})
  end.

start_writer_controller(Point, Receivers) ->
  Controller = self(),
  Writer = #writer{
    path = Point#point.path,
    messages_per_writer = Point#point.messages_per_writer,
    pace_ms = Point#point.pace_ms
  },
  spawn_link(
    fun() ->
      writer_controller(
        Controller,
        Point#point.run_ref,
        Writer,
        Receivers)
    end).

await_receiver_results(
    #point{run_ref = RunRef, expected = Expected},
    ReceiverController) ->
  receive
    {?TAG, RunRef, ReceiverController, receiver_results,
        #receiver_results{message_count = Expected} = Results} ->
      Results;
    Message ->
      exit({unexpected_point_message, Message})
  end.

finish_writer_controller(
    #point{run_ref = RunRef}, WriterController) ->
  WriterController ! {?TAG, RunRef, self(), finish},
  receive
    {?TAG, RunRef, WriterController, finished} ->
      ok;
    Message ->
      exit({unexpected_point_message, Message})
  end.


%%====================================================================
%% Receiver controller and receivers
%%====================================================================

receiver_controller(
    Controller,
    RunRef,
    ReceiverNode,
    ReceiverCount,
    MessagesPerReceiver) ->
  Receivers =
    start_receivers(
      ReceiverCount,
      ReceiverNode,
      MessagesPerReceiver,
      []),
  Controller !
    {?TAG, RunRef, self(), receivers_started, Receivers},
  Pending = maps:from_list([{Receiver, true} || Receiver <- Receivers]),
  receiver_controller_loop(
    #receiver_controller{
      run_ref = RunRef,
      controller = Controller,
      expected = MessagesPerReceiver,
      pending = Pending
    }).

start_receivers(0, _ReceiverNode, _Expected, Receivers) ->
  Receivers;
start_receivers(Count, ReceiverNode, Expected, Receivers) ->
  Receiver = start_receiver(ReceiverNode, Expected),
  start_receivers(
    Count - 1,
    ReceiverNode,
    Expected,
    [Receiver | Receivers]).

start_receiver(ReceiverNode, Expected) ->
  Receiver = #receiver{
    controller = self(),
    expected = Expected
  },
  spawn_link(
    ReceiverNode,
    fun() -> receiver_loop(Receiver) end).

receiver_controller_loop(State) ->
  receive
    {?TAG, completed, Receiver, Count, ElapsedUs}
        when Count =:= State#receiver_controller.expected,
             is_integer(ElapsedUs),
             ElapsedUs >= 0 ->
      receiver_completed(Receiver, Count, ElapsedUs, State);
    Message ->
      exit({unexpected_receiver_controller_message, Message})
  end.

receiver_completed(Receiver, Count, ElapsedUs, State) ->
  {true, Pending} =
    maps:take(Receiver, State#receiver_controller.pending),
  State1 = State#receiver_controller{
    pending = Pending,
    completed = State#receiver_controller.completed + 1,
    message_count = State#receiver_controller.message_count + Count,
    elapsed_us = State#receiver_controller.elapsed_us + ElapsedUs
  },
  receiver_controller_continue(State1).

receiver_controller_continue(
    #receiver_controller{pending = Pending} = State)
    when map_size(Pending) > 0 ->
  receiver_controller_loop(State);
receiver_controller_continue(State) ->
  Results = #receiver_results{
    message_count = State#receiver_controller.message_count,
    average_elapsed_us =
      State#receiver_controller.elapsed_us /
        State#receiver_controller.completed
  },
  State#receiver_controller.controller !
    {?TAG,
     State#receiver_controller.run_ref,
     self(),
     receiver_results,
     Results},
  ok.

receiver_loop(#receiver{
    controller = Controller,
    expected = Expected,
    count = Count,
    started_at = StartedAt} = Receiver) ->
  receive
    _Payload ->
      ReceivedAt = erlang:monotonic_time(microsecond),
      StartedAt1 = receiver_started_at(Count, StartedAt, ReceivedAt),
      Count1 = Count + 1,
      receiver_continue(
        Count1,
        Expected,
        ReceivedAt - StartedAt1,
        Receiver#receiver{count = Count1, started_at = StartedAt1},
        Controller)
  end.

receiver_continue(Count, Count, ElapsedUs, _Receiver, Controller) ->
  Controller ! {?TAG, completed, self(), Count, ElapsedUs},
  ok;
receiver_continue(Count, Expected, _ElapsedUs, Receiver, _Controller)
    when Count < Expected ->
  receiver_loop(Receiver);
receiver_continue(Count, Expected, _ElapsedUs, _Receiver, _Controller) ->
  exit({too_many_messages, Count, Expected}).

receiver_started_at(0, undefined, ReceivedAt) ->
  ReceivedAt;
receiver_started_at(_Count, StartedAt, _ReceivedAt) ->
  StartedAt.


%%====================================================================
%% Writer controller and writers
%%====================================================================

writer_controller(Controller, RunRef, Writer, Receivers) ->
  ok = start_writers(Receivers, Writer),
  await_finish(Controller, RunRef).

start_writers([], _Writer) ->
  ok;
start_writers([Receiver | Receivers], Writer) ->
  _ =
    spawn_link(
      fun() -> writer_loop(Writer#writer{receiver = Receiver}) end),
  start_writers(Receivers, Writer).

await_finish(Controller, RunRef) ->
  receive
    {?TAG, RunRef, Controller, finish} ->
      Controller ! {?TAG, RunRef, self(), finished},
      ok;
    Message ->
      exit({unexpected_writer_controller_message, Message})
  end.

writer_loop(Writer) ->
  writer_operations(1, Writer).

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

%%====================================================================
%% Reporting
%%====================================================================

result_map(Point, ReceiverResults, Metrics) ->
  ElapsedMs = average_elapsed_ms(ReceiverResults),
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
        ReceiverResults#receiver_results.message_count,
        Point#point.writer_count,
        Point#point.pace_ms,
        ElapsedMs),
    metrics => Metrics
  },
  maps:merge(Base, Point#point.metadata).

average_elapsed_ms(
    #receiver_results{average_elapsed_us = AverageElapsedUs}) ->
  AverageElapsedUs / 1000.

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
