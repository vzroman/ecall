-module(performance_call_SUITE).

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
  messages_per_writer,
  pace_ms
}).

-record(state, {
  point,
  writer_pids = [],
  ready = 0,
  writer_completed = 0,
  writer_down = 0,
  completed = 0,
  metrics
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
  run_call_test_point(native, PayloadProfile, WriterCount, Config).

ecall_test_point(PayloadProfile, WriterCount, Config) ->
  run_call_test_point(ecall, PayloadProfile, WriterCount, Config).

run_call_test_point(Path, PayloadProfile, WriterCount, Config) ->
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
      fun() -> run_call_point(PointConfig) end),
  ok = performance_metrics:point(Config, Result).

point_metadata(native) ->
  #{};
point_metadata(ecall) ->
  #{}.


%%====================================================================
%% Point runner
%%====================================================================

run_call_point(Config) ->
  Point = call_point(Config),
  persistent_term:put(
    ?PAYLOAD_KEY,
    performance_payloads:new(Point#point.payload_profile)),
  try
    State0 = start_participants(Point),
    try
      State1 = await_ready(State0),
      Metrics = performance_metrics:start(),
      try
        ok = performance_metrics:begin_point(Metrics),
        StartedAt = erlang:monotonic_time(millisecond),
        release_writers(State1),
        State2 = await_completion(State1#state{metrics = Metrics}),
        ElapsedMs = erlang:monotonic_time(millisecond) - StartedAt,
        MetricResults = performance_metrics:finish(Metrics),
        result_map(Point, State2, ElapsedMs, MetricResults)
      after
        performance_metrics:abort(Metrics)
      end
    catch
      Class:Reason:Stack ->
        cleanup_point(State0),
        erlang:raise(Class, Reason, Stack)
    end
  after
    persistent_term:erase(?PAYLOAD_KEY)
  end.

call_point(Config) ->
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

start_participants(Point) ->
  Writer = #writer{
    run_ref = Point#point.run_ref,
    path = Point#point.path,
    receiver_node = Point#point.receiver_node,
    messages_per_writer = Point#point.messages_per_writer,
    pace_ms = Point#point.pace_ms
  },
  WriterPids = start_writers(Point#point.writer_count, Writer),
  #state{point = Point, writer_pids = WriterPids}.

start_writers(Count, Writer) ->
  start_writers(Count, Writer, self(), []).

start_writers(0, _Writer, _Coordinator, Acc) ->
  Acc;
start_writers(Count, Writer, Coordinator, Acc) when Count > 0 ->
  {Pid, _Mon} = spawn_monitor(fun() -> writer_loop(Writer, Coordinator) end),
  start_writers(Count - 1, Writer, Coordinator, [Pid | Acc]).

await_ready(#state{
    point = #point{writer_count = WriterCount},
    ready = WriterCount} = State) ->
  State;
await_ready(State) ->
  receive
    Message ->
      await_ready(handle_ready_message(Message, State))
  end.

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
    completed = Expected} = State) ->
  State;
await_completion(State) ->
  receive
    Message ->
      await_completion(handle_completion_message(Message, State))
  end.

handle_completion_message({?TAG, RunRef, writer_completed, _Pid, Count}, State)
    when RunRef =:= (State#state.point)#point.run_ref ->
  Count = (State#state.point)#point.messages_per_writer,
  State#state{
    writer_completed = State#state.writer_completed + 1,
    completed = State#state.completed + Count
  };
handle_completion_message({'DOWN', _Mon, process, _Pid, _Reason} = Message, State) ->
  case performance_metrics:handle_down(Message, State#state.metrics) of
    not_collector ->
      handle_writer_down(Message, State)
  end;
handle_completion_message(Message, _State) ->
  exit({unexpected_completion_message, Message}).

handle_writer_down({'DOWN', _Mon, process, _Pid, normal}, State) ->
  State#state{writer_down = State#state.writer_down + 1};
handle_writer_down({'DOWN', _Mon, process, Pid, Reason}, _State) ->
  exit({process_failed, Pid, Reason}).


%%====================================================================
%% Writers
%%====================================================================

writer_loop(Writer, Coordinator) ->
  RunRef = Writer#writer.run_ref,
  Coordinator ! {?TAG, RunRef, writer_ready, self()},
  receive
    {?TAG, RunRef, start} ->
      Completed = writer_operations(1, 0, Writer),
      Coordinator ! {?TAG, RunRef, writer_completed, self(), Completed};
    Message ->
      exit({unexpected_writer_message, Message})
  end.

writer_operations(Seq, Completed, #writer{messages_per_writer = Max})
    when Seq > Max ->
  Completed;
writer_operations(Seq, Completed, Writer)
    when Seq =:= Writer#writer.messages_per_writer ->
  ok = call_operation(Writer),
  writer_operations(Seq + 1, Completed + 1, Writer);
writer_operations(
    Seq,
    Completed,
    #writer{pace_ms = PaceMs} = Writer) ->
  ok = call_operation(Writer),
  ok = timer:sleep(PaceMs),
  writer_operations(Seq + 1, Completed + 1, Writer).

call_operation(#writer{
    path = native,
    receiver_node = Node,
    run_ref = RunRef}) ->
  Payload = persistent_term:get(?PAYLOAD_KEY),
  {RunRef, ok} =
    erpc:call(Node, erlang, element, [1, {{RunRef, ok}, Payload}]),
  ok;
call_operation(#writer{
    path = ecall,
    receiver_node = Node,
    run_ref = RunRef}) ->
  Payload = persistent_term:get(?PAYLOAD_KEY),
  {ok, {RunRef, ok}} =
    ecall:call(Node, erlang, element, [1, {{RunRef, ok}, Payload}]),
  ok.


%%====================================================================
%% Cleanup and reporting
%%====================================================================

cleanup_point(#state{writer_pids = WriterPids}) ->
  [exit(Pid, kill) || Pid <- WriterPids],
  ok.

result_map(Point, State, ElapsedMs, Metrics) ->
  Base = #{
    suite => ?MODULE,
    operation => call,
    path => Point#point.path,
    receiver_node => Point#point.receiver_node,
    payload => Point#point.payload_profile,
    writer_count => Point#point.writer_count,
    messages_per_writer => Point#point.messages_per_writer,
    pace_ms => Point#point.pace_ms,
    elapsed_ms => ElapsedMs,
    performance_percent =>
      performance_percent(
        State#state.completed,
        Point#point.writer_count,
        Point#point.pace_ms,
        ElapsedMs),
    metrics => Metrics
  },
  maps:merge(Base, Point#point.metadata).

performance_percent(_Completed, _WriterCount, _PaceMs, 0) ->
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
