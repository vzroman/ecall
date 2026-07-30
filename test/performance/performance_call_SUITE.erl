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

-record(point, {
  run_ref,
  path,
  receiver_node,
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
  ct:pal("Call performance point completed: ~p", [Result]),
  ok.

point_metadata(native) ->
  #{};
point_metadata(ecall) ->
  #{}.


%%====================================================================
%% Point runner
%%====================================================================

run_call_point(Config) ->
  Point = call_point(Config),
  State0 = start_participants(Point),
  try
    State1 = await_ready(State0),
    StartedAt = erlang:monotonic_time(millisecond),
    release_writers(State1),
    State2 = await_completion(State1),
    ElapsedMs = erlang:monotonic_time(millisecond) - StartedAt,
    result_map(Point, State2, ElapsedMs)
  catch
    Class:Reason:Stack ->
      cleanup_point(State0),
      erlang:raise(Class, Reason, Stack)
  end.

call_point(Config) ->
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
  WriterPids = start_writers(Point#point.writer_count, Point),
  #state{point = Point, writer_pids = WriterPids}.

start_writers(Count, Point) ->
  start_writers(Count, Point, self(), []).

start_writers(0, _Point, _Coordinator, Acc) ->
  Acc;
start_writers(Count, Point, Coordinator, Acc) when Count > 0 ->
  {Pid, _Mon} = spawn_monitor(fun() -> writer_loop(Point, Coordinator) end),
  start_writers(Count - 1, Point, Coordinator, [Pid | Acc]).

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
handle_completion_message({'DOWN', _Mon, process, _Pid, normal}, State) ->
  State#state{writer_down = State#state.writer_down + 1};
handle_completion_message({'DOWN', _Mon, process, Pid, Reason}, _State) ->
  exit({process_failed, Pid, Reason});
handle_completion_message(Message, _State) ->
  exit({unexpected_completion_message, Message}).


%%====================================================================
%% Writers
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
  ok = call_operation(Point),
  writer_operations(Seq + 1, Completed + 1, Point);
writer_operations(Seq, Completed, #point{pace_ms = PaceMs, run_ref = RunRef} = Point) ->
  TimerRef = erlang:start_timer(PaceMs, self(), {?TAG, RunRef, pace, Seq}),
  ok = call_operation(Point),
  receive
    {timeout, TimerRef, {?TAG, RunRef, pace, Seq}} ->
      writer_operations(Seq + 1, Completed + 1, Point);
    Message ->
      exit({unexpected_pace_message, Message})
  end.

call_operation(#point{
    path = native,
    receiver_node = Node,
    run_ref = RunRef,
    payload = Payload}) ->
  {RunRef, ok} =
    erpc:call(Node, erlang, element, [1, {{RunRef, ok}, Payload}]),
  ok;
call_operation(#point{
    path = ecall,
    receiver_node = Node,
    run_ref = RunRef,
    payload = Payload}) ->
  {ok, {RunRef, ok}} =
    ecall:call(Node, erlang, element, [1, {{RunRef, ok}, Payload}]),
  ok.


%%====================================================================
%% Cleanup and reporting
%%====================================================================

cleanup_point(#state{writer_pids = WriterPids}) ->
  [exit(Pid, kill) || Pid <- WriterPids],
  ok.

result_map(Point, State, ElapsedMs) ->
  Base = #{
    suite => ?MODULE,
    operation => call,
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
