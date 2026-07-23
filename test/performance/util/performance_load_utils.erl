-module(performance_load_utils).

%% Point API
-export([
  run_point/1,
  run_send_point/1,
  run_cast_point/1,
  run_call_point/1
]).

%% Receiver target API
-export([
  start_send_target/3,
  start_cast_counter/3,
  stop_target/2,
  cast_target/3,
  call_target/2
]).

-define(TAG, performance_load_utils).
-define(DEFAULT_TIMEOUT_MS, infinity).

-record(point, {
  run_ref,
  operation,
  path,
  receiver_node,
  target = undefined,
  payload_profile,
  payload,
  writer_count,
  messages_per_writer,
  pace_ms,
  expected,
  timeout_ms = ?DEFAULT_TIMEOUT_MS,
  result_metadata = #{}
}).

-record(state, {
  point,
  target_pid = undefined,
  target_mon = undefined,
  target_ready = true,
  remote_completed = true,
  remote_count = 0,
  writer_monitors = #{},
  writer_status = #{},
  ready = 0,
  completed = 0,
  writer_down = 0,
  extra_monitors = #{},
  node_monitored = false
}).

-type operation() :: send | cast | call.
-type path() :: raw | erpc | ecall.
-type point_config() :: #{
  operation => operation(),
  path := path() | raw_send | ecall_send | erpc_cast | ecall_cast |
    erpc_call | ecall_call,
  receiver_node := node(),
  payload_profile := performance_payloads:profile(),
  payload => term(),
  writer_count := pos_integer(),
  messages_per_writer := pos_integer(),
  pace_ms := non_neg_integer(),
  timeout_ms => timeout(),
  run_ref => reference(),
  monitor_pids => [pid()],
  suite => atom(),
  configured_batch_size => pos_integer(),
  effective_batch_size => pos_integer(),
  configured_distribution_busy_limit_kib => pos_integer(),
  effective_distribution_busy_limit_kib => pos_integer()
}.

%%====================================================================
%% Point API
%%====================================================================

-spec run_send_point(point_config()) -> map().
run_send_point(Config) ->
  run_point(Config#{operation => send}).

-spec run_cast_point(point_config()) -> map().
run_cast_point(Config) ->
  run_point(Config#{operation => cast}).

-spec run_call_point(point_config()) -> map().
run_call_point(Config) ->
  run_point(Config#{operation => call}).

-spec run_point(point_config()) -> map().
run_point(Config) ->
  Point0 = point(Config),
  Point = Point0#point{payload = point_payload(Config, Point0)},
  State0 = start_point_participants(Point, Config),
  try
    State1 = await_ready(State0),
    StartedAt = erlang:monotonic_time(millisecond),
    release_writers(State1),
    State2 = await_completion(State1),
    State3 = final_monitor_check(State2),
    ElapsedMs = erlang:monotonic_time(millisecond) - StartedAt,
    cleanup_success(State3),
    result_map(Point, State3#state.completed, ElapsedMs)
  catch
    throw:{point_failed, State, Reason} ->
      cleanup_failure(State),
      exit({performance_point_failed, Reason, point_identity(State#state.point)})
  end.

%%====================================================================
%% Receiver target API
%%====================================================================

-spec start_send_target(reference(), pid(), non_neg_integer()) -> pid().
start_send_target(RunRef, Sender, Expected) ->
  spawn(fun() ->
    Sender ! {?TAG, RunRef, target_ready, self(), send},
    counter_loop(RunRef, Sender, Expected, 0, send)
  end).

-spec start_cast_counter(reference(), pid(), non_neg_integer()) -> pid().
start_cast_counter(RunRef, Sender, Expected) ->
  spawn(fun() ->
    Sender ! {?TAG, RunRef, target_ready, self(), cast},
    counter_loop(RunRef, Sender, Expected, 0, cast)
  end).

-spec stop_target(pid(), reference()) -> ok.
stop_target(Target, RunRef) ->
  Target ! {?TAG, RunRef, stop, self()},
  receive
    {?TAG, RunRef, stopped, Target, _Count} ->
      ok
  end.

-spec cast_target(pid(), reference(), term()) -> ok.
cast_target(Counter, RunRef, Payload) ->
  try
    Counter ! {?TAG, RunRef, cast_payload, Payload},
    ok
  catch
    Class:Reason:Stack ->
      Counter ! {?TAG, RunRef, cast_failure, {Class, Reason, Stack}},
      exit(Reason)
  end.

-spec call_target(reference(), term()) -> {reference(), ok}.
call_target(RunRef, _Payload) ->
  {RunRef, ok}.

%%====================================================================
%% SETUP
%%====================================================================

point(Config) ->
  Operation = operation(Config),
  Path = path(Operation, maps:get(path, Config)),
  WriterCount = maps:get(writer_count, Config),
  MessagesPerWriter = maps:get(messages_per_writer, Config),
  #point{
    run_ref = maps:get(run_ref, Config, make_ref()),
    operation = Operation,
    path = Path,
    receiver_node = maps:get(receiver_node, Config),
    payload_profile = maps:get(payload_profile, Config),
    writer_count = WriterCount,
    messages_per_writer = MessagesPerWriter,
    pace_ms = maps:get(pace_ms, Config),
    expected = WriterCount * MessagesPerWriter,
    timeout_ms = maps:get(timeout_ms, Config, ?DEFAULT_TIMEOUT_MS),
    result_metadata = result_metadata(Config)
  }.

operation(#{operation := Operation}) ->
  Operation;
operation(#{path := raw_send}) ->
  send;
operation(#{path := ecall_send}) ->
  send;
operation(#{path := erpc_cast}) ->
  cast;
operation(#{path := ecall_cast}) ->
  cast;
operation(#{path := erpc_call}) ->
  call;
operation(#{path := ecall_call}) ->
  call.

path(send, raw_send) ->
  raw;
path(send, raw) ->
  raw;
path(send, ecall_send) ->
  ecall;
path(send, ecall) ->
  ecall;
path(cast, erpc_cast) ->
  erpc;
path(cast, erpc) ->
  erpc;
path(cast, ecall_cast) ->
  ecall;
path(cast, ecall) ->
  ecall;
path(call, erpc_call) ->
  erpc;
path(call, erpc) ->
  erpc;
path(call, ecall_call) ->
  ecall;
path(call, ecall) ->
  ecall.

point_payload(Config, #point{payload_profile = Profile}) ->
  case maps:find(payload, Config) of
    {ok, Payload} ->
      Payload;
    error ->
      performance_payloads:new(Profile)
  end.

result_metadata(Config) ->
  Keys = [
    suite,
    configured_batch_size,
    effective_batch_size,
    configured_distribution_busy_limit_kib,
    effective_distribution_busy_limit_kib
  ],
  maps:with(Keys, Config).

start_point_participants(#point{operation = call} = Point, Config) ->
  State0 = #state{point = Point},
  State1 = monitor_receiver_node(State0),
  State2 = monitor_extra_pids(State1, maps:get(monitor_pids, Config, [])),
  spawn_writers(Point#point.writer_count, Point, State2);
start_point_participants(Point, Config) ->
  Target = start_receiver_target(Point),
  true = is_pid(Target),
  TargetMon = erlang:monitor(process, Target),
  State0 = #state{
    point = Point#point{target = Target},
    target_pid = Target,
    target_mon = TargetMon,
    target_ready = false,
    remote_completed = false
  },
  State1 = monitor_receiver_node(State0),
  State2 = monitor_extra_pids(State1, maps:get(monitor_pids, Config, [])),
  spawn_writers(Point#point.writer_count, State1#state.point, State2).

start_receiver_target(#point{
    operation = send,
    run_ref = RunRef,
    receiver_node = Node,
    expected = Expected}) ->
  erpc:call(Node, ?MODULE, start_send_target, [RunRef, self(), Expected]);
start_receiver_target(#point{
    operation = cast,
    run_ref = RunRef,
    receiver_node = Node,
    expected = Expected}) ->
  erpc:call(Node, ?MODULE, start_cast_counter, [RunRef, self(), Expected]).

monitor_receiver_node(#state{point = #point{receiver_node = Node}} = State)
    when Node =:= node() ->
  State;
monitor_receiver_node(#state{point = #point{receiver_node = Node}} = State) ->
  true = erlang:monitor_node(Node, true),
  State#state{node_monitored = true}.

monitor_extra_pids(State, []) ->
  State;
monitor_extra_pids(State, [Pid | Pids]) ->
  Mon = erlang:monitor(process, Pid),
  monitor_extra_pids(
    State#state{extra_monitors = (State#state.extra_monitors)#{Mon => Pid}},
    Pids).

spawn_writers(0, _Point, State) ->
  State;
spawn_writers(Count, Point, State) ->
  Coordinator = self(),
  {Pid, Mon} = spawn_monitor(fun() -> writer_loop(Point, Coordinator) end),
  spawn_writers(
    Count - 1,
    Point,
    State#state{
      writer_monitors = (State#state.writer_monitors)#{Mon => Pid},
      writer_status = (State#state.writer_status)#{Pid => waiting}
    }).

%%====================================================================
%% COORDINATOR
%%====================================================================

await_ready(#state{
    point = #point{writer_count = WriterCount},
    ready = WriterCount,
    target_ready = true} = State) ->
  State;
await_ready(State) ->
  receive
    Message ->
      await_ready(handle_message(Message, State, ready))
  after (State#state.point)#point.timeout_ms ->
    fail(State, {ready_timeout, point_identity(State#state.point)})
  end.

release_writers(#state{point = #point{run_ref = RunRef}, writer_status = Status}) ->
  maps:foreach(
    fun(Pid, _Status) ->
      Pid ! {?TAG, RunRef, start}
    end,
    Status).

await_completion(#state{
    point = #point{writer_count = WriterCount},
    writer_down = WriterCount,
    remote_completed = true} = State) ->
  Expected = (State#state.point)#point.expected,
  case State#state.completed of
    Expected ->
      State;
    Completed ->
      fail(State, {wrong_completed_count, Completed, Expected})
  end;
await_completion(State) ->
  receive
    Message ->
      await_completion(handle_message(Message, State, completion))
  after (State#state.point)#point.timeout_ms ->
    fail(State, {completion_timeout, point_identity(State#state.point)})
  end.

handle_message({?TAG, RunRef, target_ready, Target, _Kind}, State, _Phase)
    when RunRef =:= (State#state.point)#point.run_ref,
         Target =:= State#state.target_pid ->
  State#state{target_ready = true};
handle_message({?TAG, RunRef, target_completed, Target, Count}, State, _Phase)
    when RunRef =:= (State#state.point)#point.run_ref,
         Target =:= State#state.target_pid ->
  Expected = (State#state.point)#point.expected,
  case Count of
    Expected ->
      State#state{remote_completed = true, remote_count = Count};
    _ ->
      fail(State, {wrong_remote_count, Count, Expected})
  end;
handle_message({?TAG, RunRef, target_failed, Target, Reason}, State, _Phase)
    when RunRef =:= (State#state.point)#point.run_ref,
         Target =:= State#state.target_pid ->
  fail(State, {target_failed, Target, Reason});
handle_message({?TAG, RunRef, writer_ready, Pid}, State, ready)
    when RunRef =:= (State#state.point)#point.run_ref ->
  writer_ready(Pid, State);
handle_message({?TAG, RunRef, writer_completed, Pid, Count}, State, completion)
    when RunRef =:= (State#state.point)#point.run_ref ->
  writer_completed(Pid, Count, State);
handle_message({'DOWN', Mon, process, Pid, Reason}, State, _Phase) ->
  handle_down(Mon, Pid, Reason, State);
handle_message({nodedown, Node}, State, _Phase) ->
  fail(State, {receiver_node_down, Node});
handle_message({?TAG, OtherRunRef, _Tag, _A}, State, _Phase) ->
  fail(State, {unexpected_run_ref, OtherRunRef});
handle_message({?TAG, OtherRunRef, _Tag, _A, _B}, State, _Phase) ->
  fail(State, {unexpected_run_ref, OtherRunRef});
handle_message(Message, State, _Phase) ->
  fail(State, {unexpected_control_message, Message}).

writer_ready(Pid, #state{writer_status = Status, ready = Ready} = State) ->
  case maps:get(Pid, Status) of
    waiting ->
      State#state{
        writer_status = Status#{Pid => ready},
        ready = Ready + 1
      };
    Current ->
      fail(State, {duplicate_writer_ready, Pid, Current})
  end.

writer_completed(Pid, Count, #state{
    writer_status = Status,
    completed = Completed} = State) ->
  MessagesPerWriter = (State#state.point)#point.messages_per_writer,
  Expected = (State#state.point)#point.expected,
  case {maps:get(Pid, Status), Count} of
    {ready, MessagesPerWriter} when Completed + Count =< Expected ->
      State#state{
        writer_status = Status#{Pid => completed},
        completed = Completed + Count
      };
    {ready, _WrongCount} ->
      fail(State, {wrong_writer_completed_count, Pid, Count, MessagesPerWriter});
    {Current, _Count} ->
      fail(State, {unexpected_writer_completed, Pid, Current})
  end.

handle_down(Mon, Pid, normal, #state{target_mon = Mon} = State) ->
  fail(State, {target_down_before_stop, Pid});
handle_down(Mon, Pid, Reason, #state{target_mon = Mon} = State) ->
  fail(State, {target_down, Pid, Reason});
handle_down(Mon, Pid, Reason, #state{writer_monitors = Monitors} = State) ->
  case maps:take(Mon, Monitors) of
    {Pid, Monitors1} ->
      handle_writer_down(Pid, Reason, State#state{writer_monitors = Monitors1});
    error ->
      handle_extra_down(Mon, Pid, Reason, State)
  end.

handle_writer_down(Pid, normal, #state{
    writer_status = Status,
    writer_down = Down} = State) ->
  case maps:get(Pid, Status) of
    completed ->
      State#state{writer_down = Down + 1};
    Current ->
      fail(State, {writer_down_before_completed, Pid, Current})
  end;
handle_writer_down(Pid, Reason, State) ->
  fail(State, {writer_failed, Pid, Reason}).

handle_extra_down(Mon, Pid, Reason, #state{extra_monitors = Extra} = State) ->
  case maps:is_key(Mon, Extra) of
    true ->
      fail(State, {monitored_process_down, Pid, Reason});
    false ->
      fail(State, {unexpected_down, Pid, Reason})
  end.

fail(State, Reason) ->
  throw({point_failed, State, Reason}).

%%====================================================================
%% WRITERS
%%====================================================================

writer_loop(Point, Coordinator) ->
  RunRef = Point#point.run_ref,
  Coordinator ! {?TAG, RunRef, writer_ready, self()},
  receive
    {?TAG, RunRef, start} ->
      Completed = writer_operations(1, 0, Point),
      Coordinator ! {?TAG, RunRef, writer_completed, self(), Completed};
    {?TAG, OtherRunRef, _Tag} ->
      exit({unexpected_writer_run_ref, OtherRunRef});
    Message ->
      exit({unexpected_writer_message, Message})
  end.

writer_operations(Seq, Completed, #point{messages_per_writer = Max})
    when Seq > Max ->
  Completed;
writer_operations(Seq, Completed, Point) when Seq =:= Point#point.messages_per_writer ->
  do_operation(Point),
  writer_operations(Seq + 1, Completed + 1, Point);
writer_operations(Seq, Completed, #point{pace_ms = PaceMs, run_ref = RunRef} = Point) ->
  TimerRef = erlang:start_timer(PaceMs, self(), {?TAG, RunRef, pace, Seq}),
  do_operation(Point),
  wait_pace_timer(RunRef, Seq, TimerRef),
  writer_operations(Seq + 1, Completed + 1, Point).

wait_pace_timer(RunRef, Seq, TimerRef) ->
  receive
    {timeout, TimerRef, {?TAG, RunRef, pace, Seq}} ->
      ok;
    {?TAG, OtherRunRef, _Tag} ->
      exit({unexpected_writer_run_ref, OtherRunRef});
    Message ->
      exit({unexpected_writer_message, Message})
  end.

do_operation(#point{operation = send, path = raw, target = Target, payload = Payload}) ->
  Target ! Payload,
  ok;
do_operation(#point{operation = send, path = ecall, target = Target, payload = Payload}) ->
  _ = ecall:send(Target, Payload),
  ok;
do_operation(#point{
    operation = cast,
    path = erpc,
    receiver_node = Node,
    target = Target,
    run_ref = RunRef,
    payload = Payload}) ->
  ok = erpc:cast(Node, ?MODULE, cast_target, [Target, RunRef, Payload]);
do_operation(#point{
    operation = cast,
    path = ecall,
    receiver_node = Node,
    target = Target,
    run_ref = RunRef,
    payload = Payload}) ->
  ok = ecall:cast(Node, ?MODULE, cast_target, [Target, RunRef, Payload]);
do_operation(#point{
    operation = call,
    path = erpc,
    receiver_node = Node,
    run_ref = RunRef,
    payload = Payload}) ->
  {RunRef, ok} = erpc:call(Node, ?MODULE, call_target, [RunRef, Payload]),
  ok;
do_operation(#point{
    operation = call,
    path = ecall,
    receiver_node = Node,
    run_ref = RunRef,
    payload = Payload}) ->
  {ok, {RunRef, ok}} = ecall:call(Node, ?MODULE, call_target, [RunRef, Payload]),
  ok.

%%====================================================================
%% RECEIVER COUNTER
%%====================================================================

counter_loop(RunRef, Sender, Expected, Count, Mode) ->
  receive
    {?TAG, RunRef, stop, From} ->
      From ! {?TAG, RunRef, stopped, self(), Count};
    {?TAG, RunRef, cast_payload, _Payload} when Mode =:= cast ->
      counter_observe(RunRef, Sender, Expected, Count, Mode);
    {?TAG, RunRef, cast_failure, Reason} when Mode =:= cast ->
      Sender ! {?TAG, RunRef, target_failed, self(), Reason},
      exit({cast_target_failed, Reason});
    {?TAG, OtherRunRef, _Tag, _A} ->
      Sender ! {?TAG, RunRef, target_failed, self(), {unexpected_run_ref, OtherRunRef}},
      exit({unexpected_run_ref, OtherRunRef});
    {?TAG, OtherRunRef, _Tag, _A, _B} ->
      Sender ! {?TAG, RunRef, target_failed, self(), {unexpected_run_ref, OtherRunRef}},
      exit({unexpected_run_ref, OtherRunRef});
    _Payload when Mode =:= send ->
      counter_observe(RunRef, Sender, Expected, Count, Mode);
    Message ->
      Sender ! {?TAG, RunRef, target_failed, self(), {unexpected_target_message, Message}},
      exit({unexpected_target_message, Message})
  end.

counter_observe(RunRef, Sender, Expected, Count, Mode) ->
  Count1 = Count + 1,
  case Count1 of
    Expected ->
      Sender ! {?TAG, RunRef, target_completed, self(), Count1},
      counter_loop(RunRef, Sender, Expected, Count1, Mode);
    _ when Count1 < Expected ->
      counter_loop(RunRef, Sender, Expected, Count1, Mode);
    _ ->
      Sender ! {?TAG, RunRef, target_failed, self(), {too_many_messages, Count1, Expected}},
      exit({too_many_messages, Count1, Expected})
  end.

%%====================================================================
%% CLEANUP AND RESULTS
%%====================================================================

cleanup_success(State) ->
  wait_target_stop(State),
  demonitor_extra(State),
  unmonitor_receiver_node(State),
  ok.

final_monitor_check(State) ->
  receive
    {'DOWN', Mon, process, Pid, Reason} ->
      final_monitor_check(handle_down(Mon, Pid, Reason, State));
    {nodedown, Node} ->
      fail(State, {receiver_node_down, Node})
  after 0 ->
    State
  end.

cleanup_failure(State) ->
  stop_writers(State),
  stop_target_quietly(State),
  demonitor_extra(State),
  unmonitor_receiver_node(State),
  ok.

wait_target_stop(#state{target_pid = undefined}) ->
  ok;
wait_target_stop(#state{
    point = #point{run_ref = RunRef, timeout_ms = Timeout},
    target_pid = Target,
    target_mon = Mon} = State) ->
  Target ! {?TAG, RunRef, stop, self()},
  wait_target_stop(State, true, true, Timeout, Mon, Target).

wait_target_stop(_State, false, false, _Timeout, _Mon, _Target) ->
  ok;
wait_target_stop(State, NeedAck, NeedDown, Timeout, Mon, Target) ->
  RunRef = (State#state.point)#point.run_ref,
  receive
    {?TAG, RunRef, stopped, Target, _Count} ->
      wait_target_stop(State, false, NeedDown, Timeout, Mon, Target);
    {'DOWN', Mon, process, Target, normal} ->
      wait_target_stop(State, NeedAck, false, Timeout, Mon, Target);
    {'DOWN', Mon, process, Target, Reason} ->
      fail(State, {target_stop_failed, Target, Reason});
    Message ->
      fail(State, {unexpected_cleanup_message, Message})
  after Timeout ->
    fail(State, {target_stop_timeout, Target})
  end.

stop_target_quietly(#state{target_pid = undefined}) ->
  ok;
stop_target_quietly(#state{
    point = #point{run_ref = RunRef},
    target_pid = Target,
    target_mon = Mon}) ->
  Target ! {?TAG, RunRef, stop, self()},
  exit(Target, kill),
  erlang:demonitor(Mon, [flush]),
  ok.

stop_writers(#state{writer_status = Status, writer_monitors = Monitors}) ->
  maps:foreach(fun(Pid, _Status) -> exit(Pid, kill) end, Status),
  maps:foreach(fun(Mon, _Pid) -> erlang:demonitor(Mon, [flush]) end, Monitors).

demonitor_extra(#state{extra_monitors = Extra}) ->
  maps:foreach(fun(Mon, _Pid) -> erlang:demonitor(Mon, [flush]) end, Extra).

unmonitor_receiver_node(#state{node_monitored = false}) ->
  ok;
unmonitor_receiver_node(#state{point = #point{receiver_node = Node}}) ->
  erlang:monitor_node(Node, false),
  ok.

result_map(Point, Completed, ElapsedMs) ->
  Base = #{
    operation => Point#point.operation,
    path => Point#point.path,
    payload_profile => Point#point.payload_profile,
    writer_count => Point#point.writer_count,
    pace_ms => Point#point.pace_ms,
    messages_per_writer => Point#point.messages_per_writer,
    expected => Point#point.expected,
    completed => Completed,
    elapsed_ms => ElapsedMs,
    completed_per_second => completed_per_second(Completed, ElapsedMs)
  },
  maps:merge(Base, Point#point.result_metadata).

completed_per_second(_Completed, 0) ->
  0.0;
completed_per_second(Completed, ElapsedMs) ->
  (Completed * 1000) / ElapsedMs.

point_identity(Point) ->
  Base = #{
    operation => Point#point.operation,
    path => Point#point.path,
    payload_profile => Point#point.payload_profile,
    writer_count => Point#point.writer_count,
    pace_ms => Point#point.pace_ms,
    messages_per_writer => Point#point.messages_per_writer,
    expected => Point#point.expected
  },
  maps:merge(Base, Point#point.result_metadata).
