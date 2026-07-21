-module(performance_load).

-export([
  payload/1,
  offered_per_second/1,
  fixed_work_quotas/2,
  repetition_indexes/2,
  start_receiver_counter/0,
  await_receiver_count/3,
  receiver_stats/2,
  raw_send_n/3,
  stop_receiver_counter/2,
  start_e3_call_pacing/3,
  run_send/5,
  run_cast/6,
  run_call/5,
  run_fixed_work_raw_send/5,
  run_proxy_batching/5,
  run_proxy_pool_paced_workload/1,
  run_paced_workload/1
]).

payload(tiny) ->
  tiny;
payload(data) ->
  #{
    archive1 => data_archive(),
    archive2 => data_archive(),
    archive3 => data_archive()
  };
payload(binary_100k) ->
  binary:copy(<<0>>, 100 * 1024);
payload(binary_1m) ->
  binary:copy(<<0>>, 1024 * 1024).

offered_per_second(ClientCount) ->
  ClientCount * 10.

fixed_work_quotas(WorkTotal, Writers) ->
  Base = WorkTotal div Writers,
  Remainder = WorkTotal rem Writers,
  [Base + quota_extra(Index, Remainder) || Index <- lists:seq(1, Writers)].

repetition_indexes(RunConfig, smoke) ->
  indexes(maps:get(smoke_repetitions, RunConfig));
repetition_indexes(RunConfig, report) ->
  indexes(maps:get(report_repetitions, RunConfig)).

start_receiver_counter() ->
  spawn(fun() -> receiver_counter_loop(0, 0, []) end).

await_receiver_count(Receiver, Expected, Timeout) ->
  Ref = make_ref(),
  Receiver ! {performance_load_wait, self(), Ref, Expected},
  receive
    {performance_load_count, Ref, Count} ->
      Count
  after Timeout ->
    exit({receiver_counter_wait_timeout, Receiver, Expected})
  end.

receiver_stats(Receiver, Timeout) ->
  Ref = make_ref(),
  Receiver ! {performance_load_stats, self(), Ref},
  receive
    {performance_load_stats, Ref, Stats} ->
      Stats
  after Timeout ->
    exit({receiver_counter_stats_timeout, Receiver})
  end.

raw_send_n(_Receiver, 0, _Payload) ->
  ok;
raw_send_n(Receiver, Count, Payload) when Count > 0 ->
  Receiver ! Payload,
  raw_send_n(Receiver, Count - 1, Payload).

stop_receiver_counter(Receiver, Timeout) ->
  Ref = make_ref(),
  Receiver ! {performance_load_stop, self(), Ref},
  receive
    {performance_load_counter, Ref, Count} ->
      Count
  after Timeout ->
    exit({receiver_counter_timeout, Receiver})
  end.

run_send(Path, Receiver, Count, Payload, Timeout) ->
  Baseline = receiver_count(Receiver, Timeout),
  ok =
    repeat(
      Count,
      fun() ->
        send_once(Path, Receiver, Payload)
      end),
  Delivered = await_receiver_count(Receiver, Baseline + Count, Timeout) - Baseline,
  #{path => Path, delivered => Delivered}.

run_cast(Path, Node, Receiver, Count, Payload, Timeout) ->
  Baseline = receiver_count(Receiver, Timeout),
  ok =
    repeat(
      Count,
      fun() ->
        cast_once(Path, Node, Receiver, Payload)
      end),
  Delivered = await_receiver_count(Receiver, Baseline + Count, Timeout) - Baseline,
  #{path => Path, delivered => Delivered}.

run_call(Path, Node, Count, Payload, _Timeout) ->
  Completed =
    repeat_count(
      Count,
      fun() ->
        call_once(Path, Node, Payload)
      end),
  #{path => Path, completed => Completed}.

run_fixed_work_raw_send(Receiver, WorkTotal, Writers, Payload, Timeout) ->
  Baseline = receiver_count(Receiver, Timeout),
  Quotas = fixed_work_quotas(WorkTotal, Writers),
  Barrier = make_ref(),
  Owner = self(),
  WriterRefs =
    [
      spawn_monitor(fun() ->
        Owner ! {performance_load_writer_ready, self()},
        receive
          {Barrier, start} ->
            raw_send_n(Receiver, Quota, Payload)
        end
      end)
      || Quota <- Quotas
    ],
  wait_writer_ready(Writers),
  StartedAt = erlang:monotonic_time(millisecond),
  [Pid ! {Barrier, start} || {Pid, _MonitorRef} <- WriterRefs],
  Delivered = await_receiver_count(Receiver, Baseline + WorkTotal, Timeout) - Baseline,
  CompletionMs = erlang:monotonic_time(millisecond) - StartedAt,
  wait_writer_downs([MonitorRef || {_Pid, MonitorRef} <- WriterRefs]),
  #{
    path => fixed_work_raw_send,
    delivered => Delivered,
    completion_ms => CompletionMs,
    writers => Writers,
    quotas => Quotas
  }.

run_proxy_batching(Receiver, Count, Payload, BatchSize, Timeout) ->
  Baseline = receiver_count(Receiver, Timeout),
  Proxy = spawn(fun() -> proxy_loop(Receiver, BatchSize, [], 0, 0, 0) end),
  ok =
    repeat(
      Count,
      fun() ->
        Proxy ! {performance_load_proxy_send, Payload}
      end),
  Metrics = stop_proxy(Proxy, Timeout),
  Delivered = await_receiver_count(Receiver, Baseline + Count, Timeout) - Baseline,
  maps:merge(#{path => proxy_batching, delivered => Delivered}, Metrics).

run_proxy_pool_paced_workload(Config) ->
  Receiver = maps:get(receiver, Config),
  ClientCount = maps:get(client_count, Config),
  Payload = maps:get(payload, Config),
  BatchSize = maps:get(batch_size, Config),
  ProxyPoolSize = maps:get(proxy_pool_size, Config),
  WarmupMs = maps:get(warmup_ms, Config),
  MeasurementMs = maps:get(measurement_ms, Config),
  DrainMs = maps:get(drain_ms, Config),
  Timeout = maps:get(timeout_ms, Config),
  Ticks = paced_tick_count(MeasurementMs),
  Scheduled = ClientCount * Ticks,
  Baseline = receiver_count(Receiver, Timeout),
  Proxies = start_proxy_pool(Receiver, BatchSize, ProxyPoolSize),
  sleep_ms(WarmupMs),
  StartedAt = erlang:monotonic_time(millisecond),
  Deadline = StartedAt + MeasurementMs,
  Completed = run_proxy_pool_paced_clients(ClientCount, Ticks, Proxies, Payload),
  sleep_until(Deadline),
  sleep_ms(DrainMs),
  Metrics = stop_proxy_pool(Proxies, Timeout),
  Delivered = await_receiver_count(Receiver, Baseline + Completed, Timeout) - Baseline,
  ElapsedMs = erlang:monotonic_time(millisecond) - StartedAt,
  maps:merge(
    #{
      path => proxy_batching,
      client_count => ClientCount,
      batch_size => BatchSize,
      proxy_pool_size => ProxyPoolSize,
      warmup_ms => WarmupMs,
      measurement_ms => MeasurementMs,
      drain_ms => DrainMs,
      ticks => Ticks,
      scheduled => Scheduled,
      started => Completed,
      completed => Completed,
      offered_per_second => offered_per_second(ClientCount),
      delivered => Delivered,
      elapsed_ms => ElapsedMs
    },
    Metrics).

run_paced_workload(Config) ->
  Path = maps:get(path, Config),
  ClientCount = maps:get(client_count, Config),
  Payload = maps:get(payload, Config),
  WarmupMs = maps:get(warmup_ms, Config),
  MeasurementMs = maps:get(measurement_ms, Config),
  DrainMs = maps:get(drain_ms, Config),
  Timeout = maps:get(timeout_ms, Config),
  Receiver = maps:get(receiver, Config, undefined),
  Node = maps:get(node, Config, node()),
  CallTarget = maps:get(call_target, Config, default_call_target(Path)),
  Ticks = paced_tick_count(MeasurementMs),
  Scheduled = ClientCount * Ticks,
  Baseline = receiver_baseline(Path, Receiver, Timeout),
  sleep_ms(WarmupMs),
  StartedAt = erlang:monotonic_time(millisecond),
  Deadline = StartedAt + MeasurementMs,
  {Completed, Started} =
    case call_path(Path) of
      true ->
        run_paced_call_clients(
          ClientCount, Ticks, StartedAt, Deadline, DrainMs, Path, Node, Payload, CallTarget);
      false ->
        Completed0 = run_paced_clients(ClientCount, Ticks, Path, Node, Receiver, Payload),
        sleep_until(Deadline),
        {Completed0, Completed0}
    end,
  sleep_ms(DrainMs),
  ElapsedMs = erlang:monotonic_time(millisecond) - StartedAt,
  Delivered = paced_delivered(Path, Receiver, Baseline, Timeout),
  #{
    path => Path,
    client_count => ClientCount,
    warmup_ms => WarmupMs,
    measurement_ms => MeasurementMs,
    drain_ms => DrainMs,
    ticks => Ticks,
    scheduled => Scheduled,
    started => Started,
    offered_per_second => offered_per_second(ClientCount),
    delivered => Delivered,
    completed => Completed,
    elapsed_ms => ElapsedMs
  }.

start_e3_call_pacing(ClientIds, CallsPerClient, CallFun) ->
  Owner = self(),
  Ref = make_ref(),
  Coordinator =
    spawn(fun() ->
      MonitorRefs =
        [
          element(2, spawn_monitor(fun() ->
            exit({completed, e3_call_client(ClientId, CallsPerClient, CallFun)})
          end))
          || ClientId <- ClientIds
        ],
      Completed = collect_e3_clients(MonitorRefs, 0),
      Owner ! {Ref, #{completed => Completed}}
    end),
  {Coordinator, Ref}.

receiver_count(Receiver, Timeout) ->
  Ref = make_ref(),
  Receiver ! {performance_load_snapshot, self(), Ref},
  receive
    {performance_load_count, Ref, Count} ->
      Count
  after Timeout ->
    exit({receiver_counter_snapshot_timeout, Receiver})
  end.

send_once(raw_send, Receiver, Payload) ->
  Receiver ! Payload,
  ok;
send_once(ecall_send, Receiver, Payload) ->
  _ = ecall:send(Receiver, Payload),
  ok.

cast_once(erpc_cast, Node, Receiver, Payload) ->
  erpc:cast(Node, performance_workload_SUITE, cast_target, [Receiver, Payload]);
cast_once(ecall_cast, Node, Receiver, Payload) ->
  ecall:cast(Node, performance_workload_SUITE, cast_target, [Receiver, Payload]).

call_once(erpc_call, Node, Payload) ->
  ok = erpc:call(Node, performance_workload_SUITE, call_target, [Payload]),
  true;
call_once(ecall_call, Node, Payload) ->
  {ok, ok} = ecall:call(Node, performance_workload_SUITE, call_target, [Payload]),
  true.

default_call_target(erpc_call) ->
  {performance_workload_SUITE, call_target, []};
default_call_target(ecall_call) ->
  {performance_workload_SUITE, call_target, []};
default_call_target(_Path) ->
  undefined.

call_path(erpc_call) ->
  true;
call_path(ecall_call) ->
  true;
call_path(_Path) ->
  false.

receiver_baseline(raw_send, Receiver, Timeout) ->
  receiver_count(Receiver, Timeout);
receiver_baseline(ecall_send, Receiver, Timeout) ->
  receiver_count(Receiver, Timeout);
receiver_baseline(erpc_cast, Receiver, Timeout) ->
  receiver_count(Receiver, Timeout);
receiver_baseline(ecall_cast, Receiver, Timeout) ->
  receiver_count(Receiver, Timeout);
receiver_baseline(_Path, _Receiver, _Timeout) ->
  0.

paced_delivered(raw_send, Receiver, Baseline, Timeout) ->
  receiver_count(Receiver, Timeout) - Baseline;
paced_delivered(ecall_send, Receiver, Baseline, Timeout) ->
  receiver_count(Receiver, Timeout) - Baseline;
paced_delivered(erpc_cast, Receiver, Baseline, Timeout) ->
  receiver_count(Receiver, Timeout) - Baseline;
paced_delivered(ecall_cast, Receiver, Baseline, Timeout) ->
  receiver_count(Receiver, Timeout) - Baseline;
paced_delivered(_Path, _Receiver, _Baseline, _Timeout) ->
  0.

paced_tick_count(MeasurementMs) ->
  (MeasurementMs + 99) div 100.

run_paced_clients(ClientCount, Ticks, Path, Node, Receiver, Payload) ->
  MonitorRefs =
    [
      element(2, spawn_monitor(fun() ->
        exit({completed, run_paced_client(Ticks, Path, Node, Receiver, Payload)})
      end))
      || _Client <- lists:seq(1, ClientCount)
    ],
  collect_paced_clients(MonitorRefs, 0).

run_paced_client(Ticks, Path, Node, Receiver, Payload) ->
  run_paced_client(1, Ticks, Path, Node, Receiver, Payload, 0).

run_paced_client(Tick, Ticks, _Path, _Node, _Receiver, _Payload, Completed) when Tick > Ticks ->
  Completed;
run_paced_client(Tick, Ticks, Path, Node, Receiver, Payload, Completed) ->
  TickStartedAt = erlang:monotonic_time(millisecond),
  _ = paced_operation(Path, Node, Receiver, Payload),
  Completed1 = Completed + 1,
  case Tick < Ticks of
    true ->
      sleep_until(TickStartedAt + 100),
      run_paced_client(Tick + 1, Ticks, Path, Node, Receiver, Payload, Completed1);
    false ->
      Completed1
  end.

paced_operation(raw_send, _Node, Receiver, Payload) ->
  send_once(raw_send, Receiver, Payload);
paced_operation(ecall_send, _Node, Receiver, Payload) ->
  send_once(ecall_send, Receiver, Payload);
paced_operation(erpc_cast, Node, Receiver, Payload) ->
  cast_once(erpc_cast, Node, Receiver, Payload);
paced_operation(ecall_cast, Node, Receiver, Payload) ->
  cast_once(ecall_cast, Node, Receiver, Payload);
paced_operation(erpc_call, Node, _Receiver, Payload) ->
  call_once(erpc_call, Node, Payload);
paced_operation(ecall_call, Node, _Receiver, Payload) ->
  call_once(ecall_call, Node, Payload).

collect_paced_clients([], Completed) ->
  Completed;
collect_paced_clients(MonitorRefs, Completed) ->
  receive
    {'DOWN', MonitorRef, process, _Pid, {completed, Count}} ->
      collect_paced_clients(lists:delete(MonitorRef, MonitorRefs), Completed + Count);
    {'DOWN', _MonitorRef, process, _Pid, Reason} ->
      exit({paced_client_failed, Reason})
  end.

run_paced_call_clients(ClientCount, Ticks, StartedAt, Deadline, DrainMs, Path, Node, Payload, CallTarget) ->
  ClientIds = lists:seq(1, ClientCount),
  {Completed, Started, _Busy, MonitorToClient} =
    run_paced_call_ticks(
      1,
      Ticks,
      StartedAt,
      Deadline,
      ClientIds,
      #{},
      #{},
      0,
      0,
      Path,
      Node,
      Payload,
      CallTarget),
  DrainDeadline = erlang:monotonic_time(millisecond) + DrainMs,
  {Completed1, MonitorToClient1} =
    collect_pending_calls_until(MonitorToClient, Completed, DrainDeadline),
  cleanup_call_monitors(MonitorToClient1),
  {Completed1, Started}.

run_paced_call_ticks(
    Tick,
    Ticks,
    _StartedAt,
    Deadline,
    _ClientIds,
    Busy,
    MonitorToClient,
    Completed,
    Started,
    _Path,
    _Node,
    _Payload,
    _CallTarget) when Tick > Ticks ->
  sleep_until(Deadline),
  {Busy1, MonitorToClient1, Completed1} = collect_ready_calls(Busy, MonitorToClient, Completed),
  {Completed1, Started, Busy1, MonitorToClient1};
run_paced_call_ticks(
    Tick,
    Ticks,
    StartedAt,
    Deadline,
    ClientIds,
    Busy,
    MonitorToClient,
    Completed,
    Started,
    Path,
    Node,
    Payload,
    CallTarget) ->
  sleep_until(StartedAt + ((Tick - 1) * 100)),
  {Busy1, MonitorToClient1, Completed1} = collect_ready_calls(Busy, MonitorToClient, Completed),
  {Busy2, MonitorToClient2, Started1} =
    start_idle_calls(ClientIds, Busy1, MonitorToClient1, Started, Path, Node, Payload, CallTarget),
  run_paced_call_ticks(
    Tick + 1,
    Ticks,
    StartedAt,
    Deadline,
    ClientIds,
    Busy2,
    MonitorToClient2,
    Completed1,
    Started1,
    Path,
    Node,
    Payload,
    CallTarget).

start_idle_calls([], Busy, MonitorToClient, Started, _Path, _Node, _Payload, _CallTarget) ->
  {Busy, MonitorToClient, Started};
start_idle_calls([ClientId | ClientIds], Busy, MonitorToClient, Started, Path, Node, Payload, CallTarget) ->
  case maps:is_key(ClientId, Busy) of
    true ->
      start_idle_calls(ClientIds, Busy, MonitorToClient, Started, Path, Node, Payload, CallTarget);
    false ->
      {_Pid, MonitorRef} =
        spawn_monitor(fun() ->
          _ = paced_call_operation(Path, Node, Payload, CallTarget),
          exit({completed, true})
        end),
      start_idle_calls(
        ClientIds,
        Busy#{ClientId => MonitorRef},
        MonitorToClient#{MonitorRef => ClientId},
        Started + 1,
        Path,
        Node,
        Payload,
        CallTarget)
  end.

collect_ready_calls(Busy, MonitorToClient, Completed) ->
  receive
    {'DOWN', MonitorRef, process, _Pid, {completed, true}} ->
      case maps:take(MonitorRef, MonitorToClient) of
        {ClientId, MonitorToClient1} ->
          collect_ready_calls(maps:remove(ClientId, Busy), MonitorToClient1, Completed + 1);
        error ->
          collect_ready_calls(Busy, MonitorToClient, Completed)
      end;
    {'DOWN', MonitorRef, process, _Pid, Reason} ->
      case maps:is_key(MonitorRef, MonitorToClient) of
        true ->
          exit({paced_call_failed, Reason});
        false ->
          collect_ready_calls(Busy, MonitorToClient, Completed)
      end
  after 0 ->
    {Busy, MonitorToClient, Completed}
  end.

collect_pending_calls_until(MonitorToClient, Completed, _Deadline) when map_size(MonitorToClient) =:= 0 ->
  {Completed, MonitorToClient};
collect_pending_calls_until(MonitorToClient, Completed, Deadline) ->
  Timeout = max(Deadline - erlang:monotonic_time(millisecond), 0),
  receive
    {'DOWN', MonitorRef, process, _Pid, {completed, true}} ->
      collect_pending_calls_until(maps:remove(MonitorRef, MonitorToClient), Completed + 1, Deadline);
    {'DOWN', MonitorRef, process, _Pid, Reason} ->
      case maps:is_key(MonitorRef, MonitorToClient) of
        true ->
          exit({paced_call_failed, Reason});
        false ->
          collect_pending_calls_until(MonitorToClient, Completed, Deadline)
      end
  after Timeout ->
    {Completed, MonitorToClient}
  end.

paced_call_operation(erpc_call, Node, Payload, {Module, Function, ExtraArgs}) ->
  ok = erpc:call(Node, Module, Function, [Payload | ExtraArgs]),
  true;
paced_call_operation(ecall_call, Node, Payload, {Module, Function, ExtraArgs}) ->
  {ok, ok} = ecall:call(Node, Module, Function, [Payload | ExtraArgs]),
  true.

cleanup_call_monitors(MonitorToClient) ->
  [erlang:demonitor(MonitorRef, [flush]) || MonitorRef <- maps:keys(MonitorToClient)],
  ok.

sleep_ms(0) ->
  ok;
sleep_ms(Ms) ->
  timer:sleep(Ms).

sleep_until(TargetMs) ->
  Now = erlang:monotonic_time(millisecond),
  case TargetMs - Now of
    Remaining when Remaining > 0 ->
      timer:sleep(Remaining);
    _ ->
      ok
  end.

data_archive() ->
  #{
    field1 => value1,
    field2 => value2,
    field3 => value3,
    field4 => value4,
    field5 => value5,
    field6 => value6,
    field7 => value7,
    field8 => value8,
    field9 => value9,
    field10 => value10
  }.

quota_extra(Index, Remainder) when Index =< Remainder ->
  1;
quota_extra(_Index, _Remainder) ->
  0.

indexes(Count) ->
  lists:seq(1, Count).

receiver_counter_loop(Count, TopLevelMessages, Waiters) ->
  receive
    {performance_load_stop, From, Ref} ->
      From ! {performance_load_counter, Ref, Count};
    {performance_load_snapshot, From, Ref} ->
      From ! {performance_load_count, Ref, Count},
      receiver_counter_loop(Count, TopLevelMessages, Waiters);
    {performance_load_stats, From, Ref} ->
      From ! {performance_load_stats, Ref, #{
        delivered => Count,
        top_level_messages => TopLevelMessages
      }},
      receiver_counter_loop(Count, TopLevelMessages, Waiters);
    {performance_load_wait, From, Ref, Expected} when Count >= Expected ->
      From ! {performance_load_count, Ref, Count},
      receiver_counter_loop(Count, TopLevelMessages, Waiters);
    {performance_load_wait, From, Ref, Expected} ->
      receiver_counter_loop(Count, TopLevelMessages, [{From, Ref, Expected} | Waiters]);
    {performance_load_batch, Payloads} ->
      Count1 = Count + length(Payloads),
      receiver_counter_loop(Count1, TopLevelMessages + 1, notify_ready_waiters(Count1, Waiters, []));
    _Message ->
      Count1 = Count + 1,
      receiver_counter_loop(Count1, TopLevelMessages + 1, notify_ready_waiters(Count1, Waiters, []))
  end.

notify_ready_waiters(_Count, [], Pending) ->
  Pending;
notify_ready_waiters(Count, [{From, Ref, Expected} | Waiters], Pending) when Count >= Expected ->
  From ! {performance_load_count, Ref, Count},
  notify_ready_waiters(Count, Waiters, Pending);
notify_ready_waiters(Count, [Waiter | Waiters], Pending) ->
  notify_ready_waiters(Count, Waiters, [Waiter | Pending]).

repeat(0, _Fun) ->
  ok;
repeat(Count, Fun) ->
  _ = Fun(),
  repeat(Count - 1, Fun).

repeat_count(0, _Fun) ->
  0;
repeat_count(Count, Fun) ->
  case Fun() of
    true ->
      1 + repeat_count(Count - 1, Fun)
  end.

wait_writer_ready(0) ->
  ok;
wait_writer_ready(Count) ->
  receive
    {performance_load_writer_ready, _Pid} ->
      wait_writer_ready(Count - 1)
  end.

wait_writer_downs([]) ->
  ok;
wait_writer_downs(MonitorRefs) ->
  receive
    {'DOWN', MonitorRef, process, _Pid, normal} ->
      wait_writer_downs(lists:delete(MonitorRef, MonitorRefs));
    {'DOWN', _MonitorRef, process, _Pid, Reason} ->
      exit({fixed_work_writer_failed, Reason})
  end.

proxy_loop(Receiver, BatchSize, Buffer, FullBatches, PartialBatches, QueueMax) ->
  receive
    {performance_load_proxy_send, Payload} ->
      Buffer1 = [Payload | Buffer],
      QueueMax1 = max(QueueMax, proxy_mailbox_depth()),
      case length(Buffer1) of
        BatchSize ->
          flush_proxy_batch(Receiver, Buffer1),
          proxy_loop(Receiver, BatchSize, [], FullBatches + 1, PartialBatches, QueueMax1);
        _ ->
          proxy_loop(Receiver, BatchSize, Buffer1, FullBatches, PartialBatches, QueueMax1)
      end;
    {performance_load_proxy_stop, From, Ref} ->
      PartialBatches1 =
        case Buffer of
          [] ->
            PartialBatches;
          _ ->
            flush_proxy_batch(Receiver, Buffer),
            PartialBatches + 1
        end,
      From ! {performance_load_proxy_metrics, Ref, #{
        full_batches => FullBatches,
        partial_batches => PartialBatches1,
        proxy_queue_max => QueueMax,
        top_level_sends => FullBatches + PartialBatches1
      }}
  end.

flush_proxy_batch(Receiver, Buffer) ->
  Receiver ! {performance_load_batch, lists:reverse(Buffer)},
  ok.

proxy_mailbox_depth() ->
  {message_queue_len, Depth} = process_info(self(), message_queue_len),
  Depth.

stop_proxy(Proxy, Timeout) ->
  Ref = make_ref(),
  Proxy ! {performance_load_proxy_stop, self(), Ref},
  receive
    {performance_load_proxy_metrics, Ref, Metrics} ->
      Metrics
  after Timeout ->
    exit({performance_proxy_timeout, Proxy})
  end.

start_proxy_pool(Receiver, BatchSize, ProxyPoolSize) ->
  [
    spawn(fun() -> proxy_loop(Receiver, BatchSize, [], 0, 0, 0) end)
    || _Index <- lists:seq(1, ProxyPoolSize)
  ].

run_proxy_pool_paced_clients(ClientCount, Ticks, Proxies, Payload) ->
  MonitorRefs =
    [
      element(2, spawn_monitor(fun() ->
        Proxy = proxy_for_client(ClientIndex, Proxies),
        exit({completed, run_proxy_pool_paced_client(Ticks, Proxy, Payload)})
      end))
      || ClientIndex <- lists:seq(1, ClientCount)
    ],
  collect_paced_clients(MonitorRefs, 0).

run_proxy_pool_paced_client(Ticks, Proxy, Payload) ->
  run_proxy_pool_paced_client(1, Ticks, Proxy, Payload, 0).

run_proxy_pool_paced_client(Tick, Ticks, _Proxy, _Payload, Completed) when Tick > Ticks ->
  Completed;
run_proxy_pool_paced_client(Tick, Ticks, Proxy, Payload, Completed) ->
  TickStartedAt = erlang:monotonic_time(millisecond),
  Proxy ! {performance_load_proxy_send, Payload},
  Completed1 = Completed + 1,
  case Tick < Ticks of
    true ->
      sleep_until(TickStartedAt + 100),
      run_proxy_pool_paced_client(Tick + 1, Ticks, Proxy, Payload, Completed1);
    false ->
      Completed1
  end.

proxy_for_client(ClientIndex, Proxies) ->
  lists:nth(((ClientIndex - 1) rem length(Proxies)) + 1, Proxies).

stop_proxy_pool(Proxies, Timeout) ->
  merge_proxy_metrics([stop_proxy(Proxy, Timeout) || Proxy <- Proxies]).

merge_proxy_metrics(Metrics) ->
  #{
    full_batches => sum_metric(full_batches, Metrics),
    partial_batches => sum_metric(partial_batches, Metrics),
    proxy_queue_max => max_metric(proxy_queue_max, Metrics),
    top_level_sends => sum_metric(top_level_sends, Metrics)
  }.

sum_metric(Key, Metrics) ->
  lists:sum([maps:get(Key, Metric, 0) || Metric <- Metrics]).

max_metric(Key, Metrics) ->
  lists:max([maps:get(Key, Metric, 0) || Metric <- Metrics]).

e3_call_client(ClientId, CallsPerClient, CallFun) ->
  e3_call_client(ClientId, 1, CallsPerClient, CallFun).

e3_call_client(_ClientId, Sequence, CallsPerClient, _CallFun) when Sequence > CallsPerClient ->
  CallsPerClient;
e3_call_client(ClientId, Sequence, CallsPerClient, CallFun) ->
  _ = CallFun(ClientId, Sequence),
  e3_call_client(ClientId, Sequence + 1, CallsPerClient, CallFun).

collect_e3_clients([], Completed) ->
  Completed;
collect_e3_clients(MonitorRefs, Completed) ->
  receive
    {'DOWN', MonitorRef, process, _Pid, {completed, Count}} ->
      collect_e3_clients(lists:delete(MonitorRef, MonitorRefs), Completed + Count);
    {'DOWN', _MonitorRef, process, _Pid, Reason} ->
      exit({e3_call_client_failed, Reason})
  end.
