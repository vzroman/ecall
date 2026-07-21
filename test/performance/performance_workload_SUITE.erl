-module(performance_workload_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  h1_raw_send_curve/1,
  h2_fixed_work_writer_contention/1,
  h3_sender_pool_batching/1,
  h5_busy_limit_sweep/1,
  h10a_payload_cost/1,
  h10b_receiver_work/1,
  e1_raw_vs_ecall_send/1,
  e2_erpc_vs_ecall_cast/1,
  e3_erpc_vs_ecall_call/1,
  sender_role/2,
  receiver_role/2,
  ensure_ecall_runtime/0,
  ensure_ecall_connection/1,
  ecall_connection_state/1,
  cast_target/2,
  call_target/1,
  slow_call_target/2,
  receiver_work_target/3
]).

-ifdef(TEST).
-export([
  report_row_from_result/7
]).
-endif.

-define(RPC_TIMEOUT, 30000).
-define(DEFAULT_ROLE_TIMEOUT_MS, 300000).
-define(WORKLOAD_CASES, [
  h1_raw_send_curve,
  h2_fixed_work_writer_contention,
  h3_sender_pool_batching,
  h5_busy_limit_sweep,
  h10a_payload_cost,
  h10b_receiver_work,
  e1_raw_vs_ecall_send,
  e2_erpc_vs_ecall_cast,
  e3_erpc_vs_ecall_call
]).
-define(BUSY_LIMITS, [
  {"256 KiB", 256 * 1024},
  {"1 MiB", 1024 * 1024},
  {"8 MiB", 8 * 1024 * 1024}
]).
-define(H10_PAYLOADS, [tiny, data, binary_100k, binary_1m]).
-define(RECEIVER_WORKS, [count_only, traverse_payload, spawn_process]).
-define(H3_BATCH_SIZES, [1, 10, 100, 1000, 10000]).

all() ->
  ?WORKLOAD_CASES.

init_per_suite(Config0) ->
  RoleConfig = distributed_tests_util:role_config(Config0),
  SenderNode = distributed_tests_util:start_node(sender, RoleConfig),
  ReceiverNode = distributed_tests_util:start_node(receiver, RoleConfig),
  ok = distributed_tests_util:connect(SenderNode, ReceiverNode),
  ok = ensure_ecall_path(SenderNode, ReceiverNode),
  [
    {role_config, distributed_tests_util:public_role_config(RoleConfig)},
    {sender, SenderNode},
    {receiver, ReceiverNode}
    | Config0
  ].

end_per_suite(_Config) ->
  distributed_tests_util:stop_all().

h1_raw_send_curve(Config) ->
  Rows = run_paced_case(Config, h1_raw_send_curve, [raw_send], #{}),
  write_case_report(Config, h1_raw_send_curve, Rows).

h2_fixed_work_writer_contention(Config) ->
  RunConfig = performance_config:from_ct(Config),
  WorkTotal = maps:get(fixed_work_total, RunConfig, 1000000),
  Payload = performance_load:payload(data),
  Rows =
    [
      run_fixed_work_point(Config, RunConfig, Writers, Repetition, WorkTotal, Payload)
      || Repetition <- repetitions(RunConfig),
         Writers <- client_counts(RunConfig)
    ],
  write_case_report(Config, h2_fixed_work_writer_contention, Rows).

h3_sender_pool_batching(Config) ->
  RunConfig = performance_config:from_ct(Config),
  Payload = performance_load:payload(data),
  Rows =
    lists:append(
      [
        run_batching_points(Config, RunConfig, ClientCount, Repetition, Payload)
        || Repetition <- repetitions(RunConfig),
           ClientCount <- client_counts(RunConfig)
      ]),
  write_case_report(Config, h3_sender_pool_batching, Rows).

h5_busy_limit_sweep(Config) ->
  Rows =
    lists:append(
      [
        run_paced_case(
          Config,
          h5_busy_limit_sweep,
          [raw_send],
          #{
            client_counts => selected_client_counts(Config),
            requested_busy_limit => RequestedBusyLimit,
            effective_busy_limit => "n/a"
          })
        || {RequestedBusyLimit, _Bytes} <- ?BUSY_LIMITS
      ]),
  write_case_report(Config, h5_busy_limit_sweep, Rows).

h10a_payload_cost(Config) ->
  Rows =
    lists:append(
      [
        run_paced_case(
          Config,
          h10a_payload_cost,
          [raw_send],
          #{
            client_counts => selected_client_counts(Config),
            payload => performance_load:payload(PayloadName),
            payload_name => PayloadName
          })
        || PayloadName <- ?H10_PAYLOADS
      ]),
  write_case_report(Config, h10a_payload_cost, Rows).

h10b_receiver_work(Config) ->
  Rows =
    lists:append(
      [
        run_paced_case(
          Config,
          h10b_receiver_work,
          [raw_send],
          #{
            client_counts => selected_client_counts(Config),
            receiver_work => ReceiverWork
          })
        || ReceiverWork <- ?RECEIVER_WORKS
      ]),
  write_case_report(Config, h10b_receiver_work, Rows).

e1_raw_vs_ecall_send(Config) ->
  Rows = run_comparison_case(Config, e1_raw_vs_ecall_send, raw_send, ecall_send),
  write_case_report(Config, e1_raw_vs_ecall_send, Rows).

e2_erpc_vs_ecall_cast(Config) ->
  Rows = run_comparison_case(Config, e2_erpc_vs_ecall_cast, erpc_cast, ecall_cast),
  write_case_report(Config, e2_erpc_vs_ecall_cast, Rows).

e3_erpc_vs_ecall_call(Config) ->
  Rows = run_comparison_case(Config, e3_erpc_vs_ecall_call, erpc_call, ecall_call),
  write_case_report(Config, e3_erpc_vs_ecall_call, Rows).

sender_role(Controller, RoleConfig) ->
  Controller ! {self(), ready},
  receive
    start ->
      Result = run_sender_role(RoleConfig),
      Controller ! {self(), Result}
  after maps:get(timeout_ms, RoleConfig, ?DEFAULT_ROLE_TIMEOUT_MS) ->
      exit(sender_role_timeout)
  end.

receiver_role(Controller, #{mode := counter} = RoleConfig) ->
  Timeout = maps:get(timeout_ms, RoleConfig, ?DEFAULT_ROLE_TIMEOUT_MS),
  Receiver = start_receiver(RoleConfig),
  Controller ! {self(), {ready, #{receiver => Receiver}}},
  receive
    stop ->
      Stats = performance_load:receiver_stats(Receiver, Timeout),
      Delivered = performance_load:stop_receiver_counter(Receiver, Timeout),
      Controller ! {self(), Stats#{role => receiver, delivered => Delivered}}
  after Timeout ->
      exit(receiver_role_timeout)
  end;

receiver_role(Controller, RoleConfig) ->
  Controller ! {self(), ready},
  receive
    start ->
      Receiver = maps:get(receiver, RoleConfig),
      Expected = maps:get(expected, RoleConfig),
      Timeout = maps:get(timeout_ms, RoleConfig, 10000),
      Delivered = performance_load:await_receiver_count(Receiver, Expected, Timeout),
      Controller ! {self(), #{role => receiver, delivered => Delivered}}
  after maps:get(timeout_ms, RoleConfig, ?DEFAULT_ROLE_TIMEOUT_MS) ->
      exit(receiver_role_timeout)
  end.

cast_target(Receiver, Payload) ->
  Receiver ! Payload,
  ok.

call_target(_Payload) ->
  ok.

slow_call_target(_Payload, DelayMs) ->
  timer:sleep(DelayMs),
  ok.

receiver_work_target(Receiver, count_only, Payload) ->
  Receiver ! Payload,
  ok;
receiver_work_target(Receiver, traverse_payload, Payload) ->
  _ = traverse_payload(Payload),
  Receiver ! Payload,
  ok;
receiver_work_target(Receiver, spawn_process, Payload) ->
  Parent = self(),
  Worker =
    spawn(fun() ->
      Receiver ! Payload,
      Parent ! {self(), done}
    end),
  receive
    {Worker, done} ->
      ok
  end.

traverse_payload(Payload) when is_map(Payload) ->
  maps:fold(
    fun(_Key, Value, Acc) ->
      Acc + traverse_payload(Value)
    end,
    0,
    Payload);
traverse_payload(Payload) when is_list(Payload) ->
  lists:foldl(
    fun(Value, Acc) ->
      Acc + traverse_payload(Value)
    end,
    0,
    Payload);
traverse_payload(Payload) when is_binary(Payload) ->
  byte_size(Payload);
traverse_payload(_Payload) ->
  1.

run_sender_role(#{mode := paced} = RoleConfig) ->
  performance_load:run_paced_workload(maps:remove(mode, RoleConfig));
run_sender_role(#{path := fixed_work_raw_send} = RoleConfig) ->
  performance_load:run_fixed_work_raw_send(
    maps:get(receiver, RoleConfig),
    maps:get(work_total, RoleConfig),
    maps:get(writers, RoleConfig),
    maps:get(payload, RoleConfig),
    maps:get(timeout_ms, RoleConfig, 10000));
run_sender_role(#{path := proxy_batching} = RoleConfig) ->
  performance_load:run_proxy_batching(
    maps:get(receiver, RoleConfig),
    maps:get(count, RoleConfig),
    maps:get(payload, RoleConfig),
    maps:get(batch_size, RoleConfig),
    maps:get(timeout_ms, RoleConfig, 10000));
run_sender_role(#{path := proxy_pool_batching} = RoleConfig) ->
  performance_load:run_proxy_pool_paced_workload(
    maps:remove(path, RoleConfig));
run_sender_role(#{path := Path, count := Count} = RoleConfig)
  when Path =:= raw_send; Path =:= ecall_send ->
  performance_load:run_send(
    Path,
    maps:get(receiver, RoleConfig),
    Count,
    maps:get(payload, RoleConfig),
    maps:get(timeout_ms, RoleConfig, 10000));
run_sender_role(#{path := Path, count := Count} = RoleConfig)
  when Path =:= erpc_cast; Path =:= ecall_cast ->
  performance_load:run_cast(
    Path,
    maps:get(node, RoleConfig, node()),
    maps:get(receiver, RoleConfig),
    Count,
    maps:get(payload, RoleConfig),
    maps:get(timeout_ms, RoleConfig, 10000));
run_sender_role(#{path := Path, count := Count} = RoleConfig)
  when Path =:= erpc_call; Path =:= ecall_call ->
  performance_load:run_call(
    Path,
    maps:get(node, RoleConfig, node()),
    Count,
    maps:get(payload, RoleConfig),
    maps:get(timeout_ms, RoleConfig, 10000)).

ensure_ecall_path(SenderNode, ReceiverNode) ->
  ok = ensure_ecall_started(SenderNode),
  ok = ensure_ecall_started(ReceiverNode),
  ok = ensure_fresh_ecall_path(SenderNode, ReceiverNode).

ensure_ecall_started(Node) ->
  case rpc:call(Node, ?MODULE, ensure_ecall_runtime, [], ?RPC_TIMEOUT) of
    ok ->
      ok;
    Other ->
      ct:fail({cannot_start_ecall, Node, Other})
  end.

ensure_ecall_runtime() ->
  case ensure_pg_scope() of
    ok ->
      case ensure_receive_pool() of
        ok ->
          ensure_connection_supervisor();
        Error ->
          Error
      end;
    Error ->
      Error
  end.

ensure_pg_scope() ->
  case pg:start_link(ecall) of
    {ok, Pid} ->
      unlink(Pid),
      ok;
    {error, {already_started, _Pid}} ->
      ok;
    Other ->
      {error, {cannot_start_pg, Other}}
  end.

ensure_receive_pool() ->
  case ecall_receive:start_link() of
    {ok, Pid} ->
      unlink(Pid),
      ok;
    {error, {already_started, _Pid}} ->
      ok;
    Other ->
      {error, {cannot_start_ecall_receive, Other}}
  end.

ensure_connection_supervisor() ->
  case ecall_connection_sup:start_link() of
    {ok, Pid} ->
      unlink(Pid),
      ok;
    {error, {already_started, _Pid}} ->
      ok;
    Other ->
      {error, {cannot_start_ecall_connection_sup, Other}}
  end.

ensure_fresh_ecall_path(SenderNode, ReceiverNode) ->
  ok = ensure_remote_ecall_connection(SenderNode, ReceiverNode),
  ok = ensure_remote_ecall_connection(ReceiverNode, SenderNode).

ensure_remote_ecall_connection(FromNode, ToNode) ->
  case rpc:call(FromNode, ?MODULE, ensure_ecall_connection, [ToNode], ?RPC_TIMEOUT) of
    {ok, _State} ->
      ok;
    Other ->
      ct:fail({cannot_refresh_ecall_connection, FromNode, ToNode, Other})
  end.

ensure_ecall_connection(ToNode) ->
  ok = ensure_ecall_runtime(),
  ensure_ecall_connection(ToNode, _Attempts = 2).

ensure_ecall_connection(ToNode, Attempts) when Attempts > 0 ->
  catch ecall_connection:disconnect(ToNode),
  timer:sleep(50),
  case ecall_connection:connect(ToNode) of
    ok ->
      wait_ecall_connection(ToNode, 5000);
    Other ->
      case Attempts of
        1 ->
          {error, {connect_failed, Other, ecall_connection_state(ToNode)}};
        _ ->
          ensure_ecall_connection(ToNode, Attempts - 1)
      end
  end.

wait_ecall_connection(ToNode, TimeoutMs) ->
  Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
  wait_ecall_connection_until(ToNode, Deadline).

wait_ecall_connection_until(ToNode, Deadline) ->
  State = ecall_connection_state(ToNode),
  case maps:get(connected, State) of
    true ->
      {ok, State};
    false ->
      case erlang:monotonic_time(millisecond) >= Deadline of
        true ->
          {error, {connection_not_ready, State}};
        false ->
          timer:sleep(50),
          wait_ecall_connection_until(ToNode, Deadline)
      end
  end.

ecall_connection_state(ToNode) ->
  Connections = persistent_term:get(ecall_connection, #{}),
  case maps:get(ToNode, Connections, undefined) of
    {connection, ToNode, Master, Pool, Size} when is_map(Pool) ->
      MasterAlive = erlang:is_process_alive(Master),
      PoolAlive = lists:all(fun erlang:is_process_alive/1, maps:values(Pool)),
      #{
        connected => MasterAlive andalso PoolAlive andalso map_size(Pool) > 0,
        master_alive => MasterAlive,
        pool_alive => PoolAlive,
        pool_size => map_size(Pool),
        configured_size => Size
      };
    undefined ->
      #{connected => false, reason => missing};
    Other ->
      #{connected => false, reason => {unexpected_connection_state, Other}}
  end.

run_paced_case(Config, Case, Paths, Options) ->
  RunConfig = performance_config:from_ct(Config),
  Counts = maps:get(client_counts, Options, client_counts(RunConfig)),
  [
    run_paced_point(Config, RunConfig, Case, Path, ClientCount, Repetition, Options)
    || Repetition <- repetitions(RunConfig),
       ClientCount <- Counts,
       Path <- Paths
  ].

run_paced_point(Config, RunConfig, Case, Path, ClientCount, Repetition, Options) ->
  ok = maybe_refresh_ecall_path(Config, Path),
  PayloadName = maps:get(payload_name, Options, data),
  Payload = maps:get(payload, Options, performance_load:payload(PayloadName)),
  Timeout = role_timeout_ms(RunConfig),
  ReceiverOptions =
    #{
      mode => counter,
      timeout_ms => Timeout,
      receiver_work => maps:get(receiver_work, Options, count_only)
    },
  SenderConfigFun =
    fun(Receiver) ->
      #{
        mode => paced,
        path => Path,
        receiver => Receiver,
        node => ?config(receiver, Config),
        client_count => ClientCount,
        payload => Payload,
        warmup_ms => maps:get(warmup_ms, RunConfig),
        measurement_ms => maps:get(measurement_ms, RunConfig),
        drain_ms => maps:get(drain_ms, RunConfig),
        timeout_ms => Timeout
      }
    end,
  {SenderResult, ReceiverResult} = run_role_point(Config, ReceiverOptions, SenderConfigFun),
  report_row_from_result(
    Case,
    Path,
    RunConfig,
    Repetition,
    SenderResult,
    ReceiverResult,
    maps:merge(
      #{
        client_count => ClientCount,
        payload => PayloadName,
        receiver_work => maps:get(receiver_work, Options, "n/a"),
        requested_busy_limit => maps:get(requested_busy_limit, Options, "n/a"),
        effective_busy_limit => maps:get(effective_busy_limit, Options, "n/a")
      },
      comparison_fields(Options))).

run_fixed_work_point(Config, RunConfig, Writers, Repetition, WorkTotal, Payload) ->
  Timeout = role_timeout_ms(RunConfig),
  SenderConfigFun =
    fun(Receiver) ->
      #{
        path => fixed_work_raw_send,
        receiver => Receiver,
        writers => Writers,
        work_total => WorkTotal,
        payload => Payload,
        timeout_ms => Timeout
      }
    end,
  {SenderResult, ReceiverResult} =
    run_role_point(Config, #{mode => counter, timeout_ms => Timeout}, SenderConfigFun),
  CompletionMs = maps:get(completion_ms, SenderResult),
  Delivered = maps:get(delivered, SenderResult),
  report_row_from_result(
    h2_fixed_work_writer_contention,
    fixed_work_raw_send,
    RunConfig,
    Repetition,
    SenderResult#{
      measurement_ms => max(CompletionMs, 1),
      delivered_per_second => per_second(Delivered, max(CompletionMs, 1)),
      delivery_ratio => safe_divide(Delivered, WorkTotal)
    },
    ReceiverResult,
    #{
      writers => Writers,
      scheduled => WorkTotal,
      started => WorkTotal,
      completed => Delivered,
      payload => data
    }).

run_batching_points(Config, RunConfig, ClientCount, Repetition, Payload) ->
  DirectRow = run_paced_point(Config, RunConfig, h3_sender_pool_batching, raw_send, ClientCount, Repetition, #{}),
  BatchRows =
    [
      run_proxy_pool_batching_point(Config, RunConfig, ClientCount, Repetition, Payload, BatchSize)
      || BatchSize <- ?H3_BATCH_SIZES
    ],
  [DirectRow | BatchRows].

run_proxy_pool_batching_point(Config, RunConfig, ClientCount, Repetition, Payload, BatchSize) ->
  Timeout = role_timeout_ms(RunConfig),
  ProxyPoolSize = maps:get(h3_proxy_pool_size, RunConfig, 4),
  SenderConfigFun =
    fun(Receiver) ->
      #{
        path => proxy_pool_batching,
        receiver => Receiver,
        client_count => ClientCount,
        payload => Payload,
        batch_size => BatchSize,
        proxy_pool_size => ProxyPoolSize,
        warmup_ms => maps:get(warmup_ms, RunConfig),
        measurement_ms => maps:get(measurement_ms, RunConfig),
        drain_ms => maps:get(drain_ms, RunConfig),
        timeout_ms => Timeout
      }
    end,
  {SenderResult, ReceiverResult} =
    run_role_point(Config, #{mode => counter, timeout_ms => Timeout}, SenderConfigFun),
  report_row_from_result(
    h3_sender_pool_batching,
    proxy_batching,
    RunConfig,
    Repetition,
    SenderResult,
    ReceiverResult,
    #{
      client_count => ClientCount,
      batch_size => BatchSize,
      proxy_pool_size => ProxyPoolSize,
      payload => data,
      scheduled => maps:get(scheduled, SenderResult),
      started => maps:get(started, SenderResult),
      completed => maps:get(delivered, SenderResult),
      top_level_sends => maps:get(top_level_sends, SenderResult, "n/a")
    }).

run_comparison_case(Config, Case, BaselinePath, EcallPath) ->
  RunConfig = performance_config:from_ct(Config),
  lists:append(
    [
      run_comparison_point(Config, RunConfig, Case, BaselinePath, EcallPath, ClientCount, Repetition)
      || Repetition <- repetitions(RunConfig),
         ClientCount <- client_counts(RunConfig)
    ]).

run_comparison_point(Config, RunConfig, Case, BaselinePath, EcallPath, ClientCount, Repetition) ->
  Group = comparison_group(Case, ClientCount, Repetition),
  BaselineRow0 =
    run_paced_point(
      Config,
      RunConfig,
      Case,
      BaselinePath,
      ClientCount,
      Repetition,
      #{
        comparison_group => Group,
        comparison_path => baseline
      }),
  EcallRow0 =
    run_paced_point(
      Config,
      RunConfig,
      Case,
      EcallPath,
      ClientCount,
      Repetition,
      #{
        comparison_group => Group,
        comparison_path => ecall
      }),
  BaselineThroughput = row_throughput(BaselineRow0),
  EcallThroughput = row_throughput(EcallRow0),
  [
    BaselineRow0#{throughput_ratio => 1.0},
    EcallRow0#{throughput_ratio => safe_divide(EcallThroughput, BaselineThroughput)}
  ].

run_role_point(Config, ReceiverConfig, SenderConfigFun) ->
  Controller = self(),
  ReceiverNode = ?config(receiver, Config),
  SenderNode = ?config(sender, Config),
  {ReceiverRole, ReceiverMon} =
    spawn_monitor(ReceiverNode, ?MODULE, receiver_role, [Controller, ReceiverConfig]),
  ReceiverInfo = wait_ready(ReceiverRole, ReceiverMon),
  Receiver = maps:get(receiver, ReceiverInfo),
  SenderConfig = SenderConfigFun(Receiver),
  {SenderRole, SenderMon} =
    spawn_monitor(SenderNode, ?MODULE, sender_role, [Controller, SenderConfig]),
  _ = wait_ready(SenderRole, SenderMon),
  SenderRole ! start,
  SenderResult = wait_result(SenderRole, SenderMon),
  wait_down(SenderRole, SenderMon),
  ReceiverRole ! stop,
  ReceiverResult = wait_result(ReceiverRole, ReceiverMon),
  wait_down(ReceiverRole, ReceiverMon),
  {SenderResult, ReceiverResult}.

report_row_from_result(Case, Path, RunConfig, Repetition, SenderResult, ReceiverResult, Extra) ->
  MeasurementMs =
    maps:get(measurement_ms, SenderResult, maps:get(measurement_ms, RunConfig)),
  Delivered = result_delivered(Path, SenderResult),
  Base =
    #{
      'case' => Case,
      path => Path,
      role => receiver,
      warmup_ms => maps:get(warmup_ms, RunConfig),
      measurement_ms => MeasurementMs,
      drain_ms => maps:get(drain_ms, RunConfig),
      repetition => Repetition,
      offered_per_second => maps:get(offered_per_second, SenderResult, offered_per_second(Extra)),
      delivered => Delivered,
      completion_ms => maps:get(completion_ms, SenderResult, "n/a"),
      emulator => normal,
      node_role => receiver,
      scheduled => maps:get(scheduled, SenderResult, maps:get(scheduled, Extra, "n/a")),
      started => maps:get(started, SenderResult, maps:get(started, Extra, "n/a")),
      completed => maps:get(completed, SenderResult, maps:get(completed, Extra, "n/a")),
      elapsed_ms => maps:get(elapsed_ms, SenderResult, "n/a"),
      top_level_messages => maps:get(top_level_messages, ReceiverResult, "n/a"),
      metrics => metrics_from_result(SenderResult)
    },
  maps:merge(Base, Extra).

maybe_refresh_ecall_path(Config, Path)
  when Path =:= ecall_send; Path =:= ecall_cast; Path =:= ecall_call ->
  ensure_fresh_ecall_path(?config(sender, Config), ?config(receiver, Config));
maybe_refresh_ecall_path(_Config, _Path) ->
  ok.

metrics_from_result(Result) ->
  maps:with([full_batches, partial_batches, proxy_queue_max], Result).

result_delivered(Path, Result) when Path =:= erpc_call; Path =:= ecall_call ->
  maps:get(completed, Result);
result_delivered(_Path, Result) ->
  maps:get(delivered, Result).

offered_per_second(#{client_count := ClientCount}) ->
  performance_load:offered_per_second(ClientCount);
offered_per_second(#{scheduled := Scheduled, measurement_ms := MeasurementMs}) ->
  per_second(Scheduled, MeasurementMs);
offered_per_second(_Extra) ->
  0.

comparison_fields(#{comparison_group := Group, comparison_path := Path}) ->
  #{comparison_group => Group, comparison_path => Path};
comparison_fields(_Options) ->
  #{}.

row_throughput(#{delivered_per_second := DeliveredPerSecond}) ->
  DeliveredPerSecond;
row_throughput(#{delivered := Delivered, measurement_ms := MeasurementMs}) ->
  per_second(Delivered, MeasurementMs).

comparison_group(Case, ClientCount, Repetition) ->
  lists:flatten(io_lib:format("~p_~p_~p", [Case, ClientCount, Repetition])).

write_case_report(Config, Case, Rows) ->
  Path = filename:join(?config(priv_dir, Config), atom_to_list(Case) ++ ".csv"),
  ok = performance_report:write_csv(Path, Rows),
  ct:pal("wrote ~p rows to ~s", [length(Rows), Path]).

client_counts(RunConfig) ->
  maps:get(common_client_counts, RunConfig).

selected_client_counts(Config) ->
  RunConfig = performance_config:from_ct(Config),
  case maps:get(selected_client_counts, RunConfig, undefined) of
    undefined ->
      [lists:last(client_counts(RunConfig))];
    Counts ->
      Counts
  end.

repetitions(RunConfig) ->
  Profile = maps:get(profile, RunConfig, smoke),
  performance_load:repetition_indexes(RunConfig, Profile).

role_timeout_ms(RunConfig) ->
  maps:get(timeout_ms, RunConfig, ?DEFAULT_ROLE_TIMEOUT_MS).

per_second(_Count, 0) ->
  0.0;
per_second(Count, Milliseconds) ->
  Count / (Milliseconds / 1000).

safe_divide(_Numerator, 0) ->
  "n/a";
safe_divide(Numerator, Denominator) ->
  Numerator / Denominator.

wait_ready(Pid, Mon) ->
  receive
    {Pid, ready} ->
      #{};
    {Pid, {ready, Info}} when is_map(Info) ->
      Info;
    {'DOWN', Mon, process, Pid, Reason} ->
      ct:fail({role_exited_before_ready, Pid, Reason})
  after ?DEFAULT_ROLE_TIMEOUT_MS ->
      ct:fail({ready_timeout, Pid})
  end.

wait_result(Pid, Mon) ->
  receive
    {Pid, Result} when is_map(Result) ->
      Result;
    {'DOWN', Mon, process, Pid, Reason} ->
      ct:fail({role_exited_before_result, Pid, Reason});
    {Pid, Other} ->
      ct:fail({unexpected_role_result, Pid, Other})
  after ?DEFAULT_ROLE_TIMEOUT_MS ->
      ct:fail({result_timeout, Pid})
  end.

wait_down(Pid, Mon) ->
  receive
    {'DOWN', Mon, process, Pid, normal} ->
      ok;
    {'DOWN', Mon, process, Pid, Reason} ->
      ct:fail({role_exited, Pid, Reason})
  after ?DEFAULT_ROLE_TIMEOUT_MS ->
      ct:fail({down_timeout, Pid})
  end.

start_receiver(#{receiver_work := count_only}) ->
  performance_load:start_receiver_counter();
start_receiver(#{receiver_work := Work}) ->
  spawn(fun() -> receiver_work_counter_loop(0, 0, [], Work) end);
start_receiver(_RoleConfig) ->
  performance_load:start_receiver_counter().

receiver_work_counter_loop(Count, TopLevelMessages, Waiters, Work) ->
  receive
    {performance_load_stop, From, Ref} ->
      From ! {performance_load_counter, Ref, Count};
    {performance_load_snapshot, From, Ref} ->
      From ! {performance_load_count, Ref, Count},
      receiver_work_counter_loop(Count, TopLevelMessages, Waiters, Work);
    {performance_load_stats, From, Ref} ->
      From ! {performance_load_stats, Ref, #{
        delivered => Count,
        top_level_messages => TopLevelMessages
      }},
      receiver_work_counter_loop(Count, TopLevelMessages, Waiters, Work);
    {performance_load_wait, From, Ref, Expected} when Count >= Expected ->
      From ! {performance_load_count, Ref, Count},
      receiver_work_counter_loop(Count, TopLevelMessages, Waiters, Work);
    {performance_load_wait, From, Ref, Expected} ->
      receiver_work_counter_loop(Count, TopLevelMessages, [{From, Ref, Expected} | Waiters], Work);
    {performance_load_batch, Payloads} ->
      [apply_receiver_work(Work, Payload) || Payload <- Payloads],
      Count1 = Count + length(Payloads),
      receiver_work_counter_loop(
        Count1,
        TopLevelMessages + 1,
        notify_ready_waiters(Count1, Waiters, []),
        Work);
    Payload ->
      apply_receiver_work(Work, Payload),
      Count1 = Count + 1,
      receiver_work_counter_loop(
        Count1,
        TopLevelMessages + 1,
        notify_ready_waiters(Count1, Waiters, []),
        Work)
  end.

apply_receiver_work(count_only, _Payload) ->
  ok;
apply_receiver_work(traverse_payload, Payload) ->
  _ = traverse_payload(Payload),
  ok;
apply_receiver_work(spawn_process, Payload) ->
  Parent = self(),
  Worker =
    spawn(fun() ->
      _ = Payload,
      Parent ! {self(), done}
    end),
  receive
    {Worker, done} ->
      ok
  end.

notify_ready_waiters(_Count, [], Pending) ->
  Pending;
notify_ready_waiters(Count, [{From, Ref, Expected} | Waiters], Pending) when Count >= Expected ->
  From ! {performance_load_count, Ref, Count},
  notify_ready_waiters(Count, Waiters, Pending);
notify_ready_waiters(Count, [Waiter | Waiters], Pending) ->
  notify_ready_waiters(Count, Waiters, [Waiter | Pending]).
