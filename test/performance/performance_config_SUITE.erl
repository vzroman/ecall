-module(performance_config_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0
]).

-export([
  default_run_controls/1,
  ct_performance_overrides/1,
  smoke_config_profile/1,
  report_row_normalization/1,
  csv_write_formats_header_escaping_and_na/1,
  unavailable_supportive_metrics_render_na/1,
  public_role_config_sanitizes_password/1,
  load_payloads/1,
  offered_rate_uses_common_cadence/1,
  fixed_work_quotas_preserve_total/1,
  repetition_indexes_use_run_profile/1,
  h2_h3_report_fields_are_preserved/1,
  local_raw_send_counts_receiver_deliveries/1,
  e3_call_pacing_limits_outstanding_per_client/1,
  path_helpers_confirm_send_cast_and_call_counts/1,
  fixed_work_runner_reports_completion/1,
  proxy_batching_reports_batch_metrics/1,
  proxy_batching_sends_batches_not_individual_messages/1,
  workload_target_functions_are_available/1,
  role_procedures_follow_ready_result_protocol/1,
  workload_suite_exposes_exact_cases/1,
  test_spec_includes_performance_suites/1,
  workload_h2_report_row_carries_completion_ms/1,
  proxy_pool_batching_uses_fixed_pool_and_paced_ticks/1,
  paced_workload_uses_100ms_ticks/1,
  paced_send_reports_partial_delivery_without_timeout/1,
  paced_slow_call_skips_ticks_while_client_busy/1
]).

-define(DEFAULT_CLIENT_COUNTS, [1, 1000, 10000, 100000, 500000, 1000000]).
-define(REQUIRED_COLUMNS, [
  'case',
  path,
  role,
  warmup_ms,
  measurement_ms,
  drain_ms,
  repetition,
  offered_per_second,
  delivered,
  delivered_per_second,
  delivery_ratio,
  emulator,
  node_role
]).
-define(UNAVAILABLE_METRICS, [
  lock_attempts,
  lock_collisions,
  lock_collision_percent,
  lock_wait_time,
  dist_bytes_sent_per_second,
  dist_bytes_received_per_second,
  dist_send_count_per_second,
  full_batches,
  partial_batches,
  proxy_queue_max
]).
-define(REQUIRED_WORKLOAD_CASES, [
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

all() ->
  [
    default_run_controls,
    ct_performance_overrides,
    smoke_config_profile,
    report_row_normalization,
    csv_write_formats_header_escaping_and_na,
    unavailable_supportive_metrics_render_na,
    public_role_config_sanitizes_password,
    load_payloads,
    offered_rate_uses_common_cadence,
    fixed_work_quotas_preserve_total,
    repetition_indexes_use_run_profile,
    h2_h3_report_fields_are_preserved,
    local_raw_send_counts_receiver_deliveries,
    e3_call_pacing_limits_outstanding_per_client,
    path_helpers_confirm_send_cast_and_call_counts,
    fixed_work_runner_reports_completion,
    proxy_batching_reports_batch_metrics,
    proxy_batching_sends_batches_not_individual_messages,
    workload_target_functions_are_available,
    role_procedures_follow_ready_result_protocol,
    workload_suite_exposes_exact_cases,
    test_spec_includes_performance_suites,
    workload_h2_report_row_carries_completion_ms,
    proxy_pool_batching_uses_fixed_pool_and_paced_ticks,
    paced_workload_uses_100ms_ticks,
    paced_send_reports_partial_delivery_without_timeout,
    paced_slow_call_skips_ticks_while_client_busy
  ].

default_run_controls(_Config) ->
  RunConfig = performance_config:defaults(),
  assert_equal(5000, maps:get(warmup_ms, RunConfig)),
  assert_equal(30000, maps:get(measurement_ms, RunConfig)),
  assert_equal(10000, maps:get(drain_ms, RunConfig)),
  assert_equal(3, maps:get(report_repetitions, RunConfig)),
  assert_equal(1, maps:get(smoke_repetitions, RunConfig)),
  assert_equal(?DEFAULT_CLIENT_COUNTS, maps:get(common_client_counts, RunConfig)).

ct_performance_overrides(_Config) ->
  Overrides = #{
    warmup_ms => 10,
    measurement_ms => 20,
    drain_ms => 30,
    report_repetitions => 2,
    smoke_repetitions => 1,
    common_client_counts => [1, 2, 3]
  },
  RunConfig = performance_config:from_ct([{performance, Overrides}]),
  assert_equal(10, maps:get(warmup_ms, RunConfig)),
  assert_equal(20, maps:get(measurement_ms, RunConfig)),
  assert_equal(30, maps:get(drain_ms, RunConfig)),
  assert_equal(2, maps:get(report_repetitions, RunConfig)),
  assert_equal(1, maps:get(smoke_repetitions, RunConfig)),
  assert_equal([1, 2, 3], maps:get(common_client_counts, RunConfig)).

smoke_config_profile(_Config) ->
  {ok, Terms} = file:consult(performance_file("performance.config")),
  RunConfig = performance_config:from_ct(Terms),
  assert_equal(100, maps:get(warmup_ms, RunConfig)),
  assert_equal(1000, maps:get(measurement_ms, RunConfig)),
  assert_equal(1000, maps:get(drain_ms, RunConfig)),
  assert_equal(1, maps:get(report_repetitions, RunConfig)),
  assert_equal(1, maps:get(smoke_repetitions, RunConfig)),
  assert_equal([1, 10, 100], maps:get(common_client_counts, RunConfig)).

report_row_normalization(_Config) ->
  Row =
    performance_report:normalize_row(#{
      'case' => h1_raw_send_curve,
      path => raw_send,
      role => receiver,
      warmup_ms => 5000,
      measurement_ms => 30000,
      drain_ms => 10000,
      repetition => 2,
      offered_per_second => 10000,
      delivered => 297000,
      emulator => normal,
      node_role => receiver
    }),
  assert_columns_present(?REQUIRED_COLUMNS, Row),
  assert_equal(9900.0, maps:get(delivered_per_second, Row)),
  assert_equal(0.99, maps:get(delivery_ratio, Row)).

csv_write_formats_header_escaping_and_na(Config) ->
  Path = filename:join(?config(priv_dir, Config), "performance.csv"),
  ok =
    performance_report:write_csv(
      Path,
      [
        #{
          'case' => h1_raw_send_curve,
          path => "raw,send",
          role => "receiver \"final\"",
          warmup_ms => 5000,
          measurement_ms => 30000,
          drain_ms => 10000,
          repetition => 1,
          offered_per_second => 10,
          delivered => 300,
          emulator => normal,
          node_role => receiver
        }
      ]),
  {ok, Contents} = file:read_file(Path),
  [Header, Row, <<>>] = binary:split(Contents, <<"\n">>, [global]),
  assert_equal(
    list_to_binary(csv_line([atom_to_list(Column) || Column <- performance_report:columns()])),
    Header),
  assert_equal(
    <<"h1_raw_send_curve,\"raw,send\",\"receiver \"\"final\"\"\",5000,30000,10000,1,10,300,n/a,10.0,1.0,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,normal,receiver,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a,n/a">>,
    Row).

unavailable_supportive_metrics_render_na(_Config) ->
  Metrics = performance_metrics:sample(receiver),
  assert_na_metrics(Metrics),
  Row =
    performance_report:normalize_row(#{
      'case' => h1_raw_send_curve,
      path => raw_send,
      role => receiver,
      warmup_ms => 5000,
      measurement_ms => 30000,
      drain_ms => 10000,
      repetition => 1,
      offered_per_second => 10,
      delivered => 300,
      emulator => normal,
      node_role => receiver,
      metrics => Metrics
    }),
  assert_na_metrics(Row).

public_role_config_sanitizes_password(_Config) ->
  Private = #{
    sender => local,
    receiver => #{
      host => "host",
      user => "user",
      password => "secret",
      port => "22",
      node_host => "host"
    }
  },
  Public = distributed_tests_util:public_role_config(Private),
  Receiver = maps:get(receiver, Public),
  assert_equal(false, maps:is_key(password, Receiver)),
  assert_equal("host", maps:get(host, Receiver)),
  assert_equal("user", maps:get(user, Receiver)).

load_payloads(_Config) ->
  assert_equal(tiny, performance_load:payload(tiny)),
  Data = performance_load:payload(data),
  assert_equal(value1, maps:get(field1, maps:get(archive1, Data))),
  assert_equal(value10, maps:get(field10, maps:get(archive3, Data))),
  Binary100K = performance_load:payload(binary_100k),
  Binary1M = performance_load:payload(binary_1m),
  assert_equal(100 * 1024, byte_size(Binary100K)),
  assert_equal(1024 * 1024, byte_size(Binary1M)).

offered_rate_uses_common_cadence(_Config) ->
  assert_equal(10, performance_load:offered_per_second(1)),
  assert_equal(10000, performance_load:offered_per_second(1000)),
  assert_equal(10000000, performance_load:offered_per_second(1000000)).

fixed_work_quotas_preserve_total(_Config) ->
  Quotas = performance_load:fixed_work_quotas(1000000, 6),
  assert_equal(6, length(Quotas)),
  assert_equal(1000000, lists:sum(Quotas)),
  assert_quota_difference_at_most_one(Quotas),
  MoreWritersThanWork = performance_load:fixed_work_quotas(5, 8),
  assert_equal(8, length(MoreWritersThanWork)),
  assert_equal(5, lists:sum(MoreWritersThanWork)),
  assert_quota_difference_at_most_one(MoreWritersThanWork).

repetition_indexes_use_run_profile(_Config) ->
  RunConfig = #{smoke_repetitions => 2, report_repetitions => 4},
  assert_equal([1, 2], performance_load:repetition_indexes(RunConfig, smoke)),
  assert_equal([1, 2, 3, 4], performance_load:repetition_indexes(RunConfig, report)).

h2_h3_report_fields_are_preserved(_Config) ->
  Row =
    performance_report:normalize_row(#{
      'case' => h2_fixed_work_writer_contention,
      path => raw_send,
      role => receiver,
      warmup_ms => 100,
      measurement_ms => 1000,
      drain_ms => 100,
      repetition => 1,
      offered_per_second => 1000000,
      delivered => 1000000,
      completion_ms => 1234,
      delivered_per_second => 810372.77,
      emulator => normal,
      node_role => receiver,
      metrics => #{
        full_batches => 17,
        partial_batches => 3,
        proxy_queue_max => 42
      }
    }),
  assert_member(completion_ms, performance_report:columns()),
  assert_member(client_count, performance_report:columns()),
  assert_member(batch_size, performance_report:columns()),
  assert_member(requested_busy_limit, performance_report:columns()),
  assert_member(effective_busy_limit, performance_report:columns()),
  assert_member(comparison_path, performance_report:columns()),
  assert_equal(1234, maps:get(completion_ms, Row)),
  assert_equal(810372.77, maps:get(delivered_per_second, Row)),
  assert_equal(17, maps:get(full_batches, Row)),
  assert_equal(3, maps:get(partial_batches, Row)),
  assert_equal(42, maps:get(proxy_queue_max, Row)).

local_raw_send_counts_receiver_deliveries(_Config) ->
  Count = 25,
  Receiver = performance_load:start_receiver_counter(),
  ok = performance_load:raw_send_n(Receiver, Count, performance_load:payload(tiny)),
  assert_equal(Count, performance_load:stop_receiver_counter(Receiver, 1000)).

e3_call_pacing_limits_outstanding_per_client(_Config) ->
  Owner = self(),
  CallFun =
    fun(ClientId, Sequence) ->
      Owner ! {started, self(), ClientId, Sequence},
      receive
        {release_call, Sequence} ->
          ok
      end
    end,
  {_Coordinator, Ref} = performance_load:start_e3_call_pacing([a, b], 2, CallFun),
  FirstStarts = receive_started_calls(2, 1000),
  assert_equal([1, 1], lists:sort([Sequence || {_Pid, _ClientId, Sequence} <- FirstStarts])),
  assert_no_started_call(),
  [Pid ! {release_call, 1} || {Pid, _ClientId, 1} <- FirstStarts],
  SecondStarts = receive_started_calls(2, 1000),
  assert_equal([2, 2], lists:sort([Sequence || {_Pid, _ClientId, Sequence} <- SecondStarts])),
  [Pid ! {release_call, 2} || {Pid, _ClientId, 2} <- SecondStarts],
  receive
    {Ref, #{completed := 4}} ->
      ok
  after 1000 ->
    ct:fail({e3_call_pacing_timeout, Ref})
  end.

path_helpers_confirm_send_cast_and_call_counts(_Config) ->
  Payload = performance_load:payload(tiny),
  assert_delivered(3, fun(Receiver) ->
    performance_load:run_send(raw_send, Receiver, 3, Payload, 1000)
  end),
  assert_delivered(3, fun(Receiver) ->
    performance_load:run_send(ecall_send, Receiver, 3, Payload, 1000)
  end),
  assert_delivered(3, fun(Receiver) ->
    performance_load:run_cast(erpc_cast, node(), Receiver, 3, Payload, 1000)
  end),
  assert_delivered(3, fun(Receiver) ->
    performance_load:run_cast(ecall_cast, node(), Receiver, 3, Payload, 1000)
  end),
  assert_equal(
    #{path => erpc_call, completed => 3},
    performance_load:run_call(erpc_call, node(), 3, Payload, 1000)),
  assert_equal(
    #{path => ecall_call, completed => 3},
    performance_load:run_call(ecall_call, node(), 3, Payload, 1000)).

fixed_work_runner_reports_completion(_Config) ->
  Receiver = performance_load:start_receiver_counter(),
  try
    Result = performance_load:run_fixed_work_raw_send(Receiver, 11, 4, performance_load:payload(tiny), 1000),
    assert_equal(11, maps:get(delivered, Result)),
    assert_equal([3, 3, 3, 2], maps:get(quotas, Result)),
    assert_integer_at_least(0, maps:get(completion_ms, Result))
  after
    performance_load:stop_receiver_counter(Receiver, 1000)
  end.

proxy_batching_reports_batch_metrics(_Config) ->
  Receiver = performance_load:start_receiver_counter(),
  try
    Result = performance_load:run_proxy_batching(Receiver, 7, performance_load:payload(tiny), 3, 1000),
    assert_equal(7, maps:get(delivered, Result)),
    assert_equal(2, maps:get(full_batches, Result)),
    assert_equal(1, maps:get(partial_batches, Result)),
    assert_integer_at_least(0, maps:get(proxy_queue_max, Result))
  after
    performance_load:stop_receiver_counter(Receiver, 1000)
  end.

proxy_batching_sends_batches_not_individual_messages(_Config) ->
  Receiver = performance_load:start_receiver_counter(),
  try
    Result = performance_load:run_proxy_batching(Receiver, 7, performance_load:payload(tiny), 3, 1000),
    assert_equal(7, maps:get(delivered, Result)),
    assert_equal(3, maps:get(top_level_sends, Result)),
    assert_equal(7, performance_load:await_receiver_count(Receiver, 7, 1000)),
    Stats = performance_load:receiver_stats(Receiver, 1000),
    assert_equal(7, maps:get(delivered, Stats)),
    assert_equal(3, maps:get(top_level_messages, Stats))
  after
    performance_load:stop_receiver_counter(Receiver, 1000)
  end.

workload_target_functions_are_available(_Config) ->
  Receiver = performance_load:start_receiver_counter(),
  try
    ok = performance_workload_SUITE:cast_target(Receiver, performance_load:payload(tiny)),
    assert_equal(1, performance_load:await_receiver_count(Receiver, 1, 1000)),
    assert_equal(ok, performance_workload_SUITE:call_target(performance_load:payload(tiny))),
    ok = performance_workload_SUITE:receiver_work_target(Receiver, count_only, performance_load:payload(tiny)),
    assert_equal(2, performance_load:await_receiver_count(Receiver, 2, 1000))
  after
    performance_load:stop_receiver_counter(Receiver, 1000)
  end.

role_procedures_follow_ready_result_protocol(_Config) ->
  Receiver = performance_load:start_receiver_counter(),
  ReceiverConfig = #{receiver => Receiver, expected => 4, timeout_ms => 1000},
  SenderConfig = #{
    path => raw_send,
    receiver => Receiver,
    count => 4,
    payload => performance_load:payload(tiny),
    timeout_ms => 1000
  },
  try
    {ReceiverRole, ReceiverMon} =
      spawn_monitor(performance_workload_SUITE, receiver_role, [self(), ReceiverConfig]),
    {SenderRole, SenderMon} =
      spawn_monitor(performance_workload_SUITE, sender_role, [self(), SenderConfig]),
    assert_ready(ReceiverRole),
    assert_ready(SenderRole),
    ReceiverRole ! start,
    SenderRole ! start,
    SenderResult = assert_role_result(SenderRole),
    ReceiverResult = assert_role_result(ReceiverRole),
    assert_equal(4, maps:get(delivered, SenderResult)),
    assert_equal(4, maps:get(delivered, ReceiverResult)),
    assert_down_normal(SenderMon),
    assert_down_normal(ReceiverMon)
  after
    performance_load:stop_receiver_counter(Receiver, 1000)
  end.

workload_suite_exposes_exact_cases(_Config) ->
  assert_equal(?REQUIRED_WORKLOAD_CASES, performance_workload_SUITE:all()).

test_spec_includes_performance_suites(_Config) ->
  {ok, Terms} = file:consult(performance_file("test.spec")),
  {suites, 'PERFORMANCE_TEST', Suites} = lists:keyfind(suites, 1, Terms),
  assert_equal(
    [performance_config_SUITE, performance_orchestration_SUITE, performance_workload_SUITE],
    Suites).

workload_h2_report_row_carries_completion_ms(_Config) ->
  Row =
    performance_workload_SUITE:report_row_from_result(
      h2_fixed_work_writer_contention,
      fixed_work_raw_send,
      #{
        warmup_ms => 100,
        measurement_ms => 1000,
        drain_ms => 1000
      },
      1,
      #{
        path => fixed_work_raw_send,
        delivered => 1000,
        completion_ms => 321,
        measurement_ms => 321,
        delivered_per_second => 3115.264797507788,
        delivery_ratio => 1.0
      },
      #{
        role => receiver,
        delivered => 1000,
        top_level_messages => 1000
      },
      #{
        writers => 4,
        scheduled => 1000,
        started => 1000,
        completed => 1000,
        payload => data
      }),
  Normalized = performance_report:normalize_row(Row),
  assert_equal(321, maps:get(completion_ms, Normalized)).

proxy_pool_batching_uses_fixed_pool_and_paced_ticks(_Config) ->
  Receiver = performance_load:start_receiver_counter(),
  try
    Result =
      performance_load:run_proxy_pool_paced_workload(#{
        receiver => Receiver,
        client_count => 2,
        payload => performance_load:payload(tiny),
        batch_size => 2,
        proxy_pool_size => 2,
        warmup_ms => 0,
        measurement_ms => 100,
        drain_ms => 0,
        timeout_ms => 1000
      }),
    assert_equal(1, maps:get(ticks, Result)),
    assert_equal(2, maps:get(proxy_pool_size, Result)),
    assert_equal(2, maps:get(scheduled, Result)),
    assert_equal(2, maps:get(started, Result)),
    assert_equal(2, maps:get(completed, Result)),
    assert_equal(2, maps:get(delivered, Result)),
    assert_equal(0, maps:get(full_batches, Result)),
    assert_equal(2, maps:get(partial_batches, Result)),
    assert_equal(2, maps:get(top_level_sends, Result))
  after
    performance_load:stop_receiver_counter(Receiver, 1000)
  end.

paced_workload_uses_100ms_ticks(_Config) ->
  Receiver = performance_load:start_receiver_counter(),
  try
    Result =
      performance_load:run_paced_workload(#{
        path => raw_send,
        receiver => Receiver,
        client_count => 2,
        payload => performance_load:payload(tiny),
        warmup_ms => 0,
        measurement_ms => 220,
        drain_ms => 0,
        timeout_ms => 1000
      }),
    assert_equal(20, maps:get(offered_per_second, Result)),
    assert_equal(3, maps:get(ticks, Result)),
    assert_equal(6, maps:get(scheduled, Result)),
    assert_equal(6, maps:get(delivered, Result)),
    assert_equal(6, maps:get(completed, Result)),
    assert_integer_at_least(200, maps:get(elapsed_ms, Result))
  after
    performance_load:stop_receiver_counter(Receiver, 1000)
  end.

paced_send_reports_partial_delivery_without_timeout(_Config) ->
  Receiver = start_limited_receiver_counter(2),
  try
    Result =
      performance_load:run_paced_workload(#{
        path => raw_send,
        receiver => Receiver,
        client_count => 2,
        payload => performance_load:payload(tiny),
        warmup_ms => 0,
        measurement_ms => 120,
        drain_ms => 10,
        timeout_ms => 100
      }),
    assert_equal(4, maps:get(scheduled, Result)),
    assert_equal(4, maps:get(completed, Result)),
    assert_equal(2, maps:get(delivered, Result))
  after
    stop_limited_receiver_counter(Receiver)
  end.

paced_slow_call_skips_ticks_while_client_busy(_Config) ->
  Result =
    performance_load:run_paced_workload(#{
      path => erpc_call,
      node => node(),
      client_count => 1,
      payload => performance_load:payload(tiny),
      call_target => {performance_workload_SUITE, slow_call_target, [180]},
      warmup_ms => 0,
      measurement_ms => 250,
      drain_ms => 0,
      timeout_ms => 1000
    }),
  assert_equal(3, maps:get(scheduled, Result)),
  assert_less_than(3, maps:get(completed, Result)),
  assert_integer_at_least(200, maps:get(elapsed_ms, Result)),
  assert_less_than(400, maps:get(elapsed_ms, Result)).

assert_columns_present(Columns, Row) ->
  Missing = [Column || Column <- Columns, not maps:is_key(Column, Row)],
  case Missing of
    [] ->
      ok;
    _ ->
      ct:fail({missing_columns, Missing, Row})
  end.

assert_na_metrics(Row) ->
  assert_columns_present(?UNAVAILABLE_METRICS, Row),
  [assert_equal("n/a", maps:get(Metric, Row)) || Metric <- ?UNAVAILABLE_METRICS],
  ok.

assert_equal(Expected, Actual) when Expected =:= Actual ->
  ok;
assert_equal(Expected, Actual) ->
  ct:fail({assert_equal, Expected, Actual}).

assert_member(Member, List) ->
  case lists:member(Member, List) of
    true ->
      ok;
    false ->
      ct:fail({missing_member, Member, List})
  end.

assert_delivered(Expected, RunFun) ->
  Receiver = performance_load:start_receiver_counter(),
  try
    Result = RunFun(Receiver),
    assert_equal(Expected, maps:get(delivered, Result)),
    assert_equal(Expected, performance_load:await_receiver_count(Receiver, Expected, 1000))
  after
    performance_load:stop_receiver_counter(Receiver, 1000)
  end.

assert_integer_at_least(Min, Actual) when is_integer(Actual), Actual >= Min ->
  ok;
assert_integer_at_least(Min, Actual) ->
  ct:fail({assert_integer_at_least, Min, Actual}).

assert_less_than(Max, Actual) when is_integer(Actual), Actual < Max ->
  ok;
assert_less_than(Max, Actual) ->
  ct:fail({assert_less_than, Max, Actual}).

assert_ready(RolePid) ->
  receive
    {RolePid, ready} ->
      ok
  after 1000 ->
    ct:fail({ready_timeout, RolePid})
  end.

assert_role_result(RolePid) ->
  receive
    {RolePid, Result} when is_map(Result) ->
      Result
  after 1000 ->
    ct:fail({role_result_timeout, RolePid})
  end.

assert_down_normal(MonitorRef) ->
  receive
    {'DOWN', MonitorRef, process, _Pid, normal} ->
      ok;
    {'DOWN', MonitorRef, process, Pid, Reason} ->
      ct:fail({role_down_not_normal, Pid, Reason})
  after 1000 ->
    ct:fail({down_timeout, MonitorRef})
  end.

assert_quota_difference_at_most_one(Quotas) ->
  Sorted = lists:sort(Quotas),
  Min = hd(Sorted),
  Max = lists:last(Sorted),
  case Max - Min =< 1 of
    true ->
      ok;
    false ->
      ct:fail({quota_difference_too_large, Quotas})
  end.

receive_started_calls(0, _Timeout) ->
  [];
receive_started_calls(Count, Timeout) ->
  receive
    {started, Pid, ClientId, Sequence} ->
      [{Pid, ClientId, Sequence} | receive_started_calls(Count - 1, Timeout)]
  after Timeout ->
    ct:fail({started_call_timeout, Count})
  end.

assert_no_started_call() ->
  receive
    {started, Pid, ClientId, Sequence} ->
      ct:fail({unexpected_started_call, Pid, ClientId, Sequence})
  after 0 ->
    ok
  end.

start_limited_receiver_counter(MaxDelivered) ->
  spawn(fun() -> limited_receiver_counter_loop(0, 0, MaxDelivered) end).

stop_limited_receiver_counter(Receiver) ->
  Ref = make_ref(),
  Receiver ! {limited_receiver_stop, self(), Ref},
  receive
    {limited_receiver_stopped, Ref} ->
      ok
  after 1000 ->
    ct:fail({limited_receiver_stop_timeout, Receiver})
  end.

limited_receiver_counter_loop(Delivered, TopLevelMessages, MaxDelivered) ->
  receive
    {limited_receiver_stop, From, Ref} ->
      From ! {limited_receiver_stopped, Ref};
    {performance_load_snapshot, From, Ref} ->
      From ! {performance_load_count, Ref, Delivered},
      limited_receiver_counter_loop(Delivered, TopLevelMessages, MaxDelivered);
    {performance_load_stats, From, Ref} ->
      From ! {performance_load_stats, Ref, #{
        delivered => Delivered,
        top_level_messages => TopLevelMessages
      }},
      limited_receiver_counter_loop(Delivered, TopLevelMessages, MaxDelivered);
    {performance_load_wait, From, Ref, Expected} when Delivered >= Expected ->
      From ! {performance_load_count, Ref, Delivered},
      limited_receiver_counter_loop(Delivered, TopLevelMessages, MaxDelivered);
    {performance_load_wait, _From, _Ref, _Expected} ->
      limited_receiver_counter_loop(Delivered, TopLevelMessages, MaxDelivered);
    {performance_load_batch, Payloads} ->
      Delivered1 = min(MaxDelivered, Delivered + length(Payloads)),
      limited_receiver_counter_loop(Delivered1, TopLevelMessages + 1, MaxDelivered);
    _Message ->
      Delivered1 = min(MaxDelivered, Delivered + 1),
      limited_receiver_counter_loop(Delivered1, TopLevelMessages + 1, MaxDelivered)
  end.

performance_file(Name) ->
  filename:join(filename:dirname(?FILE), Name).

csv_line(Values) ->
  join_csv(Values).

join_csv([]) ->
  [];
join_csv([Value]) ->
  Value;
join_csv([Value | Values]) ->
  [Value, $, | join_csv(Values)].
