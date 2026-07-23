-module(performance_send_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test API
-export([
  all/0,
  groups/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_group/2,
  end_per_group/2,
  init_per_testcase/2,
  end_per_testcase/2
]).

%% Workload test cases
-export([
  point/1
]).

%%====================================================================
%% Common Test API
%%====================================================================

all() ->
  [{group, raw}, {group, ecall}].

groups() ->
  Performance = distributed_tests_utils:performance_settings([]),
  WriterCounts = maps:get(writer_counts, Performance),
  [
    {raw, [sequence], payload_groups(WriterCounts)},
    {ecall, [sequence], batch_groups(Performance, WriterCounts)}
  ].

init_per_suite(Config) ->
  distributed_tests_utils:start_topology([{suite, ?MODULE} | Config]).

end_per_suite(_Config) ->
  distributed_tests_utils:stop_all().

init_per_group(raw, Config) ->
  [{path, raw} | Config];
init_per_group(ecall, Config) ->
  [{path, ecall} | Config];
init_per_group(Group, Config) ->
  case {batch_size(Group), payload_profile(Group), writer_count_group(Group)} of
    {{ok, BatchSize}, false, false} ->
      init_ecall_batch_group(BatchSize, Config);
    {false, {ok, Profile}, false} ->
      [{payload_profile, Profile} | Config];
    {false, false, {ok, WriterCount}} ->
      [{writer_count, WriterCount} | Config];
    {false, false, false} ->
      Config
  end.

end_per_group(Group, Config) ->
  case batch_size(Group) of
    {ok, _BatchSize} ->
      SenderNode = ?config(sender, Config),
      ReceiverNode = ?config(receiver, Config),
      ok =
        distributed_tests_utils:disconnect_ecall_bidirectional(
          SenderNode,
          ReceiverNode);
    false ->
      ok
  end,
  Config.

init_per_testcase(Testcase, Config) ->
  _ = ct:timetrap(infinity),
  distributed_tests_utils:init_per_testcase(Testcase, Config).

end_per_testcase(Testcase, Config) ->
  distributed_tests_utils:end_per_testcase(Testcase, Config).

%%====================================================================
%% Workload test cases
%%====================================================================

point(Config) ->
  run_point(Config).

%%====================================================================
%% Group construction
%%====================================================================

batch_groups(Performance, WriterCounts) ->
  [
    {batch_group(BatchSize), [sequence], payload_groups(WriterCounts)}
    || BatchSize <- maps:get(ecall_batch_sizes, Performance)
  ].

payload_groups(WriterCounts) ->
  [
    {Profile, [sequence], writer_groups(WriterCounts)}
    || Profile <- performance_payloads:profiles()
  ].

writer_groups(WriterCounts) ->
  [
    {writer_group(WriterCount), [sequence], [point]}
    || WriterCount <- WriterCounts
  ].

writer_group(WriterCount) ->
  list_to_atom("writers_" ++ count_suffix(WriterCount)).

batch_group(BatchSize) ->
  list_to_atom("batch_" ++ integer_to_list(BatchSize)).

count_suffix(Count) when Count >= 1000000, Count rem 1000000 =:= 0 ->
  integer_to_list(Count div 1000000) ++ "m";
count_suffix(Count) when Count >= 1000, Count rem 1000 =:= 0 ->
  integer_to_list(Count div 1000) ++ "k";
count_suffix(Count) ->
  integer_to_list(Count).

%%====================================================================
%% Group setup
%%====================================================================

init_ecall_batch_group(BatchSize, Config) ->
  SenderNode = ?config(sender, Config),
  ReceiverNode = ?config(receiver, Config),
  BatchMetadata =
    distributed_tests_utils:prepare_ecall_batch(
      SenderNode,
      ReceiverNode,
      BatchSize),
  ct:pal("Effective ecall batch metadata: ~p", [BatchMetadata]),
  [{ecall_batch, BatchMetadata} | Config].

batch_size(Group) ->
  case atom_to_list(Group) of
    "batch_" ++ Digits ->
      {ok, list_to_integer(Digits)};
    _Other ->
      false
  end.

payload_profile(Group) ->
  case lists:member(Group, performance_payloads:profiles()) of
    true ->
      {ok, Group};
    false ->
      false
  end.

writer_count_group(Group) ->
  case atom_to_list(Group) of
    "writers_" ++ Suffix ->
      {ok, writer_count_from_suffix(Suffix)};
    _Other ->
      false
  end.

%%====================================================================
%% Point execution
%%====================================================================

run_point(Config) ->
  Path = ?config(path, Config),
  ok = maybe_verify_native_connectivity(Path, Config),
  PointConfig = point_config(Config),
  case distributed_tests_utils:run_role_procedure(
         ?config(sender, Config),
         {performance_load_utils, run_send_point, [PointConfig]},
         infinity) of
    {ok, Result} ->
      ct:pal("Performance point completed: ~p", [Result]),
      ok;
    {error, Reason} ->
      Failure = point_failure(PointConfig, Reason),
      ct:pal("Performance point failed: ~p", [Failure]),
      distributed_tests_utils:fail_point(
        Config,
        {performance_point_failed, Failure})
  end.

maybe_verify_native_connectivity(raw, Config) ->
  distributed_tests_utils:verify_role_connectivity(
    ?config(sender, Config),
    ?config(receiver, Config));
maybe_verify_native_connectivity(ecall, _Config) ->
  ok.

point_config(Config) ->
  Performance = ?config(performance, Config),
  SenderNode = ?config(sender, Config),
  ReceiverNode = ?config(receiver, Config),
  ConfiguredBusy = maps:get(distribution_busy_limit_kib, Performance),
  WriterCount = ?config(writer_count, Config),
  Base = #{
    suite => ?MODULE,
    path => ?config(path, Config),
    receiver_node => ReceiverNode,
    payload_profile => ?config(payload_profile, Config),
    writer_count => WriterCount,
    expected => WriterCount * maps:get(messages_per_writer, Performance),
    messages_per_writer => maps:get(messages_per_writer, Performance),
    pace_ms => maps:get(pace_ms, Performance),
    configured_distribution_busy_limit_kib => ConfiguredBusy,
    effective_distribution_busy_limit_kib =>
      effective_busy_limit(SenderNode, ReceiverNode),
    monitor_pids => monitor_pids(Config)
  },
  maps:merge(Base, batch_point_metadata(Config)).

point_failure(PointConfig, Reason) ->
  (maps:with(
     [
       suite,
       path,
       receiver_node,
       payload_profile,
       writer_count,
       expected,
       messages_per_writer,
       pace_ms,
       configured_batch_size,
       effective_batch_size,
       configured_distribution_busy_limit_kib,
       effective_distribution_busy_limit_kib
     ],
     PointConfig))#{operation => send, reason => Reason}.

monitor_pids(Config) ->
  case ?config(ecall_batch, Config) of
    undefined ->
      [];
    BatchMetadata ->
      [
        maps:get(sender_connection_pid, BatchMetadata),
        maps:get(receiver_connection_pid, BatchMetadata)
      ]
  end.

batch_point_metadata(Config) ->
  case ?config(ecall_batch, Config) of
    undefined ->
      #{};
    BatchMetadata ->
      maps:with(
        [
          configured_batch_size,
          effective_batch_size,
          sender_proxy_count,
          receiver_proxy_count
        ],
        BatchMetadata)
  end.

effective_busy_limit(SenderNode, ReceiverNode) ->
  SenderBusy =
    distributed_tests_utils:effective_dist_buf_busy_limit_kib(SenderNode),
  ReceiverBusy =
    distributed_tests_utils:effective_dist_buf_busy_limit_kib(ReceiverNode),
  case SenderBusy =:= ReceiverBusy of
    true ->
      SenderBusy;
    false ->
      ct:fail({busy_limit_mismatch, SenderNode, SenderBusy,
               ReceiverNode, ReceiverBusy})
  end.

writer_count_from_suffix(Suffix) ->
  case lists:last(Suffix) of
    $k ->
      list_to_integer(lists:droplast(Suffix)) * 1000;
    $m ->
      list_to_integer(lists:droplast(Suffix)) * 1000000;
    _Other ->
      list_to_integer(Suffix)
  end.
