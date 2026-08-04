-module(performance_metrics).

%% API
-export([
  start/0,
  begin_point/1,
  finish/1,
  handle_down/2,
  abort/1,
  point/2
]).

-define(TAG, ?MODULE).
-define(SAMPLE_INTERVAL_MS, 100).
-define(DISTRIBUTION_LOCK, dist_entry_out_queue).

-record(collector, {
  pid,
  monitor
}).

-record(state, {
  sample_ref,
  timer_ref,
  sample_count = 0,
  memory_sum = 0,
  memory_max = 0
}).

-opaque collector() :: #collector{}.
-export_type([collector/0]).


%%====================================================================
%% API
%%====================================================================

-spec start() -> collector().
start() ->
  Owner = self(),
  Ref = make_ref(),
  {Pid, Monitor} = spawn_monitor(fun() -> collector_init(Owner, Ref) end),
  receive
    {?TAG, Ref, Pid, ready} ->
      #collector{pid = Pid, monitor = Monitor};
    {'DOWN', Monitor, process, Pid, Reason} ->
      exit({metrics_collector_start_failed, Reason})
  end.

-spec begin_point(collector()) -> ok.
begin_point(Collector) ->
  begun = request(Collector, begin_point),
  ok.

-spec finish(collector()) -> map().
finish(#collector{monitor = Monitor} = Collector) ->
  {finished, Result} = request(Collector, finish),
  erlang:demonitor(Monitor, [flush]),
  Result.

-spec handle_down(term(), collector()) -> not_collector | no_return().
handle_down(
    {'DOWN', Monitor, process, Pid, Reason},
    #collector{pid = Pid, monitor = Monitor}) ->
  exit({metrics_collector_failed, Reason});
handle_down(_Message, _Collector) ->
  not_collector.

-spec abort(collector()) -> ok.
abort(#collector{pid = Pid, monitor = Monitor}) ->
  erlang:demonitor(Monitor, [flush]),
  exit(Pid, kill),
  ok.

-spec point(list(), map()) -> ok.
point(Config, Result) ->
  EnvSettings = ct:get_config(env_settings),
  RoleConfig = ct:get_config(role_config),
  StoredResult = Result#{
    schema_version => 1,
    distribution_busy_limit_kib =>
      maps:get(distribution_busy_limit_kib, EnvSettings),
    sender_config => role_description(maps:get(sender, RoleConfig)),
    receiver_config => role_description(maps:get(receiver, RoleConfig))
  },
  log_point(StoredResult),
  write_point(Config, StoredResult).

role_description(local) ->
  <<"local">>;
role_description(Role) ->
  User = maps:get(user, Role),
  Host = maps:get(host, Role),
  iolist_to_binary([User, $@, Host]).

log_point(#{operation := send} = Result) ->
  ct:pal("Send performance point completed: ~p", [Result]);
log_point(#{operation := cast} = Result) ->
  ct:pal("Cast performance point completed: ~p", [Result]);
log_point(#{operation := call} = Result) ->
  ct:pal("Call performance point completed: ~p", [Result]).

write_point(Config, Result) ->
  PrivDir = proplists:get_value(priv_dir, Config),
  DataDir = filename:join(PrivDir, "performance_data"),
  File = filename:join(DataDir, point_filename(Result)),
  ok = filelib:ensure_dir(File),
  ok = file:write_file(File, json:encode(Result)).

point_filename(#{
    operation := Operation,
    path := Path,
    payload := Payload,
    writer_count := WriterCount}) ->
  lists:flatten(
    io_lib:format(
      "~s.~s.~s.~B.json",
      [Operation, Path, Payload, WriterCount])).


%%====================================================================
%% Collector protocol
%%====================================================================

request(#collector{pid = Pid, monitor = Monitor}, Request) ->
  Ref = make_ref(),
  Pid ! {?TAG, Ref, self(), Request},
  receive
    {?TAG, Ref, Pid, Reply} ->
      Reply;
    {'DOWN', Monitor, process, Pid, Reason} ->
      exit({metrics_collector_failed, Reason})
  end.


%%====================================================================
%% Collector process
%%====================================================================

collector_init(Owner, Ref) ->
  ok = lcnt:rt_mask([distribution]),
  _ = erlang:memory(total),
  Owner ! {?TAG, Ref, self(), ready},
  collector_wait(#state{}).

collector_wait(State) ->
  receive
    {?TAG, Ref, From, begin_point} ->
      ok = lcnt:clear(),
      SampleRef = make_ref(),
      State1 = sample(State#state{sample_ref = SampleRef}),
      TimerRef = schedule_sample(SampleRef),
      From ! {?TAG, Ref, self(), begun},
      collector_loop(State1#state{timer_ref = TimerRef});
    Message ->
      exit({unexpected_metrics_message, Message})
  end.

collector_loop(#state{sample_ref = SampleRef} = State) ->
  receive
    {?TAG, SampleRef, sample} ->
      State1 = sample(State),
      TimerRef = schedule_sample(SampleRef),
      collector_loop(State1#state{timer_ref = TimerRef});
    {?TAG, Ref, From, finish} ->
      cancel_sample(State#state.timer_ref),
      State1 = sample(State),
      Result = result(State1),
      From ! {?TAG, Ref, self(), {finished, Result}},
      ok;
    Message ->
      exit({unexpected_metrics_message, Message})
  end.

schedule_sample(SampleRef) ->
  erlang:send_after(?SAMPLE_INTERVAL_MS, self(), {?TAG, SampleRef, sample}).

cancel_sample(TimerRef) ->
  _ = erlang:cancel_timer(TimerRef),
  ok.

%%====================================================================
%% Sampling and aggregation
%%====================================================================

sample(#state{
    sample_count = SampleCount,
    memory_sum = MemorySum,
    memory_max = MemoryMax} = State) ->
  Memory = erlang:memory(total),
  State#state{
    sample_count = SampleCount + 1,
    memory_sum = MemorySum + Memory,
    memory_max = erlang:max(MemoryMax, Memory)
  }.

result(#state{
    sample_count = SampleCount,
    memory_sum = MemorySum,
    memory_max = MemoryMax}) ->
  #{
    memory => #{
      average_bytes => MemorySum / SampleCount,
      maximum_bytes => MemoryMax
    },
    locks => lock_results()
  }.


%%====================================================================
%% Lock counters
%%====================================================================

lock_results() ->
  Data = lcnt:rt_collect(),
  DurationUs = time_us(proplists:get_value(duration, Data)),
  Locks = proplists:get_value(locks, Data),
  EntryLists =
    [Entries
     || {?DISTRIBUTION_LOCK, _Id, _Type, Entries} <- Locks],
  true = EntryLists =/= [],
  Stats =
    lists:foldl(
      fun combine_lock_entry/2,
      empty_lock_stats(),
      lists:append(EntryLists)),
  lock_result(Stats, DurationUs).

combine_lock_entry(
    {{_File, _Line}, {Tries, Collisions, Wait}, _Histogram},
    Stats) ->
  add_lock_stats(
    Stats,
    #{tries => Tries, collisions => Collisions, wait_us => time_us(Wait)});
combine_lock_entry(
    {{_File, _Line}, {Tries, Collisions, Wait}},
    Stats) ->
  add_lock_stats(
    Stats,
    #{tries => Tries, collisions => Collisions, wait_us => time_us(Wait)}).

empty_lock_stats() ->
  #{tries => 0, collisions => 0, wait_us => 0}.

add_lock_stats(Left, Right) ->
  maps:map(
    fun(Key, Value) -> Value + maps:get(Key, Right) end,
    Left).

lock_result(
    #{tries := Tries, collisions := Collisions, wait_us := WaitUs},
    DurationUs) ->
  #{
    collision_percent => percent(Collisions, Tries),
    wait_us => WaitUs,
    duration_percent => percent(WaitUs, DurationUs)
  }.

time_us({Seconds, Nanoseconds}) ->
  Seconds * 1000000 + Nanoseconds div 1000;
time_us({Seconds, Nanoseconds, _Samples}) ->
  time_us({Seconds, Nanoseconds}).

percent(_Part, 0) ->
  0.0;
percent(Part, Whole) ->
  (Part * 100) / Whole.
