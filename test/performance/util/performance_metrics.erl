-module(performance_metrics).

%% API
-export([
  start/1,
  begin_point/1,
  finish/1,
  handle_down/2,
  abort/1,
  point/2
]).

-define(TAG, ?MODULE).
-define(SAMPLE_INTERVAL_MS, 100).
-define(DISTRIBUTION_LOCK, dist_entry_out_queue).
-define(SCHEMA_VERSION, 6).
-define(PROC_LOADAVG, "/proc/loadavg").

-record(collector, {
  pid
}).

-record(state, {
  receiver_node,
  dist_port,
  sample_ref,
  timer_ref,
  sample_count = 0,
  memory_max = 0,
  send_oct_base = 0,
  send_cnt_base = 0,
  send_oct_final = 0,
  send_cnt_final = 0,
  load_sum = 0.0
}).

-opaque collector() :: #collector{}.
-export_type([collector/0]).


%%====================================================================
%% API
%%====================================================================

-spec start(node()) -> collector().
start(ReceiverNode) ->
  Owner = self(),
  Ref = make_ref(),
  Pid = spawn_link(fun() -> collector_init(Owner, Ref, ReceiverNode) end),
  receive
    {?TAG, Ref, Pid, ready} ->
      #collector{pid = Pid}
  end.

-spec begin_point(collector()) -> ok.
begin_point(Collector) ->
  begun = request(Collector, begin_point),
  ok.

-spec finish(collector()) -> map().
finish(Collector) ->
  {finished, Result} = request(Collector, finish),
  Result.

-spec handle_down(term(), collector()) -> not_collector.
handle_down(_Message, _Collector) ->
  not_collector.

-spec abort(collector()) -> ok.
abort(#collector{pid = Pid}) ->
  unlink(Pid),
  exit(Pid, kill),
  ok.

-spec point(list(), map()) -> ok.
point(Config, Result) ->
  EnvSettings = ct:get_config(env_settings),
  RoleConfig = ct:get_config(role_config),
  StoredResult = Result#{
    schema_version => ?SCHEMA_VERSION,
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

request(#collector{pid = Pid}, Request) ->
  Ref = make_ref(),
  Pid ! {?TAG, Ref, self(), Request},
  receive
    {?TAG, Ref, Pid, Reply} ->
      Reply
  end.


%%====================================================================
%% Collector process
%%====================================================================

collector_init(Owner, Ref, ReceiverNode) ->
  ok = lcnt:rt_mask([distribution]),
  _ = erlang:memory(total),
  Owner ! {?TAG, Ref, self(), ready},
  collector_wait(#state{receiver_node = ReceiverNode}).

collector_wait(State) ->
  receive
    {?TAG, Ref, From, begin_point} ->
      State1 = begin_network(State),
      ok = lcnt:clear(),
      SampleRef = make_ref(),
      State2 = sample(State1#state{sample_ref = SampleRef}),
      TimerRef = schedule_sample(SampleRef),
      From ! {?TAG, Ref, self(), begun},
      collector_loop(State2#state{timer_ref = TimerRef});
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
      State2 = finish_network(State1),
      Result = result(State2),
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
    memory_max = MemoryMax,
    load_sum = LoadSum} = State) ->
  Memory = erlang:memory(total),
  Load = read_load_average(),
  State#state{
    sample_count = SampleCount + 1,
    memory_max = erlang:max(MemoryMax, Memory),
    load_sum = LoadSum + Load
  }.

result(#state{memory_max = MemoryMax} = State) ->
  #{
    memory => #{maximum_bytes => MemoryMax},
    locks => lock_results(),
    network => network_result(State),
    load => load_result(State)
  }.


%%====================================================================
%% Network metrics
%%====================================================================

begin_network(#state{receiver_node = ReceiverNode} = State) ->
  DistPort = resolve_dist_port(ReceiverNode),
  {SendOct, SendCnt} = socket_counters(DistPort),
  State#state{
    dist_port = DistPort,
    send_oct_base = SendOct,
    send_cnt_base = SendCnt
  }.

finish_network(#state{dist_port = DistPort} = State) ->
  {SendOct, SendCnt} = socket_counters(DistPort),
  State#state{send_oct_final = SendOct, send_cnt_final = SendCnt}.

resolve_dist_port(ReceiverNode) ->
  {ReceiverNode, DistPort} =
    lists:keyfind(ReceiverNode, 1, erlang:system_info(dist_ctrl)),
  DistPort.

socket_counters(DistPort) ->
  {ok, Stats} = inet:getstat(DistPort, [send_oct, send_cnt]),
  {proplists:get_value(send_oct, Stats),
   proplists:get_value(send_cnt, Stats)}.

network_result(#state{
    send_oct_base = SendOctBase,
    send_cnt_base = SendCntBase,
    send_oct_final = SendOctFinal,
    send_cnt_final = SendCntFinal}) ->
  SendOct = SendOctFinal - SendOctBase,
  SendCnt = SendCntFinal - SendCntBase,
  #{
    send_octets => SendOct,
    average_packet_bytes => average_packet(SendOct, SendCnt)
  }.

average_packet(_SendOct, 0) ->
  0.0;
average_packet(SendOct, SendCnt) ->
  SendOct / SendCnt.


%%====================================================================
%% Lock counters
%%====================================================================

lock_results() ->
  Data = lcnt:rt_collect(),
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
  lock_result(Stats).

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

lock_result(#{tries := Tries, collisions := Collisions, wait_us := WaitUs}) ->
  #{
    collision_percent => percent(Collisions, Tries),
    wait_us => WaitUs
  }.

time_us({Seconds, Nanoseconds}) ->
  Seconds * 1000000 + Nanoseconds div 1000;
time_us({Seconds, Nanoseconds, _Samples}) ->
  time_us({Seconds, Nanoseconds}).

percent(_Part, 0) ->
  0.0;
percent(Part, Whole) ->
  (Part * 100) / Whole.


%%====================================================================
%% Load metrics
%%====================================================================

load_result(#state{sample_count = SampleCount, load_sum = LoadSum}) ->
  #{average_1m => LoadSum / SampleCount}.

read_load_average() ->
  {ok, Data} = file:read_file(?PROC_LOADAVG),
  [LoadAverage1m | _] =
    binary:split(Data, [<<" ">>, <<"\n">>], [global, trim_all]),
  binary_to_float(LoadAverage1m).
