-module(performance_report).

-export([
  columns/0,
  normalize_row/1,
  write_csv/2
]).

columns() ->
  [
    'case',
    path,
    role,
    warmup_ms,
    measurement_ms,
    drain_ms,
    repetition,
    offered_per_second,
    delivered,
    completion_ms,
    delivered_per_second,
    delivery_ratio,
    lock_attempts,
    lock_collisions,
    lock_collision_percent,
    lock_wait_time,
    dist_bytes_sent_per_second,
    dist_bytes_received_per_second,
    dist_send_count_per_second,
    full_batches,
    partial_batches,
    proxy_queue_max,
    emulator,
    node_role,
    client_count,
    writers,
    batch_size,
    proxy_pool_size,
    payload,
    receiver_work,
    requested_busy_limit,
    effective_busy_limit,
    scheduled,
    started,
    completed,
    elapsed_ms,
    top_level_sends,
    top_level_messages,
    throughput_ratio,
    comparison_group,
    comparison_path
  ].

normalize_row(Input) ->
  Metrics = maps:get(metrics, Input, #{}),
  Base = maps:merge(Metrics, maps:remove(metrics, Input)),
  MeasurementMs = maps:get(measurement_ms, Base),
  OfferedPerSecond = maps:get(offered_per_second, Base),
  Delivered = maps:get(delivered, Base),
  Derived = #{
    delivered_per_second =>
      maps:get(delivered_per_second, Base, per_second(Delivered, MeasurementMs)),
    delivery_ratio =>
      maps:get(delivery_ratio, Base, delivery_ratio(Delivered, OfferedPerSecond, MeasurementMs))
  },
  Defaults =
    maps:merge(
      performance_metrics:unavailable(),
      #{
        completion_ms => "n/a",
        client_count => "n/a",
        writers => "n/a",
        batch_size => "n/a",
        proxy_pool_size => "n/a",
        payload => "n/a",
        receiver_work => "n/a",
        requested_busy_limit => "n/a",
        effective_busy_limit => "n/a",
        scheduled => "n/a",
        started => "n/a",
        completed => "n/a",
        elapsed_ms => "n/a",
        top_level_sends => "n/a",
        top_level_messages => "n/a",
        throughput_ratio => "n/a",
        comparison_group => "n/a",
        comparison_path => "n/a"
      }),
  maps:merge(Defaults, maps:merge(Base, Derived)).

write_csv(Path, Rows) ->
  NormalizedRows = [normalize_row(Row) || Row <- Rows],
  file:write_file(Path, csv(NormalizedRows)).

per_second(Count, MeasurementMs) ->
  Count / (MeasurementMs / 1000).

delivery_ratio(_Delivered, 0, _MeasurementMs) ->
  0.0;
delivery_ratio(Delivered, OfferedPerSecond, MeasurementMs) ->
  Delivered / (OfferedPerSecond * (MeasurementMs / 1000)).

csv(Rows) ->
  [csv_line(columns()) | [csv_line([maps:get(Column, Row) || Column <- columns()]) || Row <- Rows]].

csv_line(Values) ->
  [join_csv([csv_value(Value) || Value <- Values]), $\n].

join_csv([]) ->
  [];
join_csv([Value]) ->
  Value;
join_csv([Value | Values]) ->
  [Value, $, | join_csv(Values)].

csv_value(Value) when is_atom(Value) ->
  csv_escape(atom_to_list(Value));
csv_value(Value) when is_integer(Value) ->
  integer_to_list(Value);
csv_value(Value) when is_float(Value) ->
  io_lib:format("~p", [Value]);
csv_value(Value) when is_list(Value) ->
  csv_escape(Value);
csv_value(Value) when is_binary(Value) ->
  csv_escape(binary_to_list(Value));
csv_value(Value) ->
  csv_escape(lists:flatten(io_lib:format("~p", [Value]))).

csv_escape(Value) ->
  case needs_csv_escape(Value) of
    true ->
      [$", escape_quotes(Value), $"];
    false ->
      Value
  end.

needs_csv_escape(Value) ->
  lists:any(fun(Char) -> lists:member(Char, [$,, $", $\n, $\r]) end, Value).

escape_quotes([]) ->
  [];
escape_quotes([$" | Rest]) ->
  [$", $" | escape_quotes(Rest)];
escape_quotes([Char | Rest]) ->
  [Char | escape_quotes(Rest)].
