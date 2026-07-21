-module(performance_metrics).

-export([
  sample/1,
  unavailable/0
]).

sample(NodeRole) ->
  {{input, InputBytes}, {output, OutputBytes}} = erlang:statistics(io),
  {Reductions, _ReductionsSinceLastCall} = erlang:statistics(reductions),
  maps:merge(
    unavailable(),
    #{
      node_role => NodeRole,
      emulator => normal,
      io_input_bytes => InputBytes,
      io_output_bytes => OutputBytes,
      process_count => erlang:system_info(process_count),
      reductions => Reductions
    }).

unavailable() ->
  #{
    lock_attempts => "n/a",
    lock_collisions => "n/a",
    lock_collision_percent => "n/a",
    lock_wait_time => "n/a",
    dist_bytes_sent_per_second => "n/a",
    dist_bytes_received_per_second => "n/a",
    dist_send_count_per_second => "n/a",
    full_batches => "n/a",
    partial_batches => "n/a",
    proxy_queue_max => "n/a"
  }.
