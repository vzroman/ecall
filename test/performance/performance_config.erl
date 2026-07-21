-module(performance_config).

-export([
  defaults/0,
  from_ct/1
]).

defaults() ->
  #{
    warmup_ms => 5000,
    measurement_ms => 30000,
    drain_ms => 10000,
    report_repetitions => 3,
    smoke_repetitions => 1,
    common_client_counts => [1, 1000, 10000, 100000, 500000, 1000000]
  }.

from_ct(Config) ->
  maps:merge(defaults(), performance_overrides(Config)).

performance_overrides(Config) ->
  case lists:keyfind(performance, 1, Config) of
    {performance, Performance} ->
      Performance;
    false ->
      ct:get_config(performance, #{})
  end.
