
-module(ecall_sup).

-behaviour(supervisor).

-export([
  start_link/0,
  init/1
]).

-define(MAX_RESTARTS,10).
-define(MAX_PERIOD,1000).
-define(STOP_TIMEOUT,1000).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->

  PG = #{
    id=> pg_scope,
    start=>{ pg, start_link, [ ecall ]},
    restart=> permanent,
    shutdown=> ?STOP_TIMEOUT,
    type=>worker,
    modules=>[ pg ]
  },

  _PG_monitor = #{
    id=> ecall_pg_monitor,
    start=>{ ecall_pg_monitor, start_link, []},
    restart=> permanent,
    shutdown=> ?STOP_TIMEOUT,
    type=>worker,
    modules=>[ ecall_pg_monitor ]
  },

  Supervisor=#{
    strategy=>one_for_one,
    intensity=> ?MAX_RESTARTS,
    period=> ?MAX_PERIOD
  },

  {ok, {Supervisor, [
    PG
  ]}}.
