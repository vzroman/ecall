
-module(ecall_sup).

-include("ecall.hrl").

-behaviour(supervisor).

%% OTP API
-export([
  start_link/1,
  init/1
]).

-spec start_link(ecall_receive:pool_size()) -> supervisor:startlink_ret().
start_link( PoolSize ) ->
    supervisor:start_link(?MODULE, [ PoolSize ]).

% PoolSize is a validated pool_size: a positive integer or disabled, in which
% case the node runs without a receive pool and is invisible to its neighbours.
-spec init([ecall_receive:pool_size()]) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([ PoolSize ]) ->

  PG = #{
    id=> pg_scope,
    start=>{ pg, start_link, [ ecall ]},
    restart=> permanent,
    shutdown=> ?STOP_TIMEOUT,
    type=>worker,
    modules=>[ pg ]
  },

  Receive = #{
    id=> receive_pool,
    start=>{ ecall_receive, start_link, [ PoolSize ]},
    restart=> permanent,
    shutdown=> ?STOP_TIMEOUT,
    type=> worker,
    modules=>[ ecall_receive ]
  },

  ConnectionSup=#{
    id=>ecall_connection_sup,
    start=>{ecall_connection_sup,start_link,[]},
    restart=>permanent,
    shutdown=>?STOP_TIMEOUT,
    type=>supervisor,
    modules=>[ecall_connection_sup]
  },

  PG_monitor = #{
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

  {ok, {Supervisor,
    [ PG ] ++
    [ Receive || is_integer( PoolSize ) ] ++
    [ ConnectionSup, PG_monitor ]
  }}.
