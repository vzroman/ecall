
-module(ecall_connection_sup).

-include("ecall.hrl").

-behaviour(supervisor).

-export([
  start_link/0,
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->

  ChildConfig = #{
    id=> ecall_connection,
    start=>{ecall_connection,start_link,[]},
    restart=> transient,
    shutdown=> ?STOP_TIMEOUT,
    type=>worker,
    modules=>[ecall_connection]
  },

  Supervisor=#{
    strategy=>simple_one_for_one,
    intensity=>?MAX_RESTARTS,
    period=>?MAX_PERIOD
  },

  {ok,{Supervisor, [ ChildConfig ]}}.


