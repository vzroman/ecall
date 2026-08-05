
-module(ecall_connection_sup).

-include("ecall.hrl").

-behaviour(supervisor).

-export([
  start_link/0,
  init/1
]).

-export([
  start_connection/1,
  stop_connection/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->

  Supervisor=#{
    strategy=>one_for_one,
    intensity=>?MAX_RESTARTS,
    period=>?MAX_PERIOD
  },

  {ok,{Supervisor, []}}.

start_connection( Node )->
  ChildSpec = #{
    id=> Node,
    start=>{ecall_connection, start_link, [Node]},
    restart=> permanent,
    shutdown=> ?STOP_TIMEOUT,
    type=>worker,
    modules=>[ecall_connection]
  },
  case supervisor:start_child(?MODULE, ChildSpec) of
    {ok,_}-> ok;
    {ok, _, _}-> ok;
    {error, already_present}->ok;
    {error, {already_started, _Pid}}->ok;
    {error,Error} -> throw(Error)
  end.

stop_connection( Node )->
  supervisor:terminate_child(?MODULE, Node),
  supervisor:delete_child( ?MODULE, Node),
  ok.
