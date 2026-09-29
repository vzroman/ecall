
-module(ecall_connection_sup).
-moduledoc false.

-include("ecall.hrl").

-behaviour(supervisor).

%% OTP API
-export([
  start_link/0,
  init/1
]).

%% CONNECTION API
-export([
  start_connection/1,
  stop_connection/1,
  connection_master/1
]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init([]) -> {ok, {supervisor:sup_flags(), []}}.
init([]) ->

  Supervisor=#{
    strategy=>one_for_one,
    intensity=>?MAX_RESTARTS,
    period=>?MAX_PERIOD
  },

  {ok,{Supervisor, []}}.

-spec start_connection(node()) -> {ok, pid()} | {error, term()}.
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
    {ok, Pid}-> {ok, Pid};
    {ok, Pid, _}-> {ok, Pid};
    {error, {already_started, Pid}}-> {ok, Pid};
    {error, already_present}-> restart_connection( Node );
    {error, Error} -> {error, Error}
  end.

-spec restart_connection(node()) -> {ok, pid()} | {error, term()}.
restart_connection( Node )->
  case supervisor:restart_child(?MODULE, Node) of
    {ok, Pid}-> {ok, Pid};
    {ok, Pid, _}-> {ok, Pid};
    {error, Error}-> {error, Error}
  end.

% The pid of the running connection master of the node, undefined otherwise.
-spec connection_master(node()) -> pid() | undefined.
connection_master( Node )->
  try lists:keyfind( Node, 1, supervisor:which_children(?MODULE) ) of
    {Node, Pid, _Type, _Modules} when is_pid( Pid )-> Pid;
    _-> undefined
  catch
    exit:_-> undefined
  end.

% ok only when the child is gone: a join racing the stop can restart it
% between terminate_child and delete_child.
-spec stop_connection(node()) -> ok | {error, term()}.
stop_connection( Node )->
  supervisor:terminate_child(?MODULE, Node),
  case supervisor:delete_child( ?MODULE, Node) of
    ok-> ok;
    {error, not_found}-> ok;
    {error, Error}-> {error, Error}
  end.
