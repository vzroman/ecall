-module(ecall).

%% Public application API. Transport and group policies live in internal modules.

%%=================================================================
%% SINGLE NODE API
%%=================================================================
-export([send/2, cast/4, call/4]).

%%=================================================================
%% GROUP API
%%=================================================================
-export([
  call_one/4, call_one/5,
  call_any/4, call_any/5,
  call_all/4, call_all/5,
  call_all_wait/4,
  cast_one/4,
  cast_all/4
]).

%%=================================================================
%% CONNECTION API
%%=================================================================
-export([start_connection/1, stop_connection/1, connection_info/1]).

%%=================================================================
%% SINGLE NODE API
%%=================================================================
-spec send(erlang:send_destination(), Message) -> Message when Message :: term().
send(To, Message) ->
  ecall_connection:send(To, Message).

-spec cast(node(), module(), atom(), [term()]) -> ok.
cast(Node, Module, Function, Args) ->
  ecall_connection:cast(Node, Module, Function, Args).

-spec call(node(), module(), atom(), [term()]) ->
  {ok, term()} | {error, term()}.
call(Node, Module, Function, Args) ->
  ecall_connection:call(Node, Module, Function, Args).

%%=================================================================
%% GROUP API
%%=================================================================
-spec call_one([node()], module(), atom(), [term()]) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_one(Nodes, Module, Function, Args) ->
  ecall_group:call_one(Nodes, Module, Function, Args).

-spec call_one([node()], module(), atom(), [term()], boolean()) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_one(Nodes, Module, Function, Args, RpcErr) ->
  ecall_group:call_one(Nodes, Module, Function, Args, RpcErr).

-spec call_any([node()], module(), atom(), [term()]) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_any(Nodes, Module, Function, Args) ->
  ecall_group:call_any(Nodes, Module, Function, Args).

-spec call_any([node()], module(), atom(), [term()], boolean()) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_any(Nodes, Module, Function, Args, RpcErr) ->
  ecall_group:call_any(Nodes, Module, Function, Args, RpcErr).

-spec call_all([node()], module(), atom(), [term()]) ->
  {ok, [{node(), term()}]} | {error, none_is_available | {node(), term()}}.
call_all(Nodes, Module, Function, Args) ->
  ecall_group:call_all(Nodes, Module, Function, Args).

-spec call_all([node()], module(), atom(), [term()], boolean()) ->
  {ok, [{node(), term()}]} | {error, none_is_available | {node(), term()}}.
call_all(Nodes, Module, Function, Args, RpcErr) ->
  ecall_group:call_all(Nodes, Module, Function, Args, RpcErr).

-spec call_all_wait([node()], module(), atom(), [term()]) ->
  {[{node(), term()}], [{node(), term()}]}.
call_all_wait(Nodes, Module, Function, Args) ->
  ecall_group:call_all_wait(Nodes, Module, Function, Args).

-spec cast_one(nonempty_list(node()), module(), atom(), [term()]) -> ok.
cast_one(Nodes, Module, Function, Args) ->
  ecall_group:cast_one(Nodes, Module, Function, Args).

-spec cast_all([node()], module(), atom(), [term()]) -> ok.
cast_all(Nodes, Module, Function, Args) ->
  ecall_group:cast_all(Nodes, Module, Function, Args).

%%=================================================================
%% CONNECTION API
%%=================================================================
% Asynchronous: poll connection_info/1 for the result.
-spec start_connection(node()) -> ok | {error, term()}.
start_connection(Node) ->
  ecall_connection:connect(Node).

-spec stop_connection(node()) -> ok.
stop_connection(Node) ->
  ecall_connection:disconnect(Node).

-spec connection_info(node()) ->
  {ok, #{status := connected, connection_pid := pid(),
         proxy_count := pos_integer()}}
  | {ok, #{status := down, connection_pid := pid()}}
  | {error, not_connected}.
connection_info(Node) ->
  ecall_connection:connection_info(Node).
