-module(ecall).

-moduledoc """
Pooled, batched transport for Erlang distribution.

`send/2`, `cast/4` and `call/4` replace `!`, `erpc:cast/4` and
`erpc:call/4`. When a connection to the target node exists they go through
a pool of local proxies that batch requests into the distribution channel,
otherwise they take the native path, so they are safe in a cluster where
only some nodes run `ecall`. The group functions apply one call or cast to a
list of nodes with a policy about which nodes to use and how many answers to
wait for. See the README for the design and the measurements.

This module is the whole public API. The other modules of the application
are implementation details.

## Setup

`ecall` is an OTP application: list it in the `applications` of your own
application. It relies on Erlang distribution being connected already and
finds the other nodes that run `ecall` through `m:pg`. It reads two
parameters from its application environment:

- `pool_size`: the number of workers in this node's receiver pool. A
  positive integer, `undefined` (the default) for the number of logical
  processors, or `disabled` to run without a pool, invisible to the other
  nodes. Read once at application start.
- `batch_size`: the maximum number of requests in one batch, default 1000.

```erlang
ecall:send({my_server, 'node_b@host'}, {update, Key, Value}),
ok = ecall:cast('node_b@host', my_index, insert, [Key, Value]),
{ok, 42} = ecall:call('node_b@host', my_counter, value, [Key]),
{ok, {Node, Value}} = ecall:call_one(Replicas, my_store, read, [Key]).
```

## Results

`{error, Reason}` means a rejection and any other value means success:
`call/4` returns `{error, Reason}` unchanged and wraps every other return
value as `{ok, Value}`. Raising in the called function yields
`{error, {exit, Reason}}`. Failing to reach the node yields
`{error, {badrpc, Reason}}`.

The group functions tag every result with its node, as `{Node, Value}`, and
by default skip the nodes that answer `{badrpc, Reason}` as if they were not
in the list. `RpcErr = true` makes such a node count as an ordinary
rejection.

| function          | nodes contacted | waits for     | success                 | failure                     |
|-------------------|-----------------|---------------|-------------------------|-----------------------------|
| `send/2`          | one             | nothing       | `Message`               | none reported               |
| `cast/4`          | one             | nothing       | `ok`                    | none reported               |
| `call/4`          | one             | the reply     | `{ok, Value}`           | `{error, Reason}`           |
| `call_one/4`      | one at a time   | first success | `{ok, {Node, Value}}`   | `{error, [{Node, Reason}]}` |
| `call_any/4`      | all in parallel | first success | `{ok, {Node, Value}}`   | `{error, [{Node, Reason}]}` |
| `call_all/4`      | all in parallel | all successes | `{ok, [{Node, Value}]}` | `{error, {Node, Reason}}`   |
| `call_all_wait/4` | all in parallel | all replies   | `{Replies, Rejects}`    | never fails                 |
| `cast_one/4`      | one             | nothing       | `ok`                    | none reported               |
| `cast_all/4`      | all             | nothing       | `ok`                    | none reported               |

## Semantics

- Ordering is preserved per caller, as with `!`. Nothing is promised about
  the interleaving of different callers, and cast and call bodies run in
  independent processes.
- `send/2` and `cast/4` never suspend the caller on a busy channel. Under
  sustained overload the proxy mailboxes grow without bound, so admission
  control stays the application's job.
- Send and cast are fire-and-forget on both paths. A call returns
  `{error, {badrpc, noconnection}}` when the connection goes down and never
  hangs on a dead node.
""".

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
-doc """
Sends `Message` to `To`, like `To ! Message`, and returns `Message`.

`To` is a pid or a `{RegisteredName, Node}` tuple. Any other destination
accepted by `!`, and any destination on a node without a connection, is sent
natively. Nothing is reported back: a dead process or an unregistered name
drops the message silently, as with `!`.
""".
-spec send(erlang:send_destination(), Message) -> Message when Message :: term().
send(To, Message) ->
  ecall_connection:send(To, Message).

-doc """
Runs `Module:Function(Args...)` in a new process on `Node`, like
`erpc:cast/4`, and returns at once. Nothing is reported back.
""".
-spec cast(node(), module(), atom(), [term()]) -> ok.
cast(Node, Module, Function, Args) ->
  ecall_connection:cast(Node, Module, Function, Args).

-doc """
Calls `Module:Function(Args...)` on `Node` and waits for the result, like
`erpc:call/4` with the default `infinity` timeout.

- `{error, Reason}` returned by the function is returned unchanged. Any
  other return value `Value` is returned as `{ok, Value}`, so a function
  returning `{ok, X}` produces `{ok, {ok, X}}`.
- If the function raises, or the process running it is killed,
  `{error, {exit, Reason}}` is returned. Without a connection to `Node` a
  `throw` comes back as `{error, Thrown}` instead, as `erpc:call/4` rethrows
  it.
- `{error, {badrpc, Reason}}` is returned when `Node` cannot be reached or
  the connection goes down while the call is in flight, with
  `Reason = noconnection` in both cases.

There is no timeout argument. The call waits until the function returns or
the connection is lost, and never hangs on a dead node.

```erlang
{ok, 42} = ecall:call('node_b@host', my_counter, value, [Key]),
{error, not_found} = ecall:call('node_b@host', my_index, lookup, [MissingKey]),
{error, {exit, badarith}} = ecall:call('node_b@host', erlang, '/', [1, 0]).
```
""".
-spec call(node(), module(), atom(), [term()]) ->
  {ok, term()} | {error, term()}.
call(Node, Module, Function, Args) ->
  ecall_connection:call(Node, Module, Function, Args).

%%=================================================================
%% GROUP API
%%=================================================================
-doc #{equiv => call_one(Nodes, Module, Function, Args, false)}.
-doc """
Tries `Nodes` one at a time until one succeeds, skipping unreachable nodes.
See `call_one/5`.
""".
-spec call_one([node()], module(), atom(), [term()]) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_one(Nodes, Module, Function, Args) ->
  ecall_group:call_one(Nodes, Module, Function, Args).

-doc """
Tries `Nodes` one at a time until one of them succeeds.

The local node, if present, is tried first, then the other nodes in random
order. A node that answers `{error, Reason}` is recorded as `{Node, Reason}`
and the next one is tried. An unreachable node, `{badrpc, Reason}`, is
skipped when `RpcErr` is `false` and recorded when `RpcErr` is `true`.

Returns `{ok, {Node, Value}}` for the first success, `{error, Errors}` with
the recorded errors when every node fails, which is `{error, []}` when all
of them were skipped, or `{error, none_is_available}` when `Nodes` is empty.

Loads one node at a time, at the cost of latency when the first choice
fails. Use `call_any/5` when latency matters more than load.
""".
-spec call_one([node()], module(), atom(), [term()], boolean()) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_one(Nodes, Module, Function, Args, RpcErr) ->
  ecall_group:call_one(Nodes, Module, Function, Args, RpcErr).

-doc #{equiv => call_any(Nodes, Module, Function, Args, false)}.
-doc """
Calls all `Nodes` at the same time and takes the first success, skipping
unreachable nodes. See `call_any/5`.
""".
-spec call_any([node()], module(), atom(), [term()]) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_any(Nodes, Module, Function, Args) ->
  ecall_group:call_any(Nodes, Module, Function, Args).

-doc """
Calls all `Nodes` at the same time and takes the first success.

The local node, if present, is called first and alone. If it succeeds, the
same call is cast to the other nodes with `cast_all/4` and the local result
is returned without waiting for them. If it fails, its error is recorded and
the other nodes are called in parallel. Later successes are discarded,
although the function has been started on every node. A node that answers
`{error, Reason}` is recorded as `{Node, Reason}`. An unreachable node, or
one whose caller process was killed on the calling node, `{badrpc, Reason}`,
is skipped when `RpcErr` is `false` and recorded when `RpcErr` is `true`.

Returns `{ok, {Node, Value}}` for the first success, `{error, Errors}` with
the recorded errors when every node fails, or `{error, none_is_available}`
when `Nodes` is empty or no error was recorded.

The parallel calls are coordinated by a helper process. If it is killed or
crashes, the function exits with
`{Reason, {ecall, call_any, [Nodes, Module, Function, Args, RpcErr]}}`.
""".
-spec call_any([node()], module(), atom(), [term()], boolean()) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_any(Nodes, Module, Function, Args, RpcErr) ->
  ecall_group:call_any(Nodes, Module, Function, Args, RpcErr).

-doc #{equiv => call_all(Nodes, Module, Function, Args, false)}.
-doc """
Calls all `Nodes` in parallel and requires every reachable one to succeed.
See `call_all/5`.
""".
-spec call_all([node()], module(), atom(), [term()]) ->
  {ok, [{node(), term()}]} | {error, none_is_available | {node(), term()}}.
call_all(Nodes, Module, Function, Args) ->
  ecall_group:call_all(Nodes, Module, Function, Args).

-doc """
Calls all `Nodes` in parallel and requires every one of them to succeed.

The first `{error, Reason}` answer ends the call with
`{error, {Node, Reason}}`. The results of the other nodes are discarded,
although the function has been started on every node. When all nodes
succeed, the results are returned as `{ok, [{Node, Value}]}` in the order
the replies arrived. An unreachable node, or one whose caller process was
killed on the calling node, `{badrpc, Reason}`, is skipped when `RpcErr` is
`false` and ends the call like any other error when `RpcErr` is `true`.

Returns `{error, none_is_available}` when `Nodes` is empty or, with
`RpcErr = false`, when no node could be reached.

The calls are coordinated by a helper process. If it is killed or crashes,
the function exits with
`{Reason, {ecall, call_all, [Nodes, Module, Function, Args, RpcErr]}}`.
""".
-spec call_all([node()], module(), atom(), [term()], boolean()) ->
  {ok, [{node(), term()}]} | {error, none_is_available | {node(), term()}}.
call_all(Nodes, Module, Function, Args, RpcErr) ->
  ecall_group:call_all(Nodes, Module, Function, Args, RpcErr).

-doc """
Calls all `Nodes` in parallel, waits for every one of them and returns both
the successes and the failures.

`Replies` is `[{Node, Value}]` for the nodes that succeeded. `Rejects` is
`[{Node, Reason}]` for the nodes that answered `{error, Reason}`, were
unreachable or whose caller process was killed on the calling node, the last
two as `{badrpc, Reason}`. Nothing is fatal, each list is in the order the
replies arrived, and an empty `Nodes` returns `{[], []}`.

The calls are coordinated by a helper process. If it is killed or crashes,
the function exits with
`{Reason, {ecall, call_all_wait, [Nodes, Module, Function, Args]}}`.
""".
-spec call_all_wait([node()], module(), atom(), [term()]) ->
  {[{node(), term()}], [{node(), term()}]}.
call_all_wait(Nodes, Module, Function, Args) ->
  ecall_group:call_all_wait(Nodes, Module, Function, Args).

-doc """
Casts to one of `Nodes` and returns at once: to the local node if present,
otherwise to a random one. `Nodes` must not be empty, an empty list fails
with `badarg`.
""".
-spec cast_one(nonempty_list(node()), module(), atom(), [term()]) -> ok.
cast_one(Nodes, Module, Function, Args) ->
  ecall_group:cast_one(Nodes, Module, Function, Args).

-doc """
Casts to every node in `Nodes`, the local node included if present, and
returns at once. An empty `Nodes` does nothing.
""".
-spec cast_all([node()], module(), atom(), [term()]) -> ok.
cast_all(Nodes, Module, Function, Args) ->
  ecall_group:cast_all(Nodes, Module, Function, Args).

%%=================================================================
%% CONNECTION API
%%=================================================================
-doc """
Starts a connection to `Node` if there is none.

Asynchronous: poll `connection_info/1` for the result. Not needed in normal
operation, discovery through `m:pg` connects every node that runs `ecall`.
""".
-spec start_connection(node()) -> ok | {error, term()}.
start_connection(Node) ->
  ecall_connection:connect(Node).

-doc """
Removes the connection to `Node`.

Traffic to `Node` takes the native path until `m:pg` announces it again or
`start_connection/1` is called. A call in flight returns
`{error, {badrpc, noconnection}}`.
""".
-spec stop_connection(node()) -> ok.
stop_connection(Node) ->
  ecall_connection:disconnect(Node).

-doc """
Reports the state of this node's connection to `Node`.

- `connected`: a pool of `proxy_count` proxies is in use.
- `down`: the connection exists but has no pool, because `Node` has not
  answered yet, went away, or runs with `pool_size` set to `disabled`.
- `{error, not_connected}`: there is no connection to `Node`.

```erlang
1> ecall:connection_info('node_b@host').
{ok,#{status => connected,
      connection_pid => <0.245.0>,
      proxy_count => 48}}
2> ecall:connection_info('node_c@host').
{ok,#{status => down, connection_pid => <0.301.0>}}
3> ecall:connection_info('unknown@host').
{error,not_connected}
```
""".
-spec connection_info(node()) ->
  {ok, #{status := connected, connection_pid := pid(),
         proxy_count := pos_integer()}}
  | {ok, #{status := down, connection_pid := pid()}}
  | {error, not_connected}.
connection_info(Node) ->
  ecall_connection:connection_info(Node).
