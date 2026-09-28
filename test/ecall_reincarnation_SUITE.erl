-module(ecall_reincarnation_SUITE).

%% End-to-end: a peer node running the real ecall application is crashed and
%% restarted under the same name, and its ecall pool is restarted in place.
%% The test node must be distributed (rebar3 ct --sname ...).

-include_lib("common_test/include/ct.hrl").

%%=================================================================
%% COMMON TEST API
%%=================================================================
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

%%=================================================================
%% TEST CASES
%%=================================================================
-export([
  reply_survives_peer_reincarnation/1,
  remote_pool_restart_rebuilds_connection/1,
  disabled_pool_is_invisible_but_can_send/1
]).

%%=================================================================
%% COMMON TEST API
%%=================================================================
all() ->
  [
    reply_survives_peer_reincarnation,
    remote_pool_restart_rebuilds_connection,
    disabled_pool_is_invisible_but_can_send
  ].

init_per_suite(Config) ->
  case erlang:is_alive() of
    true ->
      ok;
    false ->
      ct:fail("the test node is not distributed, run rebar3 ct with --sname")
  end,
  {ok, _} = application:ensure_all_started(ecall),
  Config.

end_per_suite(_Config) ->
  application:stop(ecall),
  ok.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _Config) ->
  ok.

%%=================================================================
%% TEST CASES
%%=================================================================
reply_survives_peer_reincarnation(_Config) ->
  process_flag(trap_exit, true),
  Local = node(),
  Name = ecall_reincarnation_peer,

  {Peer1, Node} = start_peer(Name),
  wait_both_connected(Peer1, Node),
  {ok, Local} = peer:call(Peer1, ecall, call, [Local, erlang, node, []], 5000),
  {ok, #{status := connected, connection_pid := Master}} =
    ecall:connection_info(Node),

  crash_peer(Peer1),
  wait_until_status(Node, down),

  % same node name, new creation
  {Peer2, Node} = start_peer(Name),
  wait_both_connected(Peer2, Node),
  [ {ok, Local} =
      peer:call(Peer2, ecall, call, [Local, erlang, node, []], 5000)
    || _ <- lists:seq(1, 20) ],

  % the master survived, only the incarnation and the pool were rebuilt
  {ok, #{status := connected, connection_pid := Master}} =
    ecall:connection_info(Node),

  stop_peer(Peer2),
  ok = ecall:stop_connection(Node),
  {error, not_connected} = ecall:connection_info(Node).

remote_pool_restart_rebuilds_connection(_Config) ->
  process_flag(trap_exit, true),
  Local = node(),

  {Peer, Node} = start_peer(ecall_pool_restart_peer),
  wait_both_connected(Peer, Node),
  {ok, #{status := connected, connection_pid := Master}} =
    ecall:connection_info(Node),

  Receive1 = peer:call(Peer, erlang, whereis, [ecall_receive]),
  true = is_pid(Receive1),

  % Hold the local pg monitor: the invalidation must not depend on pg,
  % and the rebuild must not race the down check.
  ok = sys:suspend(ecall_pg_monitor),
  try
    true = peer:call(Peer, erlang, exit, [Receive1, kill]),
    wait_until_status(Node, down),
    {ok, #{status := down}} = ecall:connection_info(Node)
  after
    ok = sys:resume(ecall_pg_monitor)
  end,

  wait_both_connected(Peer, Node),
  Receive2 = peer:call(Peer, erlang, whereis, [ecall_receive]),
  true = is_pid(Receive2) andalso Receive2 =/= Receive1,
  {ok, #{status := connected, connection_pid := Master}} =
    ecall:connection_info(Node),
  {ok, Local} = peer:call(Peer, ecall, call, [Local, erlang, node, []], 5000),

  stop_peer(Peer),
  ok = ecall:stop_connection(Node),
  {error, not_connected} = ecall:connection_info(Node).

% pool_size = disabled: no receive pool here, so the peer never connects to
% this node, while this node connects to the peer and the replies to its
% calls come back natively.
disabled_pool_is_invisible_but_can_send(_Config) ->
  process_flag(trap_exit, true),
  Local = node(),

  ok = application:stop(ecall),
  ok = application:set_env(ecall, pool_size, disabled),
  ok = application:start(ecall),
  try
    undefined = whereis(ecall_receive),
    {Peer, Node} = start_peer(ecall_disabled_pool_peer),
    try
      wait_until_status(Node, connected),
      timer:sleep(500),
      {error, not_connected} =
        peer:call(Peer, ecall, connection_info, [Local]),

      {ok, Node} = ecall:call(Node, erlang, node, []),
      {ok, Local} =
        peer:call(Peer, ecall, call, [Local, erlang, node, []], 5000)
    after
      stop_peer(Peer)
    end
  after
    application:stop(ecall),
    application:unset_env(ecall, pool_size),
    {ok, _} = application:ensure_all_started(ecall)
  end,
  ok.

%%=================================================================
%% PEER UTILITIES
%%=================================================================
% The control connection goes over standard_io, so it does not depend on
% the distribution link the tests break. The peer connects to this node,
% like a restarted node joining the cluster.
start_peer(Name) ->
  {ok, Peer, Node} =
    peer:start_link(#{
      name => Name,
      longnames => false,
      connection => standard_io,
      args =>
        ["-pa" | code:get_path()] ++
        ["-setcookie", atom_to_list(erlang:get_cookie())],
      wait_boot => 10000
    }),
  {ok, _} = peer:call(Peer, application, ensure_all_started, [ecall]),
  true = peer:call(Peer, net_kernel, connect_node, [node()]),
  {Peer, Node}.

% Hard crash: the peer control process is linked and exits when the node
% goes away, the test process traps exits.
crash_peer(Peer) ->
  catch peer:call(Peer, erlang, halt, []),
  receive
    {'EXIT', Peer, _Reason} ->
      ok
  after
    5000 ->
      catch peer:stop(Peer),
      ct:fail(peer_did_not_crash)
  end.

stop_peer(Peer) ->
  ok = peer:stop(Peer),
  receive
    {'EXIT', Peer, _Reason} ->
      ok
  after
    5000 ->
      ct:fail(peer_did_not_stop)
  end.

%%=================================================================
%% POLLING UTILITIES
%%=================================================================
wait_both_connected(Peer, Node) ->
  wait_both_connected(Peer, Node, _Attempts = 250).

wait_both_connected(_Peer, _Node, 0) ->
  ct:fail(not_both_connected);
wait_both_connected(Peer, Node, Attempts) ->
  LocalInfo = ecall:connection_info(Node),
  PeerInfo = peer:call(Peer, ecall, connection_info, [node()]),
  case {LocalInfo, PeerInfo} of
    {{ok, #{status := connected}}, {ok, #{status := connected}}} ->
      ok;
    _ ->
      timer:sleep(20),
      wait_both_connected(Peer, Node, Attempts - 1)
  end.

wait_until_status(Node, Status) ->
  wait_until_status(Node, Status, _Attempts = 250).

wait_until_status(_Node, Status, 0) ->
  ct:fail({status_not_reached, Status});
wait_until_status(Node, Status, Attempts) ->
  case ecall:connection_info(Node) of
    {ok, #{status := Status} = Info} ->
      Info;
    _ ->
      timer:sleep(20),
      wait_until_status(Node, Status, Attempts - 1)
  end.
