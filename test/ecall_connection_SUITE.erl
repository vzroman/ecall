-module(ecall_connection_SUITE).

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
  sender_pid_routes_all_entry_points/1,
  ecall_api_delegates_to_connection/1,
  connection_info_reports_routing_metadata/1,
  connect_is_idempotent/1,
  reconnect_uses_updated_batch_env/1,
  disconnect_is_idempotent/1,
  pool_member_loss_invalidates_connection_pid/1,
  worker_uses_configured_batch_size/1,
  incarnation_death_switches_to_native_path/1,
  call_reply_takes_native_path_when_incarnation_is_dead/1,
  inflight_call_fails_with_noconnection/1,
  join_with_same_master_is_noop/1,
  join_with_new_master_rebuilds_pool/1,
  master_restart_reconnects_through_registered_name/1,
  connection_info_reports_down_without_pool/1,
  empty_remote_pool_stays_on_native_path/1,
  ecall_start_stop_connection/1
]).

%%=================================================================
%% COMMON TEST API
%%=================================================================
all() ->
  [
    sender_pid_routes_all_entry_points,
    ecall_api_delegates_to_connection,
    connection_info_reports_routing_metadata,
    connect_is_idempotent,
    reconnect_uses_updated_batch_env,
    disconnect_is_idempotent,
    pool_member_loss_invalidates_connection_pid,
    worker_uses_configured_batch_size,
    incarnation_death_switches_to_native_path,
    call_reply_takes_native_path_when_incarnation_is_dead,
    inflight_call_fails_with_noconnection,
    join_with_same_master_is_noop,
    join_with_new_master_rebuilds_pool,
    master_restart_reconnects_through_registered_name,
    connection_info_reports_down_without_pool,
    empty_remote_pool_stays_on_native_path,
    ecall_start_stop_connection
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  case whereis(ecall_connection_sup) of
    Sup when is_pid(Sup) ->
      gen_server:stop(Sup);
    undefined ->
      ok
  end,
  ok.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _Config) ->
  application:unset_env(ecall, batch_size),
  ok.

%%=================================================================
%% TEST CASES
%%=================================================================
sender_pid_routes_all_entry_points(_Config) ->
  run_route_test(ecall_connection).

ecall_api_delegates_to_connection(_Config) ->
  run_route_test(ecall).

connection_info_reports_routing_metadata(_Config) ->
  Node = node(),
  Remote = fake_remote(),
  with_fake_receive([Remote],
    fun() ->
      application:set_env(ecall, batch_size, 7),
      ok = ecall_connection:connect(Node),
      Info = wait_until_connected(Node),
      connected = maps:get(status, Info),
      7 = maps:get(batch_size, Info),
      ProxyCount = maps:get(proxy_count, Info),
      true = is_integer(ProxyCount) andalso ProxyCount > 0,
      ConnectionPid = maps:get(connection_pid, Info),
      true = is_pid(ConnectionPid),
      true = erlang:is_process_alive(ConnectionPid)
    end).

connect_is_idempotent(_Config) ->
  Node = node(),
  Remote = fake_remote(),
  with_fake_receive([Remote],
    fun() ->
      ok = ecall_connection:connect(Node),
      Info1 = wait_until_connected(Node),
      ConnectionPid = maps:get(connection_pid, Info1),

      ok = ecall_connection:connect(Node),
      {ok, Info2} = ecall_connection:connection_info(Node),
      connected = maps:get(status, Info2),
      ConnectionPid = maps:get(connection_pid, Info2)
    end).

reconnect_uses_updated_batch_env(_Config) ->
  Node = node(),
  Remote = fake_remote(),
  with_fake_receive([Remote],
    fun() ->
      application:set_env(ecall, batch_size, 2),
      ok = ecall_connection:connect(Node),
      Info1 = wait_until_connected(Node),
      Pid1 = maps:get(connection_pid, Info1),
      2 = maps:get(batch_size, Info1),

      ok = ecall_connection:disconnect(Node),
      wait_until_dead(Pid1),

      application:set_env(ecall, batch_size, 3),
      ok = ecall_connection:connect(Node),
      Info2 = wait_until_connected(Node),
      Pid2 = maps:get(connection_pid, Info2),

      true = Pid1 =/= Pid2,
      3 = maps:get(batch_size, Info2),
      wait_until_dead(Pid1)
    end).

disconnect_is_idempotent(_Config) ->
  Node = node(),
  Remote = fake_remote(),
  with_fake_receive([Remote],
    fun() ->
      application:set_env(ecall, batch_size, 5),
      ok = ecall_connection:connect(Node),
      Info = wait_until_connected(Node),
      Pid = maps:get(connection_pid, Info),

      ok = ecall_connection:disconnect(Node),
      wait_until_dead(Pid),
      false = erlang:is_process_alive(Pid),
      {error, not_connected} = ecall_connection:connection_info(Node),

      ok = ecall_connection:disconnect(Node),
      {error, not_connected} = ecall_connection:connection_info(Node)
    end).

pool_member_loss_invalidates_connection_pid(_Config) ->
  Node = node(),
  Remote = fake_remote(),
  with_fake_receive([Remote],
    fun() ->
      application:set_env(ecall, batch_size, 6),
      ok = ecall_connection:connect(Node),
      Info = wait_until_connected(Node),
      Pid = maps:get(connection_pid, Info),
      Proxy = only_proxy_for_low_level_batch_test(Info),

      exit(Proxy, kill),
      wait_until_dead(Pid),
      wait_until_connection_pid_invalidated(Node, Pid)
    end).

worker_uses_configured_batch_size(_Config) ->
  Node = node(),
  Parent = self(),
  Remote = spawn_link(fun() -> batch_remote(Parent) end),
  with_fake_receive([Remote],
    fun() ->
      application:set_env(ecall, batch_size, 2),
      ok = ecall_connection:connect(Node),
      Info = wait_until_connected(Node),
      Proxy = only_proxy_for_low_level_batch_test(Info),
      true = erlang:suspend_process(Proxy),
      try
        [ ecall_connection:send(self(), {batch_probe, I})
          || I <- lists:seq(1, 5) ]
      after
        true = erlang:resume_process(Proxy)
      end,

      assert_batch_size(2),
      assert_batch_size(2),
      assert_batch_size(1)
    end).

incarnation_death_switches_to_native_path(_Config) ->
  Node = node(),
  Self = self(),
  process_flag(trap_exit, true),
  Remote = spawn_link(fun() -> forward_remote(Self) end),
  with_fake_receive([Remote],
    fun() ->
      ok = ecall_connection:connect(Node),
      wait_until_connected(Node),

      ecall_connection:send(Self, probe_via_proxy),
      wait_for({batch, Remote, [{send, Self, probe_via_proxy}]}),

      kill_fake_receive(whereis(ecall_receive)),
      wait_until_status(Node, down),

      ecall_connection:send(Self, probe_native),
      wait_for(probe_native),
      {ok, Node} = ecall_connection:call(Node, erlang, node, []),
      ok = ecall_connection:cast(Node, erlang, send, [Self, cast_native]),
      wait_for(cast_native)
    end).

% send/2 is the plain lookup: while the master has not unpublished the
% stale pool yet, it still routes through it. call_reply/2 checks the
% incarnation and goes native the moment the remote receive master is gone.
call_reply_takes_native_path_when_incarnation_is_dead(_Config) ->
  Node = node(),
  Self = self(),
  process_flag(trap_exit, true),
  Remote = spawn_link(fun() -> forward_remote(Self) end),
  with_fake_receive([Remote],
    fun() ->
      ok = ecall_connection:connect(Node),
      Info = wait_until_connected(Node),
      Master = maps:get(connection_pid, Info),

      ok = ecall_connection:call_reply(Self, {reply, 1}),
      wait_for({batch, Remote, [{send, Self, {reply, 1}}]}),

      Receive = whereis(ecall_receive),
      {links, Links} = process_info(Receive, links),
      [Incarnation] = [Pid || Pid <- Links, Pid =/= Self],

      % hold the master so the stale pool stays published
      true = erlang:suspend_process(Master),
      try
        kill_fake_receive(Receive),
        wait_until_dead(Incarnation),
        {ok, #{status := connected}} = ecall_connection:connection_info(Node),

        ok = ecall_connection:call_reply(Self, {reply, 2}),
        wait_for({reply, 2}),

        ecall_connection:send(Self, plain_send),
        wait_for({batch, Remote, [{send, Self, plain_send}]})
      after
        true = erlang:resume_process(Master)
      end,
      wait_until_status(Node, down)
    end).

inflight_call_fails_with_noconnection(_Config) ->
  Node = node(),
  Self = self(),
  process_flag(trap_exit, true),
  Remote = spawn_link(fun() -> forward_remote(Self) end),
  with_fake_receive([Remote],
    fun() ->
      ok = ecall_connection:connect(Node),
      wait_until_connected(Node),

      Caller =
        spawn(fun() ->
          Self ! {call_result, ecall_connection:call(Node, m, f, [])}
        end),
      receive
        {batch, Remote, [{call, _Ref, Caller, m, f, []}]} ->
          ok
      after
        1000 ->
          ct:fail(call_request_not_shipped)
      end,

      kill_fake_receive(whereis(ecall_receive)),
      receive
        {call_result, Result} ->
          {error, {badrpc, noconnection}} = Result
      after
        1000 ->
          ct:fail(inflight_call_hangs)
      end
    end).

join_with_same_master_is_noop(_Config) ->
  Node = node(),
  Remote = fake_remote(),
  with_fake_receive([Remote],
    fun() ->
      ok = ecall_connection:connect(Node),
      Info1 = wait_until_connected(Node),
      Proxy1 = only_proxy_for_low_level_batch_test(Info1),

      ok = ecall_connection:connect(Node, whereis(ecall_receive)),
      timer:sleep(100),

      {ok, Info1} = ecall_connection:connection_info(Node),
      Proxy1 = only_proxy_for_low_level_batch_test(Info1),
      true = erlang:is_process_alive(Proxy1)
    end).

join_with_new_master_rebuilds_pool(_Config) ->
  Node = node(),
  Self = self(),
  Remote1 = spawn_link(fun() -> forward_remote(Self) end),
  Remote2 = spawn_link(fun() -> forward_remote(Self) end),
  with_fake_receive([Remote1],
    fun() ->
      ok = ecall_connection:connect(Node),
      Info1 = wait_until_connected(Node),
      Proxy1 = only_proxy_for_low_level_batch_test(Info1),
      Receive1 = whereis(ecall_receive),

      ecall_connection:send(Self, probe1),
      wait_for({batch, Remote1, [{send, Self, probe1}]}),

      Receive2 = start_fake_receive([Remote2]),
      try
        ok = ecall_connection:connect(Node, Receive2),
        Info2 = wait_until_proxy_changed(Node, Proxy1),
        true =
          maps:get(connection_pid, Info1) =:= maps:get(connection_pid, Info2),
        wait_until_dead(Proxy1),

        ecall_connection:send(Self, probe2),
        wait_for({batch, Remote2, [{send, Self, probe2}]}),
        true = erlang:is_process_alive(Receive1)
      after
        Receive2 ! fake_receive_stop
      end
    end),
  Remote2 ! fake_remote_stop,
  ok.

master_restart_reconnects_through_registered_name(_Config) ->
  Node = node(),
  Remote = fake_remote(),
  with_fake_receive([Remote],
    fun() ->
      ok = ecall_connection:connect(Node),
      Info1 = wait_until_connected(Node),
      Master1 = maps:get(connection_pid, Info1),

      exit(Master1, kill),
      wait_until_dead(Master1),

      Info2 = wait_until_reconnected(Node, Master1),
      Master2 = maps:get(connection_pid, Info2),
      true = Master1 =/= Master2,
      true = erlang:is_process_alive(Master2)
    end).

connection_info_reports_down_without_pool(_Config) ->
  Node = node(),
  with_silent_receive(
    fun() ->
      ok = ecall_connection:connect(Node),
      {ok, #{status := down, connection_pid := Master}} =
        ecall_connection:connection_info(Node),
      true = erlang:is_process_alive(Master),

      timer:sleep(100),
      {ok, #{status := down, connection_pid := Master}} =
        ecall_connection:connection_info(Node),
      % nothing is published while there is no pool
      undefined = persistent_term:get({ecall_connection, Node}, undefined),

      ok = ecall_connection:disconnect(Node),
      wait_until_dead(Master),
      {error, not_connected} = ecall_connection:connection_info(Node)
    end).

empty_remote_pool_stays_on_native_path(_Config) ->
  Node = node(),
  Self = self(),
  with_fake_receive([],
    fun() ->
      ok = ecall_connection:connect(Node),
      timer:sleep(100),
      {ok, #{status := down}} = ecall_connection:connection_info(Node),

      ecall_connection:send(Self, probe_native),
      wait_for(probe_native),
      {ok, #{status := down}} = ecall_connection:connection_info(Node)
    end).

ecall_start_stop_connection(_Config) ->
  Node = node(),
  Remote = fake_remote(),
  with_fake_receive([Remote],
    fun() ->
      ok = ecall:start_connection(Node),
      Info = wait_until_connected(Node),
      Master = maps:get(connection_pid, Info),

      ok = ecall:stop_connection(Node),
      wait_until_dead(Master),
      {error, not_connected} = ecall:connection_info(Node)
    end).

run_route_test(ApiModule) ->
  Parent = self(),
  Node = node(),
  Remote = spawn_link(fun() -> route_test_remote(Parent) end),
  with_fake_receive([Remote],
    fun() ->
      application:set_env(ecall, batch_size, 16),
      ok = ecall_connection:connect(Node),
      wait_until_connected(Node),
      Sender =
        spawn(fun() -> route_test_sender(Parent, Node, ApiModule) end),

      wait_for({route_test_ready, Sender}),
      TargetPid = route_test_target_pid(),
      Sender ! {route_test_run, TargetPid},

      Requests = collect_route_requests(Sender, []),
      assert_route_requests(TargetPid, Sender, Requests),
      TargetPid ! route_test_stop
    end).

%%=================================================================
%% ROUTING TEST UTILITIES
%%=================================================================
route_test_remote(Parent) ->
  receive
    {batch, Requests} ->
      Parent ! {route_test_batch, Requests},
      reply_to_call_requests(Requests),
      route_test_remote(Parent);
    fake_remote_stop ->
      ok
  end.

reply_to_call_requests(Requests) ->
  [ ClientPID ! {Ref, route_test_call_reply}
    || {call, Ref, ClientPID, _Module, _Function, _Args} <- Requests ],
  ok.

route_test_sender(Parent, Node, ApiModule) ->
  Parent ! {route_test_ready, self()},
  receive
    {route_test_run, TargetPid} ->
      TupleSend =
        ApiModule:send({TargetPid, Node}, route_test_tuple_message),
      PidSend = ApiModule:send(TargetPid, route_test_pid_message),
      Cast =
        ApiModule:cast(Node, route_test_module, route_test_function,
                       [route_test_arg]),
      Call =
        ApiModule:call(Node, route_test_module, route_test_function,
                       [route_test_arg]),
      Parent ! {route_test_done, self(), TupleSend, PidSend, Cast, Call}
  end.

route_test_target_pid() ->
  spawn(fun() ->
    receive
      route_test_stop ->
        ok
    end
  end).

collect_route_requests(Sender, Acc) ->
  receive
    {route_test_batch, Requests} ->
      collect_route_requests(Sender, Requests ++ Acc);
    {route_test_done, Sender,
     route_test_tuple_message,
     route_test_pid_message,
     ok,
     {ok, route_test_call_reply}} ->
      drain_route_requests(Acc);
    Other ->
      ct:fail({unexpected_route_message, Sender, Other})
  after
    1000 ->
      ct:fail({route_test_timeout, Sender})
  end.

drain_route_requests(Acc) ->
  receive
    {route_test_batch, Requests} ->
      drain_route_requests(Requests ++ Acc)
  after
    0 ->
      Acc
  end.

assert_route_requests(TargetPid, Sender, Requests) ->
  true =
    lists:member({send, TargetPid, route_test_tuple_message}, Requests),
  true =
    lists:member({send, TargetPid, route_test_pid_message}, Requests),
  true =
    lists:member(
      {cast, route_test_module, route_test_function, [route_test_arg]},
      Requests),
  true =
    lists:any(fun(Request) -> call_request(Sender, Request) end, Requests).

call_request(Sender,
             {call, Ref, Sender, route_test_module, route_test_function,
              [route_test_arg]}) when is_reference(Ref) ->
  true;
call_request(_Sender, _Request) ->
  false.

%%=================================================================
%% FAKE RECEIVE POOL
%%=================================================================
ensure_connection_supervisor() ->
  case ecall_connection_sup:start_link() of
    {ok, Pid} ->
      unlink(Pid),
      ok;
    {error, {already_started, _Pid}} ->
      ok
  end.

with_fake_receive(Workers, Fun) ->
  with_receive(fun() -> fake_receive_loop(Workers) end, Workers, Fun).

% A receive master that never answers get_workers.
with_silent_receive(Fun) ->
  with_receive(fun silent_receive_loop/0, [], Fun).

with_receive(Loop, Workers, Fun) ->
  Node = node(),
  ensure_connection_supervisor(),
  ok = ecall_connection:disconnect(Node),
  undefined = whereis(ecall_receive),
  Receive = start_receive(Loop),
  true = register(ecall_receive, Receive),
  try
    Fun()
  after
    ok = ecall_connection:disconnect(Node),
    unregister_fake_receive(Receive),
    Receive ! fake_receive_stop,
    stop_workers(Workers)
  end.

start_fake_receive(Workers) ->
  start_receive(fun() -> fake_receive_loop(Workers) end).

% The real receive master traps exits: the incarnation processes of the
% connections link to it and their exits must not take it down.
start_receive(Loop) ->
  spawn_link(fun() ->
    process_flag(trap_exit, true),
    Loop()
  end).

fake_receive_loop(Workers) ->
  receive
    {get_workers, Ref, From} ->
      From ! {Ref, self(), Workers},
      fake_receive_loop(Workers);
    {'EXIT', _Pid, _Reason} ->
      fake_receive_loop(Workers);
    fake_receive_stop ->
      ok
  end.

silent_receive_loop() ->
  receive
    fake_receive_stop ->
      ok;
    _Other ->
      silent_receive_loop()
  end.

% The fake receive is linked to the test process, which must trap exits.
kill_fake_receive(Receive) ->
  exit(Receive, kill),
  receive
    {'EXIT', Receive, killed} ->
      ok
  after
    1000 ->
      ct:fail({fake_receive_not_killed, Receive})
  end.

unregister_fake_receive(Receive) ->
  case whereis(ecall_receive) of
    Receive ->
      unregister(ecall_receive);
    _Other ->
      ok
  end.

fake_remote() ->
  spawn_link(fun() -> fake_remote_loop() end).

fake_remote_loop() ->
  receive
    fake_remote_stop ->
      ok;
    _Other ->
      fake_remote_loop()
  end.

batch_remote(Parent) ->
  receive
    {batch, Requests} ->
      Parent ! {batch_size, length(Requests)},
      batch_remote(Parent);
    fake_remote_stop ->
      ok
  end.

% Forwards every batch tagged with its own pid, so a test can tell
% which remote worker a request reached.
forward_remote(Parent) ->
  receive
    {batch, Requests} ->
      Parent ! {batch, self(), Requests},
      forward_remote(Parent);
    fake_remote_stop ->
      ok
  end.

stop_workers(Workers) ->
  [Worker ! fake_remote_stop || Worker <- Workers],
  ok.

%%=================================================================
only_proxy_for_low_level_batch_test(Info) ->
  [Proxy] = proxies_of(Info),
  Proxy.

proxies_of(Info) ->
  ConnectionPid = maps:get(connection_pid, Info),
  {links, Links} = process_info(ConnectionPid, links),
  Supervisor = whereis(ecall_connection_sup),
  [Pid || Pid <- Links, Pid =/= Supervisor].

assert_batch_size(ExpectedSize) ->
  receive
    {batch_size, ExpectedSize} ->
      ok;
    Other ->
      ct:fail({unexpected_batch_size, ExpectedSize, Other})
  after
    1000 ->
      ct:fail({batch_size_timeout, ExpectedSize})
  end.

wait_for(Expected) ->
  receive
    Expected ->
      ok;
    Other ->
      ct:fail({unexpected_message, Expected, Other})
  after
    1000 ->
      ct:fail({timeout, Expected})
  end.

wait_until_dead(Pid) ->
  wait_until_dead(Pid, _Attempts = 50).

wait_until_dead(Pid, Attempts) when Attempts > 0 ->
  case erlang:is_process_alive(Pid) of
    false ->
      ok;
    true ->
      timer:sleep(20),
      wait_until_dead(Pid, Attempts - 1)
  end;
wait_until_dead(Pid, 0) ->
  ct:fail({process_still_alive, Pid}).

wait_until_connected(Node) ->
  wait_until_status(Node, connected).

wait_until_status(Node, Status) ->
  wait_until_status(Node, Status, _Attempts = 50).

wait_until_status(_Node, Status, 0) ->
  ct:fail({status_not_reached, Status});
wait_until_status(Node, Status, Attempts) ->
  case ecall_connection:connection_info(Node) of
    {ok, #{status := Status} = Info} ->
      Info;
    _ ->
      timer:sleep(20),
      wait_until_status(Node, Status, Attempts - 1)
  end.

wait_until_proxy_changed(Node, OldProxy) ->
  wait_until_proxy_changed(Node, OldProxy, _Attempts = 50).

wait_until_proxy_changed(_Node, _OldProxy, 0) ->
  ct:fail(proxy_not_changed);
wait_until_proxy_changed(Node, OldProxy, Attempts) ->
  case ecall_connection:connection_info(Node) of
    {ok, #{status := connected} = Info} ->
      case proxies_of(Info) of
        [NewProxy] when NewProxy =/= OldProxy ->
          Info;
        _Transient ->
          timer:sleep(20),
          wait_until_proxy_changed(Node, OldProxy, Attempts - 1)
      end;
    _ ->
      timer:sleep(20),
      wait_until_proxy_changed(Node, OldProxy, Attempts - 1)
  end.

wait_until_reconnected(Node, OldMaster) ->
  wait_until_reconnected(Node, OldMaster, _Attempts = 50).

wait_until_reconnected(_Node, _OldMaster, 0) ->
  ct:fail(connection_not_reestablished);
wait_until_reconnected(Node, OldMaster, Attempts) ->
  case ecall_connection:connection_info(Node) of
    {ok, #{status := connected, connection_pid := Master} = Info}
      when Master =/= OldMaster ->
      Info;
    _ ->
      timer:sleep(20),
      wait_until_reconnected(Node, OldMaster, Attempts - 1)
  end.

wait_until_connection_pid_invalidated(Node, OldPid) ->
  wait_until_connection_pid_invalidated(Node, OldPid, _Attempts = 50).

wait_until_connection_pid_invalidated(_Node, _OldPid, 0) ->
  ct:fail(connection_pid_not_invalidated);
wait_until_connection_pid_invalidated(Node, OldPid, Attempts) ->
  case ecall_connection:connection_info(Node) of
    {error, not_connected} ->
      ok;
    {ok, #{connection_pid := NewPid}} when NewPid =/= OldPid ->
      ok;
    {ok, #{connection_pid := OldPid}} ->
      timer:sleep(20),
      wait_until_connection_pid_invalidated(Node, OldPid, Attempts - 1)
  end.
