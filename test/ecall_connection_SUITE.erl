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
  worker_uses_configured_batch_size/1
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
    worker_uses_configured_batch_size
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
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
      {ok, Info} = ecall_connection:connection_info(Node),
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
      {ok, Info1} = ecall_connection:connection_info(Node),
      ConnectionPid = maps:get(connection_pid, Info1),

      ok = ecall_connection:connect(Node),
      {ok, Info2} = ecall_connection:connection_info(Node),
      ConnectionPid = maps:get(connection_pid, Info2)
    end).

reconnect_uses_updated_batch_env(_Config) ->
  Node = node(),
  Remote = fake_remote(),
  with_fake_receive([Remote],
    fun() ->
      application:set_env(ecall, batch_size, 2),
      ok = ecall_connection:connect(Node),
      {ok, Info1} = ecall_connection:connection_info(Node),
      Pid1 = maps:get(connection_pid, Info1),
      2 = maps:get(batch_size, Info1),

      ok = ecall_connection:disconnect(Node),
      wait_until_dead(Pid1),

      application:set_env(ecall, batch_size, 3),
      ok = ecall_connection:connect(Node),
      {ok, Info2} = ecall_connection:connection_info(Node),
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
      {ok, Info} = ecall_connection:connection_info(Node),
      Pid = maps:get(connection_pid, Info),

      ok = ecall_connection:disconnect(Node),
      wait_until_dead(Pid),
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
      {ok, Info} = ecall_connection:connection_info(Node),
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
      {ok, Info} = ecall_connection:connection_info(Node),
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

run_route_test(ApiModule) ->
  Parent = self(),
  Node = node(),
  Remote = spawn_link(fun() -> route_test_remote(Parent) end),
  with_fake_receive([Remote],
    fun() ->
      application:set_env(ecall, batch_size, 16),
      ok = ecall_connection:connect(Node),
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
    {batch, _Node, Requests} ->
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
  Node = node(),
  ensure_connection_supervisor(),
  ok = ecall_connection:disconnect(Node),
  undefined = whereis(ecall_receive),
  Receive = spawn_link(fun() -> fake_receive_loop(Workers) end),
  true = register(ecall_receive, Receive),
  try
    Fun()
  after
    ok = ecall_connection:disconnect(Node),
    unregister_fake_receive(Receive),
    Receive ! fake_receive_stop,
    stop_workers(Workers)
  end.

fake_receive_loop(Workers) ->
  receive
    {get_workers, Ref, From} ->
      From ! {Ref, Workers},
      fake_receive_loop(Workers);
    fake_receive_stop ->
      ok
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
    {batch, _Node, Requests} ->
      Parent ! {batch_size, length(Requests)},
      batch_remote(Parent);
    fake_remote_stop ->
      ok
  end.

stop_workers(Workers) ->
  [Worker ! fake_remote_stop || Worker <- Workers],
  ok.

%%=================================================================
only_proxy_for_low_level_batch_test(Info) ->
  ConnectionPid = maps:get(connection_pid, Info),
  {links, Links} = process_info(ConnectionPid, links),
  Supervisor = whereis(ecall_connection_sup),
  Proxies = [Pid || Pid <- Links, Pid =/= Supervisor],
  [Proxy] = Proxies,
  Proxy.

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
