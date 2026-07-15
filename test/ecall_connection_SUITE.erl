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
  ecall_api_delegates_to_connection/1
]).

-record(connection, { node, master, pool, size }).

%%=================================================================
%% COMMON TEST API
%%=================================================================
all() ->
  [
    sender_pid_routes_all_entry_points,
    ecall_api_delegates_to_connection
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _Config) ->
  ok.

%%=================================================================
%% TEST CASES
%%=================================================================
sender_pid_routes_all_entry_points(_Config) ->
  run_route_test(ecall_connection).

ecall_api_delegates_to_connection(_Config) ->
  run_route_test(ecall).

run_route_test(ApiModule) ->
  Parent = self(),
  Node = node(),
  Size = 16,
  Pool = route_test_pool(Parent, Size),
  OldConnections = persistent_term:get(ecall_connection, undefined),
  Sender =
    spawn(fun() -> route_test_sender(Parent, Node, ApiModule) end),

  wait_for({route_test_ready, Sender}),
  TargetPid = route_test_target_pid(Size, Sender),

  try
    persistent_term:put(
      ecall_connection,
      #{Node => #connection{node = Node,
                            master = Parent,
                            pool = Pool,
                            size = Size}}),

    SenderIndex = erlang:phash2(Sender, Size),
    Sender ! {route_test_run, TargetPid},

    assert_request(SenderIndex, {send, TargetPid, route_test_tuple_message}),
    assert_request(SenderIndex, {send, TargetPid, route_test_pid_message}),
    assert_request(
      SenderIndex,
      {cast, route_test_module, route_test_function, [route_test_arg]}),
    assert_call_request(SenderIndex, Sender),

    wait_for({route_test_done, Sender,
              route_test_tuple_message,
              route_test_pid_message,
              ok,
              {ok, route_test_call_reply}})
  after
    restore_connections(OldConnections),
    stop_pool(Pool),
    TargetPid ! route_test_stop
  end.

%%=================================================================
%% UTILITIES
%%=================================================================
route_test_pool(Parent, Size) ->
  maps:from_list(
    [{I, spawn(fun() -> route_test_proxy(Parent, I) end)}
     || I <- lists:seq(0, Size - 1)]).

route_test_proxy(Parent, Index) ->
  receive
    {do, {call, Ref, ClientPID, _Module, _Function, _Args} = Request} ->
      Parent ! {route_test_request, self(), Index, Request},
      ClientPID ! {Ref, route_test_call_reply},
      route_test_proxy(Parent, Index);
    {do, Request} ->
      Parent ! {route_test_request, self(), Index, Request},
      route_test_proxy(Parent, Index);
    route_test_stop ->
      ok
  end.

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

route_test_target_pid(Size, Sender) ->
  SenderIndex = erlang:phash2(Sender, Size),
  route_test_target_pid(Size, SenderIndex, _Attempts = 128).

route_test_target_pid(Size, SenderIndex, Attempts) when Attempts > 0 ->
  Pid =
    spawn(fun() ->
      receive
        route_test_stop ->
          ok
      end
    end),
  case erlang:phash2(Pid, Size) =/= SenderIndex of
    true ->
      Pid;
    false ->
      Pid ! route_test_stop,
      route_test_target_pid(Size, SenderIndex, Attempts - 1)
  end.

assert_request(ExpectedIndex, ExpectedRequest) ->
  receive
    {route_test_request, _Proxy, ExpectedIndex, ExpectedRequest} ->
      ok;
    Other ->
      ct:fail({unexpected_request, ExpectedIndex, ExpectedRequest, Other})
  after
    1000 ->
      ct:fail({timeout, ExpectedIndex, ExpectedRequest})
  end.

assert_call_request(ExpectedIndex, Sender) ->
  receive
    {route_test_request, _Proxy, ExpectedIndex,
     {call, Ref, Sender, route_test_module, route_test_function,
      [route_test_arg]}} when is_reference(Ref) ->
      ok;
    Other ->
      ct:fail({unexpected_call_request, ExpectedIndex, Sender, Other})
  after
    1000 ->
      ct:fail({timeout, ExpectedIndex, call})
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

restore_connections(undefined) ->
  persistent_term:erase(ecall_connection);
restore_connections(Connections) ->
  persistent_term:put(ecall_connection, Connections).

stop_pool(Pool) ->
  [Pid ! route_test_stop || Pid <- maps:values(Pool)],
  ok.
