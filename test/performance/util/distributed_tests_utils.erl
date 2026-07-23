-module(distributed_tests_utils).

-include_lib("common_test/include/ct.hrl").

%% Config API
-export([
  performance_settings/1,
  role_config/1,
  validate_role_config/1,
  public_role_config/1
]).

%% Suite topology API
-export([
  start_topology/1,
  init_per_testcase/2,
  end_per_testcase/2,
  invalidate_suite/2,
  fail_point/2,
  stop_all/0
]).

%% Peer and readiness API
-export([
  connect_nodes/2,
  connect_ecall_bidirectional/3,
  probe_target/1,
  run_role_procedure/3,
  verify_beam_limits/2,
  verify_container_limits/1,
  verify_controller_connectivity/1,
  verify_dist_buf_busy_limit/2,
  verify_free_process_slots/2,
  verify_role_connectivity/2
]).

%% Ecall performance API
-export([
  prepare_ecall_batch/3,
  disconnect_ecall_bidirectional/2,
  effective_dist_buf_busy_limit_kib/1
]).

-define(STATE_KEY, {?MODULE, state}).
-define(IMAGE, "ecall-performance:otp27").
-define(BASE_IMAGE, "erlang:27.2.2").
-define(COMMAND_TIMEOUT, 30000).
-define(BUILD_TIMEOUT, 300000).
-define(REMOTE_TIMEOUT, 300000).
-define(RPC_TIMEOUT, 30000).
-define(OPEN_FILE_LIMIT, 1048576).
-define(PROCESS_LIMIT, 134217727).
-define(PORT_LIMIT, 1048576).
-define(ETS_LIMIT, 262144).
-define(DEFAULT_BATCH_SIZE, 1000).
-define(BUSY_LIMIT_MIN_KIB, 1).
-define(BUSY_LIMIT_MAX_KIB, 2097151).


%%====================================================================
%% Config API
%%====================================================================

performance_settings(Config) ->
  Overrides = performance_overrides(Config),
  Performance =
    case Overrides of
      Map when is_map(Map) ->
        maps:merge(default_performance_settings(), Map);
      Other ->
        Other
    end,
  case validate_performance_settings(Performance) of
    {ok, Validated} ->
      Validated;
    {error, Reason} ->
      ct:fail({invalid_performance_settings, Reason})
  end.

role_config(Config) ->
  validate_role_config(role_config_from_ct(Config)).

validate_role_config(Config) ->
  normalize_role_config(Config).

public_role_config(RoleConfig) ->
  maps:map(
    fun(_Role, Location) ->
      public_role_location(Location)
    end,
    RoleConfig).


%%====================================================================
%% Suite topology API
%%====================================================================

start_topology(Config0) ->
  Suite = suite_name(Config0),
  Generation = begin_suite(Suite),
  try
    start_topology(Config0, Suite, Generation)
  catch
    Class:Reason:Stack ->
      record_invalid(Suite, Generation, {topology_init_failed, Class, Reason}),
      stop_all(),
      erlang:raise(Class, Reason, Stack)
  end.

start_topology(Config0, Suite, Generation) ->
  RoleConfig = role_config(Config0),
  Performance = performance_settings(Config0),
  ct:pal("Resolved performance config: ~p", [Performance]),
  ct:pal("Resolved role config: ~p", [public_role_config(RoleConfig)]),
  SenderNode = start_node(sender, RoleConfig, Performance),
  ReceiverNode = start_node(receiver, RoleConfig, Performance),
  ok = verify_controller_connectivity([SenderNode, ReceiverNode]),
  ok = verify_role_connectivity(SenderNode, ReceiverNode),
  ok = start_ecall([SenderNode, ReceiverNode]),
  ok = connect_ecall_bidirectional(
         SenderNode,
         ReceiverNode,
         ?DEFAULT_BATCH_SIZE),
  ok = verify_free_process_slots(
         SenderNode,
         largest_writer_count(Performance)),
  [
    {topology_suite, Suite},
    {topology_generation, Generation},
    {role_config, public_role_config(RoleConfig)},
    {performance, Performance},
    {sender, SenderNode},
    {receiver, ReceiverNode}
    | Config0
  ].

init_per_testcase(_Testcase, Config) ->
  case invalid_reason(Config) of
    undefined ->
      Config;
    Reason ->
      {skip, Reason}
  end.

end_per_testcase(_Testcase, Config) ->
  case testcase_status(Config) of
    undefined ->
      ok;
    ok ->
      ok;
    Status ->
      invalidate_suite(Config, {testcase_finished, Status})
  end,
  Config.

invalidate_suite(Config, Reason) when is_list(Config) ->
  Suite = ?config(topology_suite, Config),
  Generation = ?config(topology_generation, Config),
  record_invalid(Suite, Generation, Reason),
  stop_all(),
  ok;
invalidate_suite(Suite, Reason) when is_atom(Suite) ->
  Generation = current_generation(Suite),
  record_invalid(Suite, Generation, Reason),
  stop_all(),
  ok.

fail_point(Config, Reason) ->
  ct:pal("Performance point failed: ~p", [Reason]),
  invalidate_suite(Config, Reason),
  ct:fail(Reason).

stop_all() ->
  State = state(),
  Started = maps:values(maps:get(roles, State, #{})),
  [stop_peer(maps:get(peer, RoleState, undefined)) || RoleState <- Started],
  [remove_role_container(RoleState) || RoleState <- Started],
  remove_password_files(),
  put_state(State#{roles => #{}, password_files => #{}}),
  ok.


%%====================================================================
%% Peer and readiness API
%%====================================================================

connect_nodes(NodeA, NodeB) ->
  true = rpc:call(NodeA, net_kernel, connect_node, [NodeB], ?RPC_TIMEOUT),
  wait_until(
    fun() ->
      lists:member(
        NodeB,
        rpc:call(NodeA, erlang, nodes, [connected], ?RPC_TIMEOUT))
    end,
    ?RPC_TIMEOUT).

connect_ecall_bidirectional(SenderNode, ReceiverNode, BatchSize) ->
  _Infos = setup_ecall_bidirectional(SenderNode, ReceiverNode, BatchSize),
  ok.

prepare_ecall_batch(SenderNode, ReceiverNode, BatchSize) ->
  {SenderInfo, ReceiverInfo} =
    setup_ecall_bidirectional(SenderNode, ReceiverNode, BatchSize),
  #{
    configured_batch_size => BatchSize,
    effective_batch_size => BatchSize,
    sender_connection_pid => maps:get(connection_pid, SenderInfo),
    receiver_connection_pid => maps:get(connection_pid, ReceiverInfo),
    sender_proxy_count => maps:get(proxy_count, SenderInfo),
    receiver_proxy_count => maps:get(proxy_count, ReceiverInfo)
  }.

setup_ecall_bidirectional(SenderNode, ReceiverNode, BatchSize) ->
  ok = disconnect_ecall_bidirectional(SenderNode, ReceiverNode),
  ok = configure_ecall_batch_size(SenderNode, BatchSize),
  ok = configure_ecall_batch_size(ReceiverNode, BatchSize),
  SenderInfo = connect_ecall_info(SenderNode, ReceiverNode, BatchSize),
  ReceiverInfo = connect_ecall_info(ReceiverNode, SenderNode, BatchSize),
  SenderInfoAfter =
    verify_ecall_probe(SenderNode, ReceiverNode, BatchSize, SenderInfo),
  ReceiverInfoAfter =
    verify_ecall_probe(ReceiverNode, SenderNode, BatchSize, ReceiverInfo),
  {SenderInfoAfter, ReceiverInfoAfter}.

disconnect_ecall_bidirectional(SenderNode, ReceiverNode) ->
  ok = disconnect_ecall(SenderNode, ReceiverNode),
  ok = disconnect_ecall(ReceiverNode, SenderNode),
  ok.

disconnect_ecall(FromNode, ToNode) ->
  case rpc:call(
         FromNode,
         ecall_connection,
         disconnect,
         [ToNode],
         ?RPC_TIMEOUT) of
    ok ->
      ok;
    Other ->
      ct:fail({ecall_disconnect_failed, FromNode, ToNode, Other})
  end.

effective_dist_buf_busy_limit_kib(Node) ->
  rpc:call(Node, erlang, system_info, [dist_buf_busy_limit], ?RPC_TIMEOUT) div 1024.

probe_target(Ref) ->
  {ecall_probe, Ref, node()}.

run_role_procedure(Node, {Module, Function, Args}, Timeout) ->
  Controller = self(),
  Ref = make_ref(),
  {Pid, MonRef} =
    spawn_monitor(
      Node,
      fun() ->
        Result = apply(Module, Function, Args),
        Controller ! {Ref, self(), Result}
      end),
  receive
    {Ref, Pid, Result} ->
      erlang:demonitor(MonRef, [flush]),
      {ok, Result};
    {'DOWN', MonRef, process, Pid, Reason} ->
      {error, Reason}
  after Timeout ->
      erlang:demonitor(MonRef, [flush]),
      {error, timeout}
  end.

verify_beam_limits(Node, Performance) ->
  BusyKiB = maps:get(distribution_busy_limit_kib, Performance),
  ok = verify_dist_buf_busy_limit(Node, BusyKiB),
  ok = verify_system_limit(Node, process_limit, ?PROCESS_LIMIT),
  ok = verify_system_limit(Node, port_limit, ?PORT_LIMIT),
  ok = verify_system_limit(Node, ets_limit, ?ETS_LIMIT).

verify_container_limits(RoleState) ->
  ok = verify_open_file_limit(maps:get(node, RoleState)),
  ok = verify_docker_no_caps(RoleState).

verify_controller_connectivity(Nodes) ->
  [pong = net_adm:ping(Node) || Node <- Nodes],
  ok.

verify_dist_buf_busy_limit(Node, BusyKiB) ->
  Expected = BusyKiB * 1024,
  case rpc:call(Node, erlang, system_info, [dist_buf_busy_limit], ?RPC_TIMEOUT) of
    Expected ->
      ok;
    Actual ->
      ct:fail({invalid_dist_buf_busy_limit, Node, Expected, Actual})
  end.

verify_free_process_slots(Node, RequiredFree) ->
  ProcessLimit = rpc:call(Node, erlang, system_info, [process_limit], ?RPC_TIMEOUT),
  ProcessCount = rpc:call(Node, erlang, system_info, [process_count], ?RPC_TIMEOUT),
  Free = ProcessLimit - ProcessCount,
  case Free > RequiredFree of
    true ->
      ok;
    false ->
      ct:fail({insufficient_process_slots, Node, RequiredFree, Free})
  end.

verify_role_connectivity(SenderNode, ReceiverNode) ->
  ok = connect_nodes(SenderNode, ReceiverNode),
  ok = connect_nodes(ReceiverNode, SenderNode).


%%====================================================================
%% Startup
%%====================================================================

start_node(Role, RoleConfig, Performance) ->
  Started = maps:get(roles, state(), #{}),
  case maps:get(Role, Started, undefined) of
    #{node := Node} ->
      Node;
    undefined ->
      start_new_node(Role, maps:get(Role, RoleConfig), RoleConfig, Performance)
  end.

start_new_node(Role, Location, RoleConfig, Performance) ->
  Cookie = ensure_controller_cookie(RoleConfig),
  RoleState = start_role(Role, Location, Cookie, Performance),
  ok = verify_peer(maps:get(peer, RoleState), maps:get(node, RoleState)),
  ok = verify_beam_limits(maps:get(node, RoleState), Performance),
  ok = verify_container_limits(RoleState),
  maps:get(node, RoleState).

start_role(Role, local, Cookie, Performance) ->
  Docker = docker_executable(),
  ProjectDir = project_dir(),
  ensure_local_image(Docker, ProjectDir),
  Container = unique_name(atom_to_list(Role)),
  Port = role_dist_port(Role),
  Node = role_node(Role, local_node_host()),
  Exec = {Docker, docker_run_args(Container)},
  PartialState = #{
    role => Role,
    location => local,
    peer => undefined,
    node => Node,
    container => Container
  },
  put_started_role(Role, PartialState),
  {ok, Peer, Node} =
    peer:start(peer_options(Node, Cookie, Port, Exec, Performance)),
  RoleState = PartialState#{peer => Peer},
  put_started_role(Role, RoleState),
  RoleState;
start_role(Role, #{host := _Host} = HostConfig0, Cookie, Performance) ->
  HostConfig = prepare_remote_host(HostConfig0),
  Container = unique_name(atom_to_list(Role)),
  Port = role_dist_port(Role),
  Node = role_node(Role, maps:get(node_host, HostConfig)),
  Exec = remote_docker_run_exec(HostConfig, Container),
  PartialState = #{
    role => Role,
    location => remote,
    host_config => HostConfig,
    peer => undefined,
    node => Node,
    container => Container
  },
  put_started_role(Role, PartialState),
  {ok, Peer, Node} =
    peer:start(peer_options(Node, Cookie, Port, Exec, Performance)),
  RoleState = PartialState#{peer => Peer},
  put_started_role(Role, RoleState),
  RoleState.

peer_options(Node, Cookie, DistPort, Exec, Performance) ->
  #{
    name => Node,
    longnames => true,
    connection => standard_io,
    shutdown => 1000,
    exec => Exec,
    args => erl_args(Cookie, DistPort, Performance)
  }.

erl_args(Cookie, DistPort, Performance) ->
  BusyKiB = integer_to_list(maps:get(distribution_busy_limit_kib, Performance)),
  ["-pa" | container_code_paths()] ++
    [
      "-setcookie", Cookie,
      "+zdbbl", BusyKiB,
      "+P", integer_to_list(?PROCESS_LIMIT),
      "+Q", integer_to_list(?PORT_LIMIT),
      "+e", integer_to_list(?ETS_LIMIT),
      "-kernel",
      "inet_dist_listen_min", integer_to_list(DistPort),
      "inet_dist_listen_max", integer_to_list(DistPort)
    ].

container_code_paths() ->
  [
    "/opt/ecall/_build/default/lib/ecall/ebin",
    "/opt/ecall/_build/test/lib/ecall/test/performance",
    "/opt/ecall/_build/test/lib/ecall/test/performance/util",
    "/opt/ecall/test/performance",
    "/opt/ecall/test/performance/util"
  ].

docker_run_args(Container) ->
  [
    "run", "--rm",
    "--name", Container,
    "--network", "host",
    "--ulimit", "nofile=1048576:1048576",
    "-i",
    ?IMAGE
  ].

remote_docker_run_exec(HostConfig, Container) ->
  remote_exec(HostConfig, ["docker" | docker_run_args(Container)]).

verify_peer(Peer, Node) ->
  Node = peer:call(Peer, erlang, node, [], ?RPC_TIMEOUT),
  ok.

start_ecall(Nodes) ->
  [
    begin
      {ok, _Started} =
        rpc:call(Node, application, ensure_all_started, [ecall], ?RPC_TIMEOUT),
      ok
    end
    || Node <- Nodes
  ],
  ok.

connect_ecall_info(FromNode, ToNode, BatchSize) ->
  assert_exported(FromNode, ecall_connection, connect, 1),
  assert_exported(FromNode, ecall_connection, connection_info, 1),
  ensure_ecall_connection(FromNode, ToNode, BatchSize, 5).

verify_ecall_probe(FromNode, ToNode, BatchSize, Info) ->
  ok = run_ecall_probe(FromNode, ToNode),
  InfoAfter = verify_ecall_connection(FromNode, ToNode, BatchSize),
  case maps:get(connection_pid, InfoAfter) =:= maps:get(connection_pid, Info) of
    true ->
      InfoAfter;
    false ->
      ct:fail({ecall_connection_replaced_during_probe, FromNode, ToNode})
  end.

ensure_ecall_connection(FromNode, ToNode, BatchSize, Attempts)
    when Attempts > 0 ->
  case connect_ecall_request(FromNode, ToNode) of
    ok ->
      verify_or_recover_ecall_connection(
        FromNode,
        ToNode,
        BatchSize,
        Attempts);
    already_started ->
      verify_or_recover_ecall_connection(
        FromNode,
        ToNode,
        BatchSize,
        Attempts);
    {error, Other} ->
      ct:fail({ecall_connect_failed, FromNode, ToNode, Other})
  end;
ensure_ecall_connection(FromNode, ToNode, BatchSize, 0) ->
  verify_ecall_connection(FromNode, ToNode, BatchSize).

verify_or_recover_ecall_connection(FromNode, ToNode, BatchSize, Attempts) ->
  case ecall_connection_result(FromNode, ToNode, BatchSize) of
    {ok, Info} ->
      Info;
    {error, _VerifyError} ->
      recover_ecall_connect_race(FromNode, ToNode, BatchSize, Attempts)
  end.

recover_ecall_connect_race(FromNode, ToNode, BatchSize, Attempts) ->
  ok = disconnect_ecall(FromNode, ToNode),
  ok = configure_ecall_batch_size(FromNode, BatchSize),
  timer:sleep(20),
  ensure_ecall_connection(FromNode, ToNode, BatchSize, Attempts - 1).

connect_ecall_request(FromNode, ToNode) ->
  case rpc:call(
         FromNode,
         ecall_connection,
         connect,
         [ToNode],
         ?RPC_TIMEOUT) of
    ok ->
      ok;
    {already_started, Pid} when is_pid(Pid) ->
      already_started;
    {badrpc, {'EXIT', {{nocatch, {already_started, Pid}}, _Stack}}}
        when is_pid(Pid) ->
      already_started;
    Other ->
      {error, Other}
  end.

configure_ecall_batch_size(Node, BatchSize) ->
  case rpc:call(
         Node,
         application,
         set_env,
         [ecall, batch_size, BatchSize],
         ?RPC_TIMEOUT) of
    ok ->
      ok;
    Other ->
      ct:fail({ecall_batch_size_config_failed, Node, BatchSize, Other})
  end.

verify_ecall_connection(FromNode, ToNode, BatchSize) ->
  case ecall_connection_result(FromNode, ToNode, BatchSize) of
    {ok, Info} ->
      Info;
    {error, Other} ->
      ct:fail({invalid_ecall_connection_info, FromNode, ToNode, Other})
  end.

ecall_connection_result(FromNode, ToNode, BatchSize) ->
  case ecall_connection_info(FromNode, ToNode) of
    {ok, #{status := connected,
           connection_pid := Pid,
           proxy_count := ProxyCount,
           batch_size := BatchSize} = Info}
        when is_pid(Pid), is_integer(ProxyCount), ProxyCount > 0 ->
      case rpc:call(FromNode, erlang, is_process_alive, [Pid], ?RPC_TIMEOUT) of
        true ->
          {ok, Info};
        Other ->
          {error, {connection_pid_not_alive, Pid, Other}}
      end;
    Other ->
      {error, Other}
  end.

ecall_connection_info(FromNode, ToNode) ->
  rpc:call(
    FromNode,
    ecall_connection,
    connection_info,
    [ToNode],
    ?RPC_TIMEOUT).

run_ecall_probe(FromNode, ToNode) ->
  Ref = make_ref(),
  Expected = {ecall_probe, Ref, ToNode},
  case rpc:call(
         FromNode,
         ecall,
         call,
         [ToNode, ?MODULE, probe_target, [Ref]],
         ?RPC_TIMEOUT) of
    {ok, Expected} ->
      ok;
    Other ->
      ct:fail({ecall_probe_failed, FromNode, ToNode, Expected, Other})
  end.

assert_exported(Node, Module, Function, Arity) ->
  ok = ensure_module_loaded(Node, Module),
  case rpc:call(
         Node,
         erlang,
         function_exported,
         [Module, Function, Arity],
         ?RPC_TIMEOUT) of
    true ->
      ok;
    false ->
      ct:fail({missing_required_api, Node, Module, Function, Arity});
    Other ->
      ct:fail({cannot_check_required_api, Node, Module, Function, Arity, Other})
  end.

ensure_module_loaded(Node, Module) ->
  case rpc:call(Node, code, ensure_loaded, [Module], ?RPC_TIMEOUT) of
    {module, Module} ->
      ok;
    {error, Reason} ->
      ct:fail({cannot_load_required_api_module, Node, Module, Reason});
    Other ->
      ct:fail({cannot_load_required_api_module, Node, Module, Other})
  end.


%%====================================================================
%% Suite state
%%====================================================================

begin_suite(Suite) ->
  State = state(),
  Generation = maps:get(next_generation, State, 0) + 1,
  Invalid = maps:get(invalid, State, #{}),
  put_state(
    State#{
      next_generation => Generation,
      suite => Suite,
      generation => Generation,
      invalid => maps:remove(Suite, Invalid)
    }),
  Generation.

invalid_reason(Config) ->
  Suite = ?config(topology_suite, Config),
  Generation = ?config(topology_generation, Config),
  Invalid = maps:get(invalid, state(), #{}),
  case maps:get(Suite, Invalid, undefined) of
    #{generation := Generation, reason := Reason} ->
      {topology_invalid, Reason};
    _ ->
      undefined
  end.

record_invalid(Suite, Generation, Reason) ->
  State = state(),
  Invalid = maps:get(invalid, State, #{}),
  put_state(
    State#{
      invalid => Invalid#{
        Suite => #{generation => Generation, reason => Reason}
      }
    }).

current_generation(Suite) ->
  State = state(),
  case maps:get(suite, State, undefined) of
    Suite ->
      maps:get(generation, State, undefined);
    _ ->
      undefined
  end.

suite_name(Config) ->
  case lists:keyfind(suite, 1, Config) of
    {suite, Suite} ->
      Suite;
    false ->
      undefined
  end.

testcase_status(Config) ->
  case lists:keyfind(tc_status, 1, Config) of
    {tc_status, Status} ->
      Status;
    false ->
      undefined
  end.

state() ->
  persistent_term:get(?STATE_KEY, #{roles => #{}, invalid => #{}}).

put_state(State) ->
  persistent_term:put(?STATE_KEY, State).

put_started_role(Role, RoleState) ->
  State = state(),
  Roles = maps:get(roles, State, #{}),
  put_state(State#{roles => Roles#{Role => RoleState}}).


%%====================================================================
%% Config helpers
%%====================================================================

default_performance_settings() ->
  #{
    pace_ms => 100,
    messages_per_writer => 1000,
    writer_counts => [1000, 10000, 100000, 500000, 1000000],
    ecall_batch_sizes => [10, 100, 1000, 10000],
    distribution_busy_limit_kib => 1024
  }.

performance_overrides(Config) ->
  case lists:keyfind(performance, 1, Config) of
    {performance, Performance} ->
      Performance;
    false ->
      ct:get_config(performance, #{})
  end.

validate_performance_settings(Performance) when is_map(Performance) ->
  Validators = [
    fun validate_pace_ms/1,
    fun validate_messages_per_writer/1,
    fun validate_writer_counts/1,
    fun validate_ecall_batch_sizes/1,
    fun validate_distribution_busy_limit/1
  ],
  validate_all(Validators, Performance);
validate_performance_settings(Performance) ->
  {error, {not_a_map, Performance}}.

validate_all([], Performance) ->
  {ok, Performance};
validate_all([Validator | Rest], Performance) ->
  case Validator(Performance) of
    ok ->
      validate_all(Rest, Performance);
    {error, Reason} ->
      {error, Reason}
  end.

validate_pace_ms(Performance) ->
  validate_positive_integer(pace_ms, maps:get(pace_ms, Performance, undefined)).

validate_messages_per_writer(Performance) ->
  validate_positive_integer(
    messages_per_writer,
    maps:get(messages_per_writer, Performance, undefined)).

validate_writer_counts(Performance) ->
  validate_distinct_positive_list(
    writer_counts,
    maps:get(writer_counts, Performance, undefined)).

validate_ecall_batch_sizes(Performance) ->
  validate_distinct_positive_list(
    ecall_batch_sizes,
    maps:get(ecall_batch_sizes, Performance, undefined)).

validate_distribution_busy_limit(Performance) ->
  Key = distribution_busy_limit_kib,
  Value = maps:get(Key, Performance, undefined),
  case validate_positive_integer(Key, Value) of
    ok when Value >= ?BUSY_LIMIT_MIN_KIB, Value =< ?BUSY_LIMIT_MAX_KIB ->
      ok;
    ok ->
      {error, {out_of_range, Key, Value,
               {?BUSY_LIMIT_MIN_KIB, ?BUSY_LIMIT_MAX_KIB}}};
    Error ->
      Error
  end.

validate_positive_integer(_Key, Value) when is_integer(Value), Value > 0 ->
  ok;
validate_positive_integer(Key, Value) ->
  {error, {not_positive_integer, Key, Value}}.

validate_distinct_positive_list(Key, Values)
    when is_list(Values), Values =/= [] ->
  case lists:dropwhile(fun is_positive_integer/1, Values) of
    [] ->
      validate_distinct(Key, Values);
    [Invalid | _] ->
      {error, {not_positive_integer, Key, Invalid}}
  end;
validate_distinct_positive_list(Key, Values) ->
  {error, {not_non_empty_list, Key, Values}}.

validate_distinct(Key, Values) ->
  case ordsets:size(ordsets:from_list(Values)) =:= length(Values) of
    true ->
      ok;
    false ->
      {error, {not_distinct, Key, Values}}
  end.

is_positive_integer(Value) ->
  is_integer(Value) andalso Value > 0.

role_config_from_ct(_Config) ->
  case ct:get_config(role_config, undefined) of
    undefined ->
      #{sender => local, receiver => local};
    RoleConfig ->
      RoleConfig
  end.

normalize_role_config(Config) when is_map(Config) ->
  #{
    sender => normalize_role_location(sender, maps:get(sender, Config, undefined)),
    receiver => normalize_role_location(receiver, maps:get(receiver, Config, undefined))
  };
normalize_role_config(Config) ->
  ct:fail({invalid_role_config, Config}).

normalize_role_location(_Role, local) ->
  local;
normalize_role_location(Role, #{host := Host, user := User} = Location) ->
  Password = maps:get(password, Location, undefined),
  NodeHost = maps:get(node_host, Location, Host),
  #{
    host => string_value(Role, host, Host),
    user => string_value(Role, user, User),
    password => optional_string_value(Role, password, Password),
    port => string_value(Role, port, maps:get(port, Location, "22")),
    node_host => string_value(Role, node_host, NodeHost)
  };
normalize_role_location(Role, Location) ->
  ct:fail({invalid_role_location, Role, Location}).

public_role_location(local) ->
  local;
public_role_location(Location) when is_map(Location) ->
  maps:remove(password, Location).

string_value(_Role, _Key, Value) when is_list(Value) ->
  Value;
string_value(Role, Key, Value) ->
  ct:fail({invalid_role_location_value, Role, Key, Value}).

optional_string_value(_Role, _Key, undefined) ->
  undefined;
optional_string_value(_Role, _Key, Value) when is_list(Value) ->
  Value;
optional_string_value(Role, Key, Value) ->
  ct:fail({invalid_role_location_value, Role, Key, Value}).

largest_writer_count(Performance) ->
  lists:max(maps:get(writer_counts, Performance)).


%%====================================================================
%% Docker and remote commands
%%====================================================================

ensure_local_image(Docker, ProjectDir) ->
  Dockerfile = filename:join([ProjectDir, "test", "performance", "Dockerfile"]),
  ct:pal("Rebuilding performance image ~s from current source", [?IMAGE]),
  command_ok(
    "docker build",
    Docker,
    [
      "build",
      "--file", Dockerfile,
      "--build-arg", "BASE_IMAGE=" ++ ?BASE_IMAGE,
      "--tag", ?IMAGE,
      ProjectDir
    ],
    ?BUILD_TIMEOUT,
    []).

prepare_remote_host(HostConfig) ->
  _ = remote_ok(
        HostConfig,
        ["docker", "version", "--format", "{{.Server.Version}}"],
        ?COMMAND_TIMEOUT),
  remote_build_image(HostConfig),
  HostConfig.

remote_build_image(HostConfig) ->
  ProjectDir = project_dir(),
  Dockerfile = filename:join([ProjectDir, "test", "performance", "Dockerfile"]),
  RemoteBuild =
    remote_command_string(
      HostConfig,
      [
        "docker", "build",
        "--build-arg", "BASE_IMAGE=" ++ ?BASE_IMAGE,
        "--tag", ?IMAGE,
        "-"
      ]),
  Command =
    "tmp=$(mktemp -d) && "
    "trap 'rm -rf \"$tmp\"' EXIT && "
    "cp " ++ shell_quote(Dockerfile) ++ " \"$tmp/Dockerfile\" && "
    "tar -C " ++ shell_quote(ProjectDir) ++ " -cf - . | "
    "tar -C \"$tmp\" -xf - && "
    "tar -C \"$tmp\" -cf - . | " ++ RemoteBuild,
  command_ok("remote docker build", "sh", ["-c", Command], ?REMOTE_TIMEOUT, []),
  ok.

verify_docker_no_caps(#{location := local, container := Container}) ->
  verify_docker_no_caps_output(
    run(
      docker_executable(),
      docker_inspect_caps_args(Container),
      ?COMMAND_TIMEOUT));
verify_docker_no_caps(#{location := remote,
                        host_config := HostConfig,
                        container := Container}) ->
  verify_docker_no_caps_output(
    remote_run(HostConfig, docker_inspect_caps_args(Container), ?COMMAND_TIMEOUT)).

docker_inspect_caps_args(Container) ->
  [
    "inspect",
    "--format",
    "{{.HostConfig.Memory}} {{.HostConfig.PidsLimit}}",
    Container
  ].

verify_docker_no_caps_output({0, Output}) ->
  Words = string:tokens(string:trim(binary_to_list(Output)), " \t\r\n"),
  case Words of
    [Memory, PidsLimit] ->
      verify_docker_caps(Memory, PidsLimit);
    _ ->
      ct:fail({cannot_parse_docker_caps, trim_output(Output)})
  end;
verify_docker_no_caps_output({Status, Output}) when is_integer(Status) ->
  ct:fail({docker_inspect_failed, Status, trim_output(Output)});
verify_docker_no_caps_output({error, Reason}) ->
  ct:fail({docker_inspect_failed, Reason}).

verify_docker_caps("0", PidsLimit)
    when PidsLimit =:= "0";
         PidsLimit =:= "-1";
         PidsLimit =:= "<nil>" ->
  ok;
verify_docker_caps(Memory, PidsLimit) ->
  ct:fail({docker_caps_configured, #{memory => Memory, pids_limit => PidsLimit}}).

remote_ok(HostConfig, RemoteArgs, Timeout) ->
  {Exec, Args, Env} = remote_exec_parts(HostConfig, RemoteArgs),
  command_ok("remote command", Exec, Args, Timeout, Env).

remote_run(HostConfig, RemoteArgs, Timeout) ->
  {Exec, Args, Env} = remote_exec_parts(HostConfig, RemoteArgs),
  run(Exec, Args, Timeout, Env).

remote_exec(HostConfig, RemoteArgs) ->
  {Exec, Args, _Env} = remote_exec_parts(HostConfig, RemoteArgs),
  {Exec, Args}.

remote_exec_parts(HostConfig, RemoteArgs) ->
  SshArgs = ssh_args(HostConfig) ++ RemoteArgs,
  case maps:get(password, HostConfig, undefined) of
    undefined ->
      {ssh_executable(), SshArgs, []};
    _Password ->
      {sshpass_executable(), ["-f", password_file(HostConfig), ssh_executable()
                              | SshArgs], []}
  end.

remote_command_string(HostConfig, RemoteArgs) ->
  {Exec, Args, _Env} = remote_exec_parts(HostConfig, RemoteArgs),
  string:join([shell_quote(Exec) | [shell_quote(Arg) || Arg <- Args]], " ").

ssh_args(HostConfig) ->
  User = maps:get(user, HostConfig),
  Host = maps:get(host, HostConfig),
  Port = maps:get(port, HostConfig),
  AuthArgs =
    case maps:get(password, HostConfig, undefined) of
      undefined ->
        ["-o", "BatchMode=yes"];
      _Password ->
        []
    end,
  [
    "-p", Port,
    "-o", "StrictHostKeyChecking=accept-new",
    "-o", "ConnectTimeout=10"
  ] ++ AuthArgs ++ [User ++ "@" ++ Host].

remove_role_container(#{location := local, container := Container}) ->
  _ = run(docker_executable(), ["rm", "-f", Container], ?COMMAND_TIMEOUT),
  ok;
remove_role_container(#{location := remote,
                        host_config := HostConfig,
                        container := Container}) ->
  _ = remote_run(HostConfig, ["docker", "rm", "-f", Container], ?COMMAND_TIMEOUT),
  ok;
remove_role_container(_) ->
  ok.

stop_peer(undefined) ->
  ok;
stop_peer(Peer) ->
  catch peer:stop(Peer),
  ok.


%%====================================================================
%% Controller distribution
%%====================================================================

ensure_controller_cookie(RoleConfig) ->
  State = state(),
  case maps:get(cookie, State, undefined) of
    undefined ->
      Cookie = unique_cookie(),
      ensure_controller_node(Cookie, RoleConfig),
      put_state(State#{cookie => Cookie}),
      Cookie;
    Cookie ->
      ensure_controller_node(Cookie, RoleConfig),
      Cookie
  end.

ensure_controller_node(Cookie, RoleConfig) ->
  Name = controller_node_name(RoleConfig),
  case node() of
    nonode@nohost ->
      set_controller_dist_port(),
      case net_kernel:start([Name, longnames]) of
        {ok, _Pid} ->
          set_cookie(Cookie);
        {error, {already_started, _Pid}} ->
          set_cookie(Cookie);
        {error, Reason} ->
          ct:fail({cannot_start_controller_node, Name, Reason})
      end;
    _Node ->
      set_cookie(Cookie)
  end.

set_controller_dist_port() ->
  Port = dist_port_base(),
  ok = application:set_env(kernel, inet_dist_listen_min, Port),
  ok = application:set_env(kernel, inet_dist_listen_max, Port).

set_cookie(Cookie) ->
  true = erlang:set_cookie(node(), list_to_atom(Cookie)),
  ok.

controller_node_name(RoleConfig) ->
  State = state(),
  case maps:get(controller_node, State, undefined) of
    undefined ->
      Host = controller_node_host(RoleConfig),
      Name = list_to_atom("performance_controller_" ++ os:getpid() ++ "@" ++ Host),
      put_state(State#{controller_node => Name}),
      Name;
    Name ->
      Name
  end.

controller_node_host(RoleConfig) ->
  RemoteRoles =
    [Location || Location <- maps:values(RoleConfig),
                 is_map(Location),
                 maps:is_key(host, Location)],
  case RemoteRoles of
    [] ->
      local_node_host();
    [HostConfig | _] ->
      detect_controller_host(HostConfig)
  end.

detect_controller_host(HostConfig) ->
  case remote_run(HostConfig, ["printenv", "SSH_CLIENT"], ?COMMAND_TIMEOUT) of
    {0, Output} ->
      case string:tokens(string:trim(binary_to_list(Output)), " \t") of
        [Host | _] ->
          Host;
        [] ->
          ct:fail({cannot_detect_controller_host, empty_ssh_client})
      end;
    {Status, Output} when is_integer(Status) ->
      ct:fail({cannot_detect_controller_host, Status, trim_output(Output)});
    {error, Reason} ->
      ct:fail({cannot_detect_controller_host, Reason})
  end.

role_node(Role, Host) ->
  list_to_atom(atom_to_list(Role) ++ "@" ++ Host).

role_dist_port(Role) ->
  dist_port_base() + role_port_offset(Role).

role_port_offset(sender) ->
  1;
role_port_offset(receiver) ->
  2;
role_port_offset(Role) ->
  10 + erlang:phash2(Role, 1000).

dist_port_base() ->
  State = state(),
  case maps:get(dist_port_base, State, undefined) of
    undefined ->
      Base = integer_env("PERFORMANCE_DIST_PORT_BASE", 4443),
      put_state(State#{dist_port_base => Base}),
      Base;
    Base ->
      Base
  end.


%%====================================================================
%% Runtime checks
%%====================================================================

verify_system_limit(Node, Limit, Minimum) ->
  case rpc:call(Node, erlang, system_info, [Limit], ?RPC_TIMEOUT) of
    Actual when is_integer(Actual), Actual >= Minimum ->
      ok;
    Actual ->
      ct:fail({insufficient_beam_limit, Node, Limit, Minimum, Actual})
  end.

verify_open_file_limit(Node) ->
  case rpc:call(Node, os, cmd, ["ulimit -n"], ?RPC_TIMEOUT) of
    Output when is_list(Output) ->
      verify_open_file_limit_value(Node, string:trim(Output));
    Other ->
      ct:fail({cannot_read_open_file_limit, Node, Other})
  end.

verify_open_file_limit_value(_Node, "unlimited") ->
  ok;
verify_open_file_limit_value(Node, Value) ->
  try list_to_integer(Value) of
    Limit when Limit >= ?OPEN_FILE_LIMIT ->
      ok;
    Limit ->
      ct:fail({insufficient_open_file_limit, Node, ?OPEN_FILE_LIMIT, Limit})
  catch
    error:badarg ->
      ct:fail({cannot_parse_open_file_limit, Node, Value})
  end.


%%====================================================================
%% Command helpers
%%====================================================================

password_file(HostConfig) ->
  Key = {maps:get(host, HostConfig),
         maps:get(port, HostConfig),
         maps:get(user, HostConfig)},
  State = state(),
  PasswordFiles = maps:get(password_files, State, #{}),
  case maps:get(Key, PasswordFiles, undefined) of
    undefined ->
      Path = filename:join("/tmp", unique_name("sshpass")),
      ok = file:write_file(Path, maps:get(password, HostConfig)),
      ok = file:change_mode(Path, 8#600),
      put_state(State#{password_files => PasswordFiles#{Key => Path}}),
      Path;
    Path ->
      Path
  end.

remove_password_files() ->
  State = state(),
  PasswordFiles = maps:get(password_files, State, #{}),
  [file:delete(Path) || Path <- maps:values(PasswordFiles)],
  ok.

docker_executable() ->
  executable("docker").

ssh_executable() ->
  executable("ssh").

sshpass_executable() ->
  executable("sshpass").

executable(Name) ->
  case os:find_executable(Name) of
    false ->
      ct:fail({executable_not_found, Name});
    Path ->
      Path
  end.

command_ok(Label, Exec, Args, Timeout, Env) ->
  case run(Exec, Args, Timeout, Env) of
    {0, Output} ->
      Output;
    {Status, Output} when is_integer(Status) ->
      ct:fail({command_failed, Label, Status, trim_output(Output)});
    {error, Reason} ->
      ct:fail({command_failed, Label, Reason})
  end.

run(Exec0, Args, Timeout) ->
  run(Exec0, Args, Timeout, []).

run(Exec0, Args, Timeout, Env) ->
  case resolve_executable(Exec0) of
    false ->
      {error, {executable_not_found, Exec0}};
    Exec ->
      Port =
        open_port(
          {spawn_executable, Exec},
          [{args, Args}, {env, Env}, binary, exit_status, stderr_to_stdout]),
      collect_port(Port, Timeout, [])
  end.

resolve_executable(Exec) ->
  case filename:pathtype(Exec) of
    absolute ->
      Exec;
    _Relative ->
      os:find_executable(Exec)
  end.

collect_port(Port, Timeout, Acc) ->
  receive
    {Port, {data, Data}} ->
      collect_port(Port, Timeout, [Data | Acc]);
    {Port, {exit_status, Status}} ->
      {Status, iolist_to_binary(lists:reverse(Acc))}
  after Timeout ->
      catch port_close(Port),
      {error, timeout}
  end.

wait_until(Predicate, Timeout) ->
  Deadline = erlang:monotonic_time(millisecond) + Timeout,
  wait_until(Predicate, Deadline, undefined).

wait_until(Predicate, Deadline, LastResult) ->
  case catch Predicate() of
    true ->
      ok;
    Other ->
      Now = erlang:monotonic_time(millisecond),
      case Now >= Deadline of
        true ->
          ct:fail({timeout, LastResult});
        false ->
          timer:sleep(100),
          wait_until(Predicate, Deadline, Other)
      end
  end.

trim_output(Output) ->
  Text = binary_to_list(Output),
  case length(Text) > 4000 of
    true ->
      lists:sublist(Text, 4000);
    false ->
      Text
  end.


%%====================================================================
%% Environment and names
%%====================================================================

project_dir() ->
  case os:getenv("PERFORMANCE_APP_DIR") of
    false ->
      project_dir_from_code_path();
    ProjectDir ->
      ProjectDir
  end.

project_dir_from_code_path() ->
  case code:lib_dir(ecall) of
    {error, Reason} ->
      ct:fail({cannot_resolve_project_dir, Reason});
    AppDir ->
      project_dir_with_dockerfile(candidate_project_dirs(AppDir), AppDir)
  end.

candidate_project_dirs(AppDir) ->
  SourceDir =
    case dir_before_build(AppDir) of
      false ->
        AppDir;
      Dir ->
        Dir
    end,
  case SourceDir of
    AppDir ->
      [AppDir];
    _Other ->
      [SourceDir, AppDir]
  end.

dir_before_build(AppDir) ->
  dir_before_build(filename:split(AppDir), []).

dir_before_build(["_build" | _Rest], []) ->
  false;
dir_before_build(["_build" | _Rest], Prefix) ->
  filename:join(lists:reverse(Prefix));
dir_before_build([Part | Rest], Prefix) ->
  dir_before_build(Rest, [Part | Prefix]);
dir_before_build([], _Prefix) ->
  false.

project_dir_with_dockerfile([Dir | Rest], AppDir) ->
  Dockerfile = filename:join([Dir, "test", "performance", "Dockerfile"]),
  case filelib:is_file(Dockerfile) of
    true ->
      Dir;
    false ->
      project_dir_with_dockerfile(Rest, AppDir)
  end;
project_dir_with_dockerfile([], AppDir) ->
  ct:fail({cannot_resolve_project_dir, AppDir}).

local_node_host() ->
  env("PERFORMANCE_LOCAL_NODE_HOST", "127.0.0.1").

env(Name, Default) ->
  case os:getenv(Name) of
    false ->
      Default;
    Value ->
      Value
  end.

integer_env(Name, Default) ->
  try list_to_integer(env(Name, integer_to_list(Default))) of
    Integer ->
      Integer
  catch
    error:badarg ->
      ct:fail({invalid_integer_env, Name})
  end.

unique_name(Prefix) ->
  "ecall-performance-" ++ Prefix ++ "-" ++ os:getpid() ++ "-" ++ unique_suffix().

unique_cookie() ->
  "cookie_" ++ unique_suffix().

unique_suffix() ->
  integer_to_list(erlang:unique_integer([monotonic, positive]), 36).

shell_quote(Value) ->
  "'" ++ shell_quote_chars(Value) ++ "'".

shell_quote_chars([]) ->
  [];
shell_quote_chars([$' | Rest]) ->
  "'\\''" ++ shell_quote_chars(Rest);
shell_quote_chars([Char | Rest]) ->
  [Char | shell_quote_chars(Rest)].
