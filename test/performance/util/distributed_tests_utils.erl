-module(distributed_tests_utils).

-include_lib("common_test/include/ct.hrl").

%% API
-export([
  start_nodes/1,
  stop_nodes/1
]).

-define(STATE_KEY, {?MODULE, state}).
-define(IMAGE, "ecall-performance:otp27").
-define(BASE_IMAGE, "erlang:27.2.2").
-define(COMMAND_TIMEOUT, 30000).
-define(BUILD_TIMEOUT, 300000).
-define(REMOTE_TIMEOUT, 300000).
-define(RPC_TIMEOUT, 30000).
-define(PROCESS_LIMIT, 134217727).
-define(PORT_LIMIT, 1048576).
-define(ETS_LIMIT, 262144).


%%====================================================================
%% API
%%====================================================================

-spec start_nodes([map()]) -> [node()].
start_nodes(NodeConfigs) ->
  EnvSettings = env_settings(),
  Cookie = ensure_controller_cookie(NodeConfigs),
  Nodes = [start_node(NodeConfig, Cookie, EnvSettings)
           || NodeConfig <- NodeConfigs],
  ok = connect_nodes(Nodes),
  ok = start_ecall(Nodes, EnvSettings),
  ok = connect_ecall(Nodes),
  Nodes.

-spec stop_nodes([node()]) -> ok.
stop_nodes(Nodes) ->
  [stop_node(Node) || Node <- Nodes],
  remove_password_files(),
  ok.


%%====================================================================
%% STARTUP
%%====================================================================

start_node(#{location := local} = Config, Cookie, EnvSettings) ->
  Docker = docker_executable(),
  ProjectDir = project_dir(),
  ensure_local_image(Docker, ProjectDir),
  Name = maps:get(name, Config),
  Container = unique_name(atom_to_list(Name)),
  Node = node_name(Name, local_node_host()),
  Exec = {Docker, docker_run_args(Container)},
  start_peer(Node, Exec, Container, local, Config, Cookie, EnvSettings);
start_node(#{location := Location0} = Config, Cookie, EnvSettings) ->
  Location = remote_location(Location0),
  prepare_remote_host(Location),
  Name = maps:get(name, Config),
  Container = unique_name(atom_to_list(Name)),
  Node = node_name(Name, maps:get(node_host, Location)),
  Exec = remote_docker_run_exec(Location, Container),
  start_peer(Node, Exec, Container, {remote, Location}, Config, Cookie, EnvSettings).

start_peer(Node, Exec, Container, Location, Config, Cookie, EnvSettings) ->
  State0 = node_state(Node, Container, Location, undefined),
  put_started_node(Node, State0),
  DistPort = node_dist_port(maps:get(name, Config)),
  {ok, Peer, Node} =
    peer:start(peer_options(Node, Cookie, DistPort, Exec, EnvSettings)),
  put_started_node(Node, State0#{peer => Peer}),
  Node.

peer_options(Node, Cookie, DistPort, Exec, EnvSettings) ->
  #{
    name => Node,
    longnames => true,
    connection => standard_io,
    shutdown => 1000,
    exec => Exec,
    args => erl_args(Cookie, DistPort, EnvSettings)
  }.

erl_args(Cookie, DistPort, EnvSettings) ->
  BusyKiB =
    integer_to_list(maps:get(distribution_busy_limit_kib, EnvSettings)),
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

remote_docker_run_exec(Location, Container) ->
  remote_exec(Location, ["docker" | docker_run_args(Container)]).

remote_location(Location) ->
  Location#{
    port => maps:get(port, Location, "22"),
    node_host => maps:get(node_host, Location, maps:get(host, Location))
  }.

node_name(Name, Host) ->
  list_to_atom(atom_to_list(Name) ++ "@" ++ Host).

node_dist_port(sender) ->
  dist_port_base() + 1;
node_dist_port(receiver) ->
  dist_port_base() + 2;
node_dist_port(Name) ->
  dist_port_base() + 10 + erlang:phash2(Name, 1000).


%%====================================================================
%% READINESS
%%====================================================================

connect_nodes(Nodes) ->
  [pong = net_adm:ping(Node) || Node <- Nodes],
  [connect_node_pair(FromNode, ToNode)
   || FromNode <- Nodes,
      ToNode <- Nodes,
      FromNode =/= ToNode],
  ok.

connect_node_pair(FromNode, ToNode) ->
  true =
    rpc:call(
      FromNode,
      net_kernel,
      connect_node,
      [ToNode],
      ?RPC_TIMEOUT),
  ok = wait_connected(FromNode, ToNode).

wait_connected(FromNode, ToNode) ->
  wait_until(
    fun() ->
      lists:member(
        ToNode,
        rpc:call(FromNode, erlang, nodes, [connected], ?RPC_TIMEOUT))
    end,
    ?RPC_TIMEOUT).

start_ecall(Nodes, EnvSettings) ->
  BatchSize = maps:get(ecall_batch_size, EnvSettings),
  [
    begin
      ok =
        rpc:call(
          Node,
          application,
          set_env,
          [ecall, batch_size, BatchSize],
          ?RPC_TIMEOUT),
      {ok, _Started} =
        rpc:call(
          Node,
          application,
          ensure_all_started,
          [ecall],
          ?RPC_TIMEOUT),
      ok
    end
    || Node <- Nodes
  ],
  ok.

connect_ecall(Nodes) ->
  [connect_ecall_pair(FromNode, ToNode)
   || FromNode <- Nodes,
      ToNode <- Nodes,
      FromNode =/= ToNode],
  ok.

connect_ecall_pair(FromNode, ToNode) ->
  ok = maybe_connect_ecall_pair(FromNode, ToNode),
  {ok, #{status := connected}} = ecall_connection_info(FromNode, ToNode),
  ok.

maybe_connect_ecall_pair(FromNode, ToNode) ->
  case current_ecall_connection_info(FromNode, ToNode) of
    {ok, #{status := connected}} ->
      ok;
    _Other ->
      _ =
        rpc:call(
          FromNode,
          ecall_connection,
          connect,
          [ToNode],
          ?RPC_TIMEOUT),
      ok
  end.

current_ecall_connection_info(FromNode, ToNode) ->
  rpc:call(
    FromNode,
    ecall_connection,
    connection_info,
    [ToNode],
    ?RPC_TIMEOUT).

ecall_connection_info(FromNode, ToNode) ->
  wait_until(
    fun() ->
      case current_ecall_connection_info(FromNode, ToNode) of
        {ok, #{status := connected}} = Info ->
          Info;
        _Other ->
          false
      end
    end,
    ?RPC_TIMEOUT).

wait_until(Predicate, Timeout) ->
  Deadline = erlang:monotonic_time(millisecond) + Timeout,
  wait_until(Predicate, Deadline, undefined).

wait_until(Predicate, Deadline, LastResult) ->
  case Predicate() of
    false ->
      Now = erlang:monotonic_time(millisecond),
      case Now >= Deadline of
        true ->
          exit({timeout, LastResult});
        false ->
          timer:sleep(100),
          wait_until(Predicate, Deadline, false)
      end;
    true ->
      ok;
    Result ->
      Result
  end.


%%====================================================================
%% SHUTDOWN
%%====================================================================

stop_node(Node) ->
  State = state(),
  Nodes = maps:get(nodes, State, #{}),
  NodeState = maps:get(Node, Nodes),
  stop_peer(maps:get(peer, NodeState)),
  remove_node_container(NodeState),
  put_state(State#{nodes => maps:remove(Node, Nodes)}),
  ok.

node_state(Node, Container, local, Peer) ->
  #{
    node => Node,
    container => Container,
    location => local,
    peer => Peer
  };
node_state(Node, Container, {remote, Location}, Peer) ->
  #{
    node => Node,
    container => Container,
    location => remote,
    host_config => Location,
    peer => Peer
  }.

stop_peer(Peer) ->
  catch peer:stop(Peer),
  ok.

remove_node_container(#{location := local, container := Container}) ->
  _ = run(docker_executable(), ["rm", "-f", Container], ?COMMAND_TIMEOUT),
  ok;
remove_node_container(#{location := remote,
                        host_config := Location,
                        container := Container}) ->
  _ = remote_run(Location, ["docker", "rm", "-f", Container], ?COMMAND_TIMEOUT),
  ok.


%%====================================================================
%% STATE
%%====================================================================

state() ->
  persistent_term:get(?STATE_KEY, #{nodes => #{}, password_files => #{}}).

put_state(State) ->
  persistent_term:put(?STATE_KEY, State).

put_started_node(Node, NodeState) ->
  State = state(),
  Nodes = maps:get(nodes, State, #{}),
  put_state(State#{nodes => Nodes#{Node => NodeState}}).


%%====================================================================
%% DOCKER AND REMOTE COMMANDS
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

prepare_remote_host(Location) ->
  _ = remote_ok(
        Location,
        ["docker", "version", "--format", "{{.Server.Version}}"],
        ?COMMAND_TIMEOUT),
  remote_build_image(Location).

remote_build_image(Location) ->
  ProjectDir = project_dir(),
  Dockerfile = filename:join([ProjectDir, "test", "performance", "Dockerfile"]),
  RemoteBuild =
    remote_command_string(
      Location,
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

remote_ok(Location, RemoteArgs, Timeout) ->
  {Exec, Args, Env} = remote_exec_parts(Location, RemoteArgs),
  command_ok("remote command", Exec, Args, Timeout, Env).

remote_run(Location, RemoteArgs, Timeout) ->
  {Exec, Args, Env} = remote_exec_parts(Location, RemoteArgs),
  run(Exec, Args, Timeout, Env).

remote_exec(Location, RemoteArgs) ->
  {Exec, Args, _Env} = remote_exec_parts(Location, RemoteArgs),
  {Exec, Args}.

remote_exec_parts(Location, RemoteArgs) ->
  SshArgs = ssh_args(Location) ++ RemoteArgs,
  case maps:get(password, Location, undefined) of
    undefined ->
      {ssh_executable(), SshArgs, []};
    _Password ->
      {sshpass_executable(), ["-f", password_file(Location), ssh_executable()
                              | SshArgs], []}
  end.

remote_command_string(Location, RemoteArgs) ->
  {Exec, Args, _Env} = remote_exec_parts(Location, RemoteArgs),
  string:join([shell_quote(Exec) | [shell_quote(Arg) || Arg <- Args]], " ").

ssh_args(Location) ->
  User = maps:get(user, Location),
  Host = maps:get(host, Location),
  Port = maps:get(port, Location),
  AuthArgs =
    case maps:get(password, Location, undefined) of
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


%%====================================================================
%% CONTROLLER DISTRIBUTION
%%====================================================================

ensure_controller_cookie(NodeConfigs) ->
  Cookie = unique_cookie(),
  ensure_controller_node(Cookie, NodeConfigs),
  Cookie.

ensure_controller_node(Cookie, NodeConfigs) ->
  case node() of
    nonode@nohost ->
      set_controller_dist_port(),
      Name = controller_node_name(NodeConfigs),
      {ok, _Pid} = net_kernel:start([Name, longnames]),
      set_cookie(Cookie);
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

controller_node_name(NodeConfigs) ->
  Host = controller_node_host(NodeConfigs),
  list_to_atom("performance_controller_" ++ os:getpid() ++ "@" ++ Host).

controller_node_host(NodeConfigs) ->
  case remote_locations(NodeConfigs) of
    [] ->
      local_node_host();
    [Location | _Rest] ->
      detect_controller_host(Location)
  end.

remote_locations(NodeConfigs) ->
  [
    remote_location(Location)
    || #{location := Location} <- NodeConfigs,
       Location =/= local
  ].

detect_controller_host(Location) ->
  {0, Output} = remote_run(Location, ["printenv", "SSH_CLIENT"], ?COMMAND_TIMEOUT),
  [Host | _] = string:tokens(string:trim(binary_to_list(Output)), " \t"),
  Host.


%%====================================================================
%% COMMAND HELPERS
%%====================================================================

password_file(Location) ->
  Key = {maps:get(host, Location),
         maps:get(port, Location),
         maps:get(user, Location)},
  State = state(),
  PasswordFiles = maps:get(password_files, State, #{}),
  case maps:get(Key, PasswordFiles, undefined) of
    undefined ->
      Path = filename:join("/tmp", unique_name("sshpass")),
      ok = file:write_file(Path, maps:get(password, Location)),
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
  put_state(State#{password_files => #{}}),
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
  Exec = resolve_executable(Exec0),
  Port =
    open_port(
      {spawn_executable, Exec},
      [{args, Args}, {env, Env}, binary, exit_status, stderr_to_stdout]),
  collect_port(Port, Timeout, []).

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

trim_output(Output) ->
  Text = binary_to_list(Output),
  case length(Text) > 4000 of
    true ->
      lists:sublist(Text, 4000);
    false ->
      Text
  end.


%%====================================================================
%% ENVIRONMENT AND NAMES
%%====================================================================

env_settings() ->
  maps:merge(default_env_settings(), ct:get_config(env_settings, #{})).

default_env_settings() ->
  #{
    ecall_batch_size => 1000,
    distribution_busy_limit_kib => 1024
  }.

project_dir() ->
  case os:getenv("PERFORMANCE_APP_DIR") of
    false ->
      project_dir_from_code_path();
    ProjectDir ->
      ProjectDir
  end.

project_dir_from_code_path() ->
  {ok, Cwd} = file:get_cwd(),
  case filelib:is_file(filename:join([Cwd, "test", "performance", "Dockerfile"])) of
    true ->
      Cwd;
    false ->
      {ok, AppDir} = file:get_cwd(),
      AppDir
  end.

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
  list_to_integer(env(Name, integer_to_list(Default))).

dist_port_base() ->
  integer_env("PERFORMANCE_DIST_PORT_BASE", 4443).

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
