-module(peer_smoke_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  groups/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_group/2,
  end_per_group/2,
  init_per_testcase/2,
  end_per_testcase/2
]).

-export([
  config_accepts_role_location_maps/1,
  single_container_shutdown/1,
  two_container_distribution/1,
  configured_sender_receiver_start/1
]).

-define(DEFAULT_IMAGE, "ecall-peer-smoke:otp27").
-define(DEFAULT_BASE_IMAGE, "erlang:27.2.2").
-define(DIST_PORT, 4445).
-define(COMMAND_TIMEOUT, 30000).
-define(BUILD_TIMEOUT, 300000).
-define(REMOTE_TIMEOUT, 300000).
-define(RPC_TIMEOUT, 10000).

all() ->
  [
    {group, config},
    {group, docker_local},
    {group, docker_remote}
  ].

groups() ->
  [
    {config, [sequence], [
      config_accepts_role_location_maps
    ]},
    {docker_local, [sequence], [
      single_container_shutdown,
      two_container_distribution
    ]},
    {docker_remote, [sequence], [
      configured_sender_receiver_start
    ]}
  ].

init_per_suite(Config) ->
  case os:find_executable("docker") of
    false ->
      {skip, "docker executable not found"};
    Docker ->
      Root = root_dir(),
      Image = env("PEER_SMOKE_IMAGE", ?DEFAULT_IMAGE),
      BaseImage = default_base_image(Docker),
      ensure_image(Docker, Root, Image, BaseImage),
      [{docker, Docker}, {root, Root}, {image, Image}, {base_image, BaseImage} | Config]
  end.

end_per_suite(_Config) ->
  ok.

init_per_group(docker_remote, Config) ->
  case role_config() of
      {skip, Reason} ->
      {skip, Reason};
    {ok, RoleConfig0} ->
      Image = ?config(image, Config),
      BaseImage = ?config(base_image, Config),
      RoleConfig = prepare_remote_roles(Image, BaseImage, RoleConfig0),
      [{role_config, RoleConfig} | Config]
  end;
init_per_group(_Group, Config) ->
  Config.

end_per_group(_Group, _Config) ->
  ok.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_Case, _Config) ->
  ok.

config_accepts_role_location_maps(Config) ->
  Path = filename:join(?config(priv_dir, Config), "role-map.config"),
  Term =
    "#{\n"
    "  sender => local,\n"
    "  receiver => #{host => \"example.net\", user => \"root\", password => \"secret\"}\n"
    "}.\n",
  ok = file:write_file(Path, Term),
  #{sender := local,
    receiver := #{host := "example.net",
                  user := "root",
                  password := "secret",
                  port := "22"}} = consult_role_config(Path).

single_container_shutdown(Config) ->
  Docker = ?config(docker, Config),
  Image = ?config(image, Config),
  Container = unique_name("single"),
  Name = list_to_atom(unique_name("single-node")),
  Cookie = unique_cookie(),
  DockerArgs = [
    "run", "--rm",
    "--name", Container,
    "-i",
    Image
  ],
  Options = peer_options(Name, false, Cookie, {Docker, DockerArgs}),

  try
    {ok, Peer, Node} = peer:start_link(Options),
    try
      Node = peer:call(Peer, erlang, node, [], ?RPC_TIMEOUT),
      ok
    after
      stop_peer(Peer)
    end,
    wait_until(fun() -> not container_exists(Docker, Container) end, 10000)
  after
    remove_container(Docker, Container)
  end.

two_container_distribution(Config) ->
  Docker = ?config(docker, Config),
  Image = ?config(image, Config),
  Network = unique_name("net"),
  HostA = unique_host("one"),
  HostB = unique_host("two"),
  ContainerA = unique_name("one"),
  ContainerB = unique_name("two"),
  NodeA = list_to_atom("lambda@" ++ HostA),
  NodeB = list_to_atom("lambda@" ++ HostB),
  Cookie = unique_cookie(),

  docker_ok(Docker, ["network", "create", Network], ?COMMAND_TIMEOUT),
  try
    {ok, PeerA, NodeA} =
      peer:start_link(
        peer_options(
          NodeA,
          false,
          Cookie,
          {Docker, local_docker_run_args(ContainerA, HostA, Network, Image)})),
    try
      {ok, PeerB, NodeB} =
        peer:start_link(
          peer_options(
            NodeB,
            false,
            Cookie,
            {Docker, local_docker_run_args(ContainerB, HostB, Network, Image)})),
      try
        true = peer:call(PeerA, net_kernel, connect_node, [NodeB], ?RPC_TIMEOUT),
        wait_for_node(PeerA, NodeB),
        wait_for_node(PeerB, NodeA)
      after
        stop_peer(PeerB)
      end
    after
      stop_peer(PeerA)
    end,
    wait_until(fun() -> not container_exists(Docker, ContainerA) end, 10000),
    wait_until(fun() -> not container_exists(Docker, ContainerB) end, 10000)
  after
    remove_container(Docker, ContainerA),
    remove_container(Docker, ContainerB),
    remove_network(Docker, Network)
  end.

configured_sender_receiver_start(Config) ->
  Image = ?config(image, Config),
  RoleConfig = ?config(role_config, Config),
  Cookie = unique_cookie(),
  Started = start_configured_roles([sender, receiver], RoleConfig, Image, Cookie, Config),
  try
    Sender = maps:get(sender, Started),
    Receiver = maps:get(receiver, Started),
    verify_started_role(Sender),
    verify_started_role(Receiver),
    true =
      peer:call(
        maps:get(peer, Sender),
        net_kernel,
        connect_node,
        [maps:get(node, Receiver)],
        ?RPC_TIMEOUT),
    wait_for_node(maps:get(peer, Sender), maps:get(node, Receiver))
  after
    stop_started_roles(Started, Config)
  end.

peer_options(Name, LongNames, Cookie, Exec) ->
  peer_options(Name, LongNames, Cookie, Exec, []).

peer_options(Name, LongNames, Cookie, Exec, Env) ->
  #{
    name => Name,
    longnames => LongNames,
    connection => standard_io,
    shutdown => 1000,
    exec => Exec,
    env => Env,
    args => distribution_args(Cookie)
  }.

distribution_args(Cookie) ->
  [
    "-setcookie", Cookie,
    "-kernel",
    "inet_dist_listen_min", integer_to_list(?DIST_PORT),
    "inet_dist_listen_max", integer_to_list(?DIST_PORT)
  ].

local_docker_run_args(Container, Host, Network, Image) ->
  [
    "run", "--rm",
    "--name", Container,
    "--hostname", Host,
    "--network", Network,
    "-i",
    Image
  ].

local_role_docker_run_args(Container, Image) ->
  [
    "run", "--rm",
    "--name", Container,
    "-i",
    Image
  ].

remote_role_docker_exec(HostConfig, Container, Image) ->
  remote_exec(HostConfig, [
    "docker", "run", "--rm",
    "--name", Container,
    "--network", "host",
    "-i",
    Image
  ]).

remote_peer_env(HostConfig) ->
  case maps:get(password, HostConfig, undefined) of
    undefined ->
      [];
    Password ->
      [{"SSHPASS", Password}]
  end.

start_configured_roles(Roles, RoleConfig, Image, Cookie, Config) ->
  maps:from_list(
    [{Role, start_configured_role(Role, maps:get(Role, RoleConfig), Image, Cookie, Config)}
     || Role <- Roles]).

start_configured_role(Role, local, Image, Cookie, Config) ->
  Docker = ?config(docker, Config),
  Container = unique_name(atom_to_list(Role)),
  NodeHost = env("PEER_SMOKE_LOCAL_NODE_HOST", "127.0.0.1"),
  Node = role_node(Role, NodeHost),
  Exec = {Docker, local_role_docker_run_args(Container, Image)},
  {ok, Peer, Node} = peer:start_link(peer_options(Node, true, Cookie, Exec)),
  #{
    role => Role,
    location => local,
    peer => Peer,
    node => Node,
    container => Container
  };
start_configured_role(Role, #{host := _Host} = HostConfig, Image, Cookie, _Config) ->
  Container = unique_name(atom_to_list(Role)),
  Node = role_node(Role, maps:get(node_host, HostConfig)),
  {ok, Peer, Node} =
    peer:start_link(
      peer_options(
        Node,
        true,
        Cookie,
        remote_role_docker_exec(HostConfig, Container, Image),
        remote_peer_env(HostConfig))),
  #{
    role => Role,
    location => remote,
    host_config => HostConfig,
    peer => Peer,
    node => Node,
    container => Container
  }.

role_node(Role, Host) ->
  list_to_atom(atom_to_list(Role) ++ "@" ++ Host).

verify_started_role(StartedRole) ->
  Peer = maps:get(peer, StartedRole),
  Node = maps:get(node, StartedRole),
  Node = peer:call(Peer, erlang, node, [], ?RPC_TIMEOUT),
  ok.

stop_started_roles(Started, Config) ->
  [stop_peer(maps:get(peer, StartedRole))
   || StartedRole <- maps:values(Started)],
  [remove_started_role(StartedRole, Config)
   || StartedRole <- maps:values(Started)],
  ok.

remove_started_role(#{location := local, container := Container}, Config) ->
  remove_container(?config(docker, Config), Container);
remove_started_role(#{location := remote,
                      host_config := HostConfig,
                      container := Container}, _Config) ->
  remote_remove_container(HostConfig, Container).

wait_for_node(Peer, Node) ->
  wait_until(
    fun() ->
      lists:member(Node, peer:call(Peer, erlang, nodes, [connected], ?RPC_TIMEOUT))
    end,
    10000).

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

stop_peer(undefined) ->
  ok;
stop_peer(Peer) ->
  catch peer:stop(Peer),
  ok.

root_dir() ->
  case os:getenv("PEER_SMOKE_ROOT") of
    false ->
      {ok, Cwd} = file:get_cwd(),
      Cwd;
    Root ->
      Root
  end.

default_base_image(Docker) ->
  case os:getenv("PEER_SMOKE_BASE_IMAGE") of
    false ->
      case image_exists(Docker, "vzroman/erlang_otp:v27.2.3") of
        true ->
          "vzroman/erlang_otp:v27.2.3";
        false ->
          ?DEFAULT_BASE_IMAGE
      end;
    BaseImage ->
      BaseImage
  end.

ensure_image(Docker, Root, Image, BaseImage) ->
  case env_true("PEER_SMOKE_FORCE_IMAGE_BUILD") orelse
       not image_exists(Docker, Image) of
    true ->
      Dockerfile = filename:join([Root, "perf_tests", "peer_smoke.Dockerfile"]),
      ct:pal("Building peer smoke image ~s from ~s", [Image, BaseImage]),
      docker_ok(
        Docker,
        [
          "build",
          "--file", Dockerfile,
          "--build-arg", "BASE_IMAGE=" ++ BaseImage,
          "--tag", Image,
          Root
        ],
        ?BUILD_TIMEOUT);
    false ->
      ct:pal("Using existing peer smoke image ~s", [Image]),
      ok
  end.

image_exists(Docker, Image) ->
  case run(Docker, ["image", "inspect", Image], ?COMMAND_TIMEOUT) of
    {0, _Output} ->
      true;
    {error, _Reason} ->
      false;
    {_Status, _Output} ->
      false
  end.

container_exists(Docker, Container) ->
  Output =
    docker_ok(
      Docker,
      [
        "ps", "-a",
        "--filter", "name=^/" ++ Container ++ "$",
        "--format", "{{.Names}}"
      ],
      ?COMMAND_TIMEOUT),
  string:trim(binary_to_list(Output)) =:= Container.

remove_container(Docker, Container) ->
  _ = run(Docker, ["rm", "-f", Container], ?COMMAND_TIMEOUT),
  ok.

remove_network(Docker, Network) ->
  _ = run(Docker, ["network", "rm", Network], ?COMMAND_TIMEOUT),
  ok.

role_config() ->
  case os:getenv("PEER_SMOKE_CONFIG") of
    false ->
      {skip, "set PEER_SMOKE_CONFIG=/path/to/role.config to run configured Docker smoke"};
    Path ->
      {ok, consult_role_config(Path)}
  end.

consult_role_config(Path) ->
  case file:read_file(Path) of
    {ok, Contents} ->
      parse_role_config(Path, binary_to_list(Contents));
    {error, Reason} ->
      ct:fail({cannot_read_role_config, Path, Reason})
  end.

parse_role_config(Path, Contents) ->
  case erl_scan:string(Contents) of
    {ok, Tokens, _EndLine} ->
      parse_role_config_tokens(Path, Tokens);
    {error, ErrorInfo, _EndLine} ->
      ct:fail({invalid_role_config_tokens, Path, ErrorInfo})
  end.

parse_role_config_tokens(Path, Tokens) ->
  case erl_parse:parse_term(Tokens) of
    {ok, Config} when is_map(Config) ->
      normalize_role_config(Config);
    {ok, Other} ->
      ct:fail({invalid_role_config_term, Path, Other});
    {error, ErrorInfo} ->
      ct:fail({invalid_role_config_term, Path, ErrorInfo})
  end.

normalize_role_config(Config) ->
  #{
    sender => normalize_role_location(sender, maps:get(sender, Config, undefined)),
    receiver => normalize_role_location(receiver, maps:get(receiver, Config, undefined))
  }.

normalize_role_location(_Role, local) ->
  local;
normalize_role_location(Role, #{host := Host, user := User} = Location) ->
  Password = maps:get(password, Location, undefined),
  assert_password_support(Role, Password),
  NodeHost = maps:get(node_host, Location, Host),
  #{
    host => string_value(Role, host, Host),
    user => string_value(Role, user, User),
    password => password_value(Role, Password),
    port => string_value(Role, port, maps:get(port, Location, "22")),
    node_host => string_value(Role, node_host, NodeHost)
  };
normalize_role_location(Role, Location) ->
  ct:fail({invalid_role_location, Role, Location}).

string_value(_Role, _Key, Value) when is_list(Value) ->
  Value;
string_value(Role, Key, Value) ->
  ct:fail({invalid_role_location_value, Role, Key, Value}).

password_value(_Role, undefined) ->
  undefined;
password_value(_Role, Value) when is_list(Value) ->
  Value;
password_value(Role, Value) ->
  ct:fail({invalid_role_location_value, Role, password, Value}).

assert_password_support(_Role, undefined) ->
  ok;
assert_password_support(Role, _Password) ->
  case os:find_executable("sshpass") of
    false ->
      ct:fail({sshpass_required_for_password, Role});
    _ ->
      ok
  end.

prepare_remote_roles(Image, BaseImage, RoleConfig) ->
  maps:map(
    fun(_Role, local) ->
        local;
       (_Role, #{host := _Host} = HostConfig) ->
        prepare_remote_host(Image, BaseImage, HostConfig)
    end,
    RoleConfig).

prepare_remote_host(Image, BaseImage, HostConfig0) ->
  HostConfig = HostConfig0,
  _ = remote_ok(HostConfig, ["docker", "version", "--format", "{{.Server.Version}}"], ?COMMAND_TIMEOUT),
  remote_ensure_image(HostConfig, Image, BaseImage),
  HostConfig.

remote_ensure_image(HostConfig, Image, BaseImage) ->
  case remote_image_exists(HostConfig, Image) of
    true ->
      ct:pal("Using existing remote peer smoke image ~s on ~s",
             [Image, maps:get(host, HostConfig)]),
      ok;
    false ->
      remote_build_image(HostConfig, Image, BaseImage)
  end.

remote_image_exists(HostConfig, Image) ->
  case remote_run(HostConfig, ["docker", "image", "inspect", Image], ?COMMAND_TIMEOUT) of
    {0, _Output} ->
      true;
    _ ->
      false
  end.

remote_build_image(HostConfig, Image, BaseImage) ->
  Dockerfile =
    "ARG BASE_IMAGE\n"
    "FROM ${BASE_IMAGE}\n"
    "ENTRYPOINT [\"/usr/local/bin/erl\"]\n",
  RemoteBuildCommand =
    "tmp=$(mktemp -d) && "
    "trap 'rm -rf \"$tmp\"' EXIT && "
    "cat > \"$tmp/Dockerfile\" && "
    "docker build --file \"$tmp/Dockerfile\" --build-arg "
    ++ shell_quote("BASE_IMAGE=" ++ BaseImage)
    ++ " --tag " ++ shell_quote(Image) ++ " \"$tmp\"",
  Command =
    string:join(
      ["printf", "%s", shell_quote(Dockerfile), "|",
       remote_command_string(HostConfig, [RemoteBuildCommand])],
      " "),
  command_ok("remote docker build", "sh", ["-c", Command], ?REMOTE_TIMEOUT, remote_peer_env(HostConfig)),
  ok.

remote_remove_container(HostConfig, Container) ->
  _ = remote_run(HostConfig, ["docker", "rm", "-f", Container], ?COMMAND_TIMEOUT),
  ok.

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
    Password ->
      {sshpass_executable(), ["-e", ssh_executable() | SshArgs], [{"SSHPASS", Password}]}
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
      _ ->
        []
    end,
  [
    "-p", Port,
    "-o", "StrictHostKeyChecking=accept-new",
    "-o", "ConnectTimeout=10"
  ] ++ AuthArgs ++ [User ++ "@" ++ Host].

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

docker_ok(Docker, Args, Timeout) ->
  command_ok("docker", Docker, Args, Timeout, []).

command_ok(Label, Exec, Args, Timeout, Env) ->
  case run(Exec, Args, Timeout, Env) of
    {0, Output} ->
      Output;
    {Status, Output} when is_integer(Status) ->
      ct:fail({command_failed, Label, Status, trim_output(Output)});
    {error, Reason} ->
      ct:fail({command_failed, Label, Reason})
  end.

run(Exec, Args, Timeout) ->
  run(Exec, Args, Timeout, []).

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
    _ ->
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

env(Name, Default) ->
  case os:getenv(Name) of
    false ->
      Default;
    Value ->
      Value
  end.

env_true(Name) ->
  case string:lowercase(env(Name, "")) of
    "1" ->
      true;
    "true" ->
      true;
    "yes" ->
      true;
    _ ->
      false
  end.

unique_name(Prefix) ->
  "ecall-peer-smoke-" ++ Prefix ++ "-" ++ os:getpid() ++ "-" ++ unique_suffix().

unique_host(Prefix) ->
  Prefix ++ "-" ++ unique_suffix().

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
