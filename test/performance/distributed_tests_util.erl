-module(distributed_tests_util).

-include_lib("common_test/include/ct.hrl").

-export([
  role_config/1,
  public_role_config/1,
  start_node/2,
  connect/2,
  stop_all/0
]).

-define(STATE_KEY, {?MODULE, state}).
-define(DEFAULT_IMAGE, "ecall-performance:otp27").
-define(DEFAULT_BASE_IMAGE, "erlang:27.2.2").
-define(COMMAND_TIMEOUT, 30000).
-define(BUILD_TIMEOUT, 300000).
-define(REMOTE_TIMEOUT, 300000).
-define(RPC_TIMEOUT, 10000).

role_config(_Config) ->
  Source =
    case os:getenv("PERFORMANCE_TEST_CONFIG") of
      false ->
        role_config_from_ct();
      Path ->
        consult_role_config(Path)
    end,
  normalize_role_config(Source).

public_role_config(RoleConfig) ->
  maps:map(fun(_Role, Location) -> public_role_location(Location) end, RoleConfig).

start_node(Role, RoleConfig) ->
  Started = maps:get(roles, state()),
  case maps:get(Role, Started, undefined) of
    #{node := Node} ->
      Node;
    undefined ->
      start_new_node(Role, maps:get(Role, RoleConfig), RoleConfig)
  end.

connect(NodeA, NodeB) ->
  true = rpc:call(NodeA, net_kernel, connect_node, [NodeB], ?RPC_TIMEOUT),
  wait_until(
    fun() ->
      lists:member(NodeB, rpc:call(NodeA, erlang, nodes, [connected], ?RPC_TIMEOUT))
    end,
    ?RPC_TIMEOUT).

stop_all() ->
  State = state(),
  Started = maps:values(maps:get(roles, State, #{})),
  [stop_peer(maps:get(peer, RoleState)) || RoleState <- Started],
  [remove_role_container(RoleState) || RoleState <- Started],
  remove_password_files(),
  persistent_term:erase(?STATE_KEY),
  ok.

start_new_node(Role, Location, RoleConfig) ->
  Docker = docker_executable(),
  AppDir = app_dir(),
  Image = image_name(),
  BaseImage = base_image(Docker),
  Cookie = ensure_controller_cookie(RoleConfig),
  ensure_local_image(Docker, AppDir, Image, BaseImage),
  StartedRole = start_role(Role, Location, Docker, Image, BaseImage, Cookie, RoleConfig),
  put_started_role(Role, StartedRole),
  maps:get(node, StartedRole).

start_role(Role, local, Docker, Image, _BaseImage, Cookie, _RoleConfig) ->
  Container = unique_name(atom_to_list(Role)),
  Port = role_dist_port(Role),
  Node = role_node(Role, local_node_host()),
  Exec = {Docker, local_docker_run_args(Container, Image)},
  {ok, Peer, Node} = peer:start(peer_options(Node, Cookie, Port, Exec)),
  verify_node(Peer, Node),
  #{
    role => Role,
    location => local,
    peer => Peer,
    node => Node,
    container => Container
  };
start_role(Role, #{host := _Host} = HostConfig0, _Docker, Image, BaseImage, Cookie, _RoleConfig) ->
  HostConfig = prepare_remote_host(Image, BaseImage, HostConfig0),
  Container = unique_name(atom_to_list(Role)),
  Port = role_dist_port(Role),
  Node = role_node(Role, maps:get(node_host, HostConfig)),
  Exec = remote_docker_run_exec(HostConfig, Container, Image),
  {ok, Peer, Node} =
    peer:start(peer_options(Node, Cookie, Port, Exec, remote_env(HostConfig))),
  verify_node(Peer, Node),
  #{
    role => Role,
    location => remote,
    host_config => HostConfig,
    peer => Peer,
    node => Node,
    container => Container
  }.

peer_options(Node, Cookie, DistPort, Exec) ->
  peer_options(Node, Cookie, DistPort, Exec, []).

peer_options(Node, Cookie, DistPort, Exec, Env) ->
  #{
    name => Node,
    longnames => true,
    connection => standard_io,
    shutdown => 1000,
    exec => Exec,
    env => Env,
    args => erl_args(Cookie, DistPort)
  }.

erl_args(Cookie, DistPort) ->
  ["-pa" | container_code_paths()] ++
    [
      "-setcookie", Cookie,
      "-kernel",
      "inet_dist_listen_min", integer_to_list(DistPort),
      "inet_dist_listen_max", integer_to_list(DistPort)
    ].

container_code_paths() ->
  [
    "/opt/ecall/ebin",
    "/opt/ecall/test",
    "/opt/ecall/test/performance",
    "/opt/ecall/perf_tests"
  ].

local_docker_run_args(Container, Image) ->
  [
    "run", "--rm",
    "--name", Container,
    "--network", "host",
    "-i",
    Image
  ].

remote_docker_run_exec(HostConfig, Container, Image) ->
  remote_exec(HostConfig, [
    "docker", "run", "--rm",
    "--name", Container,
    "--network", "host",
    "-i",
    Image
  ]).

verify_node(Peer, Node) ->
  Node = peer:call(Peer, erlang, node, [], ?RPC_TIMEOUT),
  ok.

put_started_role(Role, StartedRole) ->
  State = state(),
  Roles = maps:get(roles, State, #{}),
  put_state(State#{roles => Roles#{Role => StartedRole}}).

state() ->
  persistent_term:get(?STATE_KEY, #{roles => #{}}).

put_state(State) ->
  persistent_term:put(?STATE_KEY, State).

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
    _ ->
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
  case os:getenv("PERFORMANCE_CONTROLLER_NODE_HOST") of
    false ->
      controller_node_host_from_roles(RoleConfig);
    Host ->
      Host
  end.

controller_node_host_from_roles(RoleConfig) ->
  RemoteRoles =
    [Location || Location <- maps:values(RoleConfig),
                 is_map(Location),
                 maps:is_key(host, Location)],
  case RemoteRoles of
    [] ->
      "127.0.0.1";
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
  Base = dist_port_base(),
  Base + role_port_offset(Role).

dist_port_base() ->
  State = state(),
  case maps:get(dist_port_base, State, undefined) of
    undefined ->
      Default = 4443,
      Base = integer_value(dist_port_base, env("PERFORMANCE_DIST_PORT_BASE", integer_to_list(Default))),
      put_state(State#{dist_port_base => Base}),
      Base;
    Base ->
      Base
  end.

role_port_offset(sender) ->
  1;
role_port_offset(receiver) ->
  2;
role_port_offset(Role) ->
  10 + erlang:phash2(Role, 1000).

role_config_from_ct() ->
  case ct:get_config(role_config, undefined) of
    undefined ->
      #{sender => local, receiver => local};
    Config ->
      Config
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
    {ok, {role_config, Config}} ->
      Config;
    {ok, Config} when is_map(Config) ->
      Config;
    {ok, Other} ->
      ct:fail({invalid_role_config_term, Path, Other});
    {error, ErrorInfo} ->
      ct:fail({invalid_role_config_term, Path, ErrorInfo})
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

public_role_location(local) ->
  local;
public_role_location(Location) when is_map(Location) ->
  maps:remove(password, Location).

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

ensure_local_image(Docker, AppDir, Image, BaseImage) ->
  case env_true("PERFORMANCE_FORCE_IMAGE_BUILD") orelse
       not image_exists(Docker, Image) of
    true ->
      Context = build_context(AppDir),
      Dockerfile = filename:join([AppDir, "test", "performance", "Dockerfile"]),
      ct:pal("Building performance image ~s from ~s", [Image, BaseImage]),
      command_ok(
        "docker build",
        Docker,
        [
          "build",
          "--file", Dockerfile,
          "--build-arg", "BASE_IMAGE=" ++ BaseImage,
          "--tag", Image,
          Context
        ],
        ?BUILD_TIMEOUT,
        []);
    false ->
      ok
  end.

image_exists(Docker, Image) ->
  case run(Docker, ["image", "inspect", Image], ?COMMAND_TIMEOUT) of
    {0, _Output} ->
      true;
    _ ->
      false
  end.

build_context(AppDir) ->
  AppDir.

prepare_remote_host(Image, BaseImage, HostConfig) ->
  _ = remote_ok(HostConfig, ["docker", "version", "--format", "{{.Server.Version}}"], ?COMMAND_TIMEOUT),
  remote_ensure_image(HostConfig, Image, BaseImage),
  HostConfig.

remote_ensure_image(HostConfig, Image, BaseImage) ->
  ForceBuild = env_true("PERFORMANCE_FORCE_IMAGE_BUILD"),
  case {ForceBuild, remote_image_exists(HostConfig, Image)} of
    {false, true} ->
      ok;
    _ ->
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
  AppDir = app_dir(),
  Context = build_context(AppDir),
  Dockerfile = filename:join([AppDir, "test", "performance", "Dockerfile"]),
  RemoteBuild =
    remote_command_string(
      HostConfig,
      [
        "docker", "build",
        "--build-arg", "BASE_IMAGE=" ++ BaseImage,
        "--tag", Image,
        "-"
      ]),
  Command =
    "tmp=$(mktemp -d) && "
    "trap 'rm -rf \"$tmp\"' EXIT && "
    "cp " ++ shell_quote(Dockerfile) ++ " \"$tmp/Dockerfile\" && "
    "tar -C " ++ shell_quote(Context) ++ " -cf - . | tar -C \"$tmp\" -xf - && "
    "tar -C \"$tmp\" -cf - . | " ++ RemoteBuild,
  command_ok("remote docker build", "sh", ["-c", Command], ?REMOTE_TIMEOUT, remote_env(HostConfig)),
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
    _Password ->
      {sshpass_executable(), ["-f", password_file(HostConfig), ssh_executable() | SshArgs], []}
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

remote_env(HostConfig) ->
  case maps:get(password, HostConfig, undefined) of
    undefined ->
      [];
    _Password ->
      []
  end.

remove_role_container(#{location := local, container := Container}) ->
  _ = run(docker_executable(), ["rm", "-f", Container], ?COMMAND_TIMEOUT),
  ok;
remove_role_container(#{location := remote,
                        host_config := HostConfig,
                        container := Container}) ->
  _ = remote_run(HostConfig, ["docker", "rm", "-f", Container], ?COMMAND_TIMEOUT),
  ok.

stop_peer(undefined) ->
  ok;
stop_peer(Peer) ->
  catch peer:stop(Peer),
  ok.

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

app_dir() ->
  case os:getenv("PERFORMANCE_APP_DIR") of
    false ->
      code:lib_dir(ecall);
    AppDir ->
      AppDir
  end.

image_name() ->
  env("PERFORMANCE_IMAGE", ?DEFAULT_IMAGE).

base_image(Docker) ->
  case os:getenv("PERFORMANCE_BASE_IMAGE") of
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

local_node_host() ->
  env("PERFORMANCE_LOCAL_NODE_HOST", "127.0.0.1").

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

integer_value(Key, Value) ->
  try list_to_integer(Value) of
    Integer ->
      Integer
  catch
    error:badarg ->
      ct:fail({invalid_integer, Key, Value})
  end.

trim_output(Output) ->
  Text = binary_to_list(Output),
  case length(Text) > 4000 of
    true ->
      lists:sublist(Text, 4000);
    false ->
      Text
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
