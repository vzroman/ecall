
-module(ecall_scale_SUITE).

-include("ecall.hrl").

%% API
-export([
  all/0,
  groups/0,
  init_per_testcase/2,
  end_per_testcase/2,
  init_per_group/2,
  end_per_group/2,
  init_per_suite/1,
  end_per_suite/1
]).

%% Create group
-export([
  distributed_test/1
]).


all()->
  [
     {group, distributed_test}
  ].

groups()->[
  {distributed_test,
    [sequence],
    [
      distributed_test
    ]
  }
].

init_per_suite(Config)->
  build_image(Config),
  Nodes = start_nodes( _Count = 1 ),
  ?LOGDEBUG("Nodes: ~p",[Nodes]),
  connect_nodes(Nodes),
  ?LOGDEBUG("Connected nodes: ~p",[erlang:nodes()]),
  [{nodes, Nodes} | Config].

end_per_suite( Config )->
  Nodes = proplists:get_value(nodes, Config),
  stop_nodes( Nodes ),
  remove_image(),
  ok.

init_per_group(_,Config)->
  Config.

end_per_group(_,_Config)->
  ok.

init_per_testcase(_,Config)->
  Config.

end_per_testcase(_,_Config)->
  ok.

distributed_test( _Config )->
  ?LOGDEBUG("distributed_test"),
  ok.

%%--------------------------------------------------------------
%%  UTILITIES
%%--------------------------------------------------------------
build_image(Config)->
  PrivDir = proplists:get_value(priv_dir, Config),
  [ProjectRoot|_] = string:split(PrivDir, "/_build/test/logs"),
  Sources = filename:join(PrivDir, "src"),
  CopySrc = lists:join(" && ",[
    "mkdir "++Sources,
    "cp -R "++ProjectRoot++"/config "++Sources++"/",
    "cp -R "++ProjectRoot++"/include "++Sources++"/",
    "cp -R "++ProjectRoot++"/src "++Sources++"/",
    "cp -R "++ProjectRoot++"/test "++Sources++"/",
    "cp -R "++ProjectRoot++"/rebar3 "++Sources++"/",
    "cp -R "++ProjectRoot++"/rebar.config "++Sources++"/",
    "sed -i 's/^-name/##-name/' "++Sources++"/config/vm.args"
  ]),
  ?LOGDEBUG("CopySrc ~p",[CopySrc]),
  os:cmd( CopySrc ,#{ exception_on_failure => true }),

%%  peer:start_link(),

  Dockerfile = lists:join("\n",[
    "FROM vzroman/erlang_otp:v27.2.3",
    "EXPOSE 4445\n"
    "ENV SRC=/opt/ecall",
    "ENV ERL_FLAGS=\"-args_file config/vm.args -config config/sys.config -user peer\"",
%%    "ENV ERL_FLAGS=\"-args_file config/vm.args -config config/sys.config\"",
    "RUN mkdir $SRC",
    "COPY ./src $SRC/",
    "WORKDIR $SRC",
    "ENTRYPOINT [ \"./rebar3\", \"as\", \"test\", \"shell\" ]"
  ]),
  ?LOGDEBUG("Dockerfile ~p",[Dockerfile]),
  ok = file:write_file(filename:join(PrivDir, "Dockerfile"), Dockerfile),
  os:cmd("docker build -t ecall " ++ PrivDir, #{ exception_on_failure => true }).

remove_image()->
  os:cmd("docker stop $(docker ps -a --filter \"ancestor=ecall\" --format \"{{.ID}}\") && docker rm $(docker ps -a --filter \"ancestor=ecall\" --format \"{{.ID}}\")"),
  os:cmd("docker rmi ecall"),
  ok.

start_nodes( Count  )->
  lists:foldl(
    fun(I, Acc)->
      {Node, Peer} = start_node("host"++integer_to_list(I)++".ecall"),
      Acc#{ Node => Peer }
    end, #{}, lists:seq(1,Count) ).

start_node( Host )->
  Docker = os:find_executable("docker"),
  {ok, Peer, Node} = ecall_peer:start(#{
    name => "ecall",
    host => Host,
    longnames => true,
    connection => standard_io,
    post_process_args => fun to_rebar_args/1,
    exec => {Docker, ["run", "-h", Host, "-i", "ecall"]}}),

  {Node, Peer}.

to_rebar_args(["-name", Name|Rest])->
  ["--name",Name|to_rebar_args(Rest)];
to_rebar_args(["-user", _User|Rest])->
  to_rebar_args(Rest);
to_rebar_args([Option|Rest])->
  [Option|to_rebar_args(Rest)];
to_rebar_args([])->
  [].

connect_nodes(Nodes)->
  inet_db:set_lookup([file]),

  maps:foreach(
    fun(Node, Peer)->
      ?LOGDEBUG("try connect node ~p",[Node]),
      {ok, Ips} = peer:call(Peer, inet, getifaddrs, []),
      ?LOGDEBUG("node ~p ips: ~p",[Node, Ips]),
      {"eth0", Eth0} = lists:keyfind("eth0", 1, Ips),
      ?LOGDEBUG("node ~p eth0: ~p",[Node, Eth0]),
      {addr, Ip} = lists:keyfind(addr, 1, Eth0),
      ?LOGDEBUG("node ~p ip: ~p",[Node, Ip]),
      [_,Host] = string:split( atom_to_list(Node), "@" ),
      ?LOGDEBUG("node ~p Host: ~p",[Node, Host]),
      ok = inet_db:add_host(Ip, [Host]),
      true = net_kernel:connect_node(Node),
      ?LOGDEBUG("~p nodes: ~p",[Node, peer:call(Peer, erlang, nodes, [])])
    end, Nodes).

stop_nodes( Nodes )->
  [ peer:stop(Peer) || Peer <- maps:values( Nodes ) ],
  ok.