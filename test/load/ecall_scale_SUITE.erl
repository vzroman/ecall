
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
    "cp -R "++ProjectRoot++"/include "++Sources++"/",
    "cp -R "++ProjectRoot++"/src "++Sources++"/",
    "cp -R "++ProjectRoot++"/test "++Sources++"/",
    "cp -R "++ProjectRoot++"/rebar3 "++Sources++"/",
    "cp -R "++ProjectRoot++"/rebar.config "++Sources++"/",
    "cp -R "++ProjectRoot++"/run_as_peer "++Sources++"/"
  ]),
  ?LOGDEBUG("CopySrc ~p",[CopySrc]),
  os:cmd( CopySrc ,#{ exception_on_failure => true }),

  Dockerfile = lists:join("\n",[
    "FROM vzroman/erlang_otp:v27.2.3",
    "ENV SRC=/opt/ecall",
    "RUN mkdir $SRC",
    "COPY ./src $SRC/",
    "WORKDIR $SRC",
    "ENTRYPOINT [ \"./run_as_peer\" ]"
  ]),
  ?LOGDEBUG("Dockerfile ~p",[Dockerfile]),
  ok = file:write_file(filename:join(PrivDir, "Dockerfile"), Dockerfile),
  os:cmd("docker build -t ecall " ++ PrivDir, #{ exception_on_failure => true }).


start_nodes( Count  )->
  lists:foldl(
    fun(I, Acc)->
      {Node, Peer} = start_node("host"++integer_to_list(I)++".ecall"),
      Acc#{ Node => Peer }
    end, #{}, lists:seq(1,Count) ).

start_node( Name )->
  Docker = os:find_executable("docker"),
  {ok, Peer, Node} = ecall_peer:start(#{name => list_to_atom("ecall@"++Name),
    connection => standard_io,
    exec => {Docker, ["run", "-h", Name, "-i", "ecall"]}}),

  {Node, Peer}.

connect_nodes(Nodes)->
  inet_db:set_lookup([file]),

  maps:foreach(
    fun(Node, Peer)->
      {ok, Ips} = peer:call(Peer, inet, getifaddrs, []),
      {"eth0", Eth0} = lists:keyfind("eth0", 1, Ips),
      {addr, Ip} = lists:keyfind(addr, 1, Eth0),
      [_,Host] = lists:split( atom_to_list(Node), "@" ),
      inet_db:add_host(Ip, [Host]),
      true = net_kernel:connect_node(Node),
      ?LOGDEBUG("~p nodes: ~p",[Node, peer:call(Peer, erlang, nodes, [])])
    end, Nodes).

stop_nodes( Nodes )->
  [ peer:stop(Peer) || Peer <- maps:values( Nodes ) ],
  ok.