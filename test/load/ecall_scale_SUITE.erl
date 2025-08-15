
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
  Config.

end_per_suite(_Config)->
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
