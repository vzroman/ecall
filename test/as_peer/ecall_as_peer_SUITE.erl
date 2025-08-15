
-module(ecall_as_peer_SUITE).


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

-export([
  just_sleep/1
]).


all()->
  [
    just_sleep
  ].

groups()->[].

init_per_suite(Config)->
  Config.

end_per_suite( _Config )->
  ok.

init_per_group(_,Config)->
  Config.

end_per_group(_,_Config)->
  ok.

init_per_testcase(_,Config)->
  Config.

end_per_testcase(_,_Config)->
  ok.

just_sleep( _Config )->
  timer:sleep(infinity),
  ok.