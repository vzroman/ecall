
-module(ecall_scale_SUITE).


-define(PROCESSES,50000).
-define(ITERATIONS,20).
-define(CYCLE, 1000).


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
  make_ref_test/1
]).


all()->
  [
     {group, make_ref_test}
  ].

groups()->[
  {make_ref_test,
    [parallel],
    [make_ref_test||_<-lists:seq(1,?PROCESSES)]
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

make_ref_test( _Config )->

  make_ref_test_loop( ?ITERATIONS ),

  ok.

make_ref_test_loop( Count ) when Count > 0->
  _Ref = make_ref(),
  make_ref_test_loop( Count - 1 );
make_ref_test_loop( _Count )->
  ok.
