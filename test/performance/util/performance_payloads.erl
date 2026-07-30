-module(performance_payloads).

%% API
-export([
  profiles/0,
  new/1
]).

-type profile() :: tiny | data | binary_100kib | binary_1mib.

-export_type([
  profile/0
]).

%%====================================================================
%% API
%%====================================================================

-spec profiles() -> [profile()].
profiles() ->
  %[tiny, data, binary_100kib, binary_1mib].
  [tiny, data].

-spec new(profile()) -> term().
new(tiny) ->
  tiny;
new(data) ->
  #{
    archive1 => data_archive(),
    archive2 => data_archive(),
    archive3 => data_archive()
  };
new(binary_100kib) ->
  binary:copy(<<0>>, 102400);
new(binary_1mib) ->
  binary:copy(<<0>>, 1048576).


%%====================================================================
%% INTERNAL FUNCTIONS
%%====================================================================

data_archive() ->
  #{
    field1 => value1,
    field2 => value2,
    field3 => value3,
    field4 => value4,
    field5 => value5,
    field6 => value6,
    field7 => value7,
    field8 => value8,
    field9 => value9,
    field10 => value10
  }.
