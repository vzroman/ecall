
-module(ecall_erpc).

-include("ecall.hrl").

%%=================================================================
%% API
%%=================================================================
-export([
  call/4
]).

call(Node, Module, Function, Args)->
  try {ok, erpc:call(Node, Module, Function, Args)}
  catch
    throw:Error -> {error, Error};
    exit:{_,Reason} -> {error,{exit, Reason}};
    error:{exception, Error, _Stack}-> {error, {exit,Error}};
    error:{erpc, Reason}->{error,{badrpc, Reason}};
    _:Error-> {error,{unexpected, Error}}
  end.


