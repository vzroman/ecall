
-module(ecall_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    ecall_sup:start_link().

stop(_State) ->
    ok.

