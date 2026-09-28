
-module(ecall_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    case ecall_receive:pool_size() of
        {error, _} = Error ->
            Error;
        PoolSize ->
            ecall_sup:start_link(PoolSize)
    end.

stop(_State) ->
    ok.

