
-module(ecall_app).

-behaviour(application).

%% OTP API
-export([
    start/2,
    stop/1
]).

-spec start(application:start_type(), term()) -> supervisor:startlink_ret().
start(_StartType, _StartArgs) ->
    case ecall_receive:pool_size() of
        {error, _} = Error ->
            Error;
        PoolSize ->
            ecall_sup:start_link(PoolSize)
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
