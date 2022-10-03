-module(ecall_SUITE).
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_case/2, end_per_case/2]).

-export([
    call_one/1, 
    call_any/1,
    call_all/1,
    call_all_wait/1,
    cast_one/1,
    cast_all/1
]).

%% Functions to be called
-export([
    echo/1, 
    echo1/1, 
    side_effect/1
]).

-define(NODES, [
    'testnode1@host1.com',
    'testnode2@host2.com',
    'testnode3@host3.com'
]).

-define(SYNC_DELAY, 1000).


all() -> 
    [
        call_one,
        call_any,
        call_all,
        call_all_wait,
        cast_one,
        cast_all
    ].


init_per_case(Case, _Config) ->
    register('@ecall_test@', self()),
    timer:sleep(?SYNC_DELAY),
    
    ok.

end_per_case(Case, _Config) ->
    ok.



call_one(_Config) ->
    {error, _} = ecall:call_one([], ecall_SUITE, echo1, [hello]),
    {error, _} = ecall:call_one([], ecall_SUITE, echo1, [hello], true),
    {error, _} = ecall:call_one([], ecall_SUITE, echo1, [hello], false),

    {error, []} = ecall:call_one(['notestnode@nohost.com'], ecall_SUITE, echo1, [hello]),
    {error, []} = ecall:call_one(['notestnode@nohost.com'], ecall_SUITE, echo1, [hello], false),
    {error, [{'notestnode@nohost.com', {badrpc, _}}]} = ecall:call_one(['notestnode@nohost.com'], ecall_SUITE, echo, [hello], true),

    {error, _} = ecall:call_one(?NODES -- ['testnode1@host1.com'], ecall_SUITE, echo1, [hello], false),
    {error, _} = ecall:call_one(?NODES -- ['testnode1@host1.com'], ecall_SUITE, echo1, [hello], true),

    {ok, {'testnode1@host1.com', hello}} = ecall:call_one(?NODES, ecall_SUITE, echo1, [hello], true),    
    {ok, {'testnode1@host1.com', hello}} = ecall:call_one(?NODES, ecall_SUITE, echo1, [hello], false),    

    ok.


call_any(_Config) ->
    {error, _} = ecall:call_any([], ecall_SUITE, echo1, [hello]),
    {error, _} = ecall:call_any([], ecall_SUITE, echo1, [hello], true),
    {error, _} = ecall:call_any([], ecall_SUITE, echo1, [hello], false),

    {error, _} = ecall:call_any(?NODES, ecall_SUITE, echo11, [hello], false),    
    {error, _} = ecall:call_any(?NODES, ecall_SUITE, echo11, [hello], true),   
    

    {ok, {'testnode1@host1.com', hello}} = ecall:call_any(?NODES, ecall_SUITE, echo1, [hello], true),    
    {ok, {'testnode1@host1.com', hello}} = ecall:call_any(?NODES, ecall_SUITE, echo1, [hello], false), 

    {error,_} = ecall:call_any(?NODES -- ['testnode1@host1.com'], ecall_SUITE, echo1, [hello], false), 
    {error, _} = ecall:call_any(?NODES, ecall_SUITE, echo111, [hello], true), 
    ok.


call_all(_Config) ->
    {error, _} = ecall:call_all([], ecall_SUITE, echo, [hello], false),
    {error, _} = ecall:call_all([], ecall_SUITE, echo, [hello], true),

    {error, _} = ecall:call_all(?NODES -- ['testnode1@host1.com'], ecall_SUITE, echo1, [hello], false),
    {error, _} = ecall:call_all(?NODES -- ['testnode1@host1.com'], ecall_SUITE, echo1, [hello], true),

    {error, {_, {badrpc, _}}} = ecall:call_all(?NODES, ecall_SUITE, echo111, [hello], true),
    {error, _} = ecall:call_all(?NODES, ecall_SUITE, echo111, [hello], false),

    {ok, _} = ecall:call_all(?NODES, ecall_SUITE, echo, [hello], true),    
    {ok, _} = ecall:call_all(?NODES, ecall_SUITE, echo, [hello], false),    
    ok.

call_all_wait(_Config) ->
    {error, _} = ecall:call_all_wait([], ecall_SUITE, echo, [hello]),
    {Replies, Rejects} = ecall:call_all_wait(?NODES, ecall_SUITE, echo1, [hello]),
    Replies1 = lists:sort(Replies),
    Rejects1 = lists:sort(Rejects),

    {ExpectedReplies, ExpectedRejects} = lists:foldl(fun(N, {Ok, Err}) ->  
        if 
            N == 'testnode1@host1.com' -> 
                {[{N, hello} | Ok], Err};
            true -> 
                {Ok, [{N, call_from_another_node} | Err]}
        end
    end, {[], []}, ?NODES),

    ExpectedReplies1 =lists:sort(ExpectedReplies), ExpectedRejects1 = lists:sort(ExpectedRejects),
    ct:pal("Replies1 ~p ExpectedReplies ~p, Rejects1 ~p, ExpectedRejects1 ~p", [Replies1, ExpectedReplies1, Rejects1, ExpectedRejects1]),
    Replies1 = ExpectedReplies1,
    Rejects1 = ExpectedRejects1,

    ok.

cast_one(_Config) ->
    ok = ecall:cast_one([], ecall_SUITE, side_effect, [self()]),
    ok = ecall:cast_one(?NODES, ecall_SUITE, side_effect, [self()]),
    receive
        '@hello@' -> ct:pal("received")
    after 5000 ->
        ct:pal("timeout")
    end,
    ok.

cast_all(_Config) ->
    ok = ecall:cast_all([], ecall_SUITE, side_effect, [self()]), 
    ecall:cast_all(?NODES, ecall_SUITE, side_effect, [self()]), 
    [begin
        receive
            '@hello@' -> ct:pal("received")
        after 5000 ->
            ct:pal("timeout")
        end
    end||_Node<- ?NODES],
    ok.

%%=================================================================
%% Utilities
%%=================================================================
echo(Argument) ->
    Argument.

echo1(Argument) ->
    case node() of
        'testnode1@host1.com' -> Argument;
        _ -> {error, call_from_another_node}
    end.

side_effect(Pid) ->
    Pid ! '@hello@',
    ok.


sync_nodes(Msg, ProcRegName) ->
    Nodes = ?NODES -- [node()],
    [{ProcRegName, N} ! Msg|| N<-Nodes],
    do_sync_nodes(Msg, length(Nodes)).

do_sync_nodes(Msg, N) when N>0->
    receive
        Msg -> do_sync_nodes(Msg, N - 1)
    end;
do_sync_nodes(_Msg, 0) ->
    ok.