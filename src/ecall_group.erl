-module(ecall_group).
-moduledoc false.

%% Group operation implementation. Application callers use the ecall facade.

%%=================================================================
%% INTERNAL GROUP API
%%=================================================================
-export([
  call_one/4, call_one/5,
  call_any/4, call_any/5,
  call_all/4, call_all/5,
  call_all_wait/4,
  cast_one/4,
  cast_all/4
]).

-type pending_calls() :: #{reference() => node()}.
-type call_mfa() :: {module(), atom(), [term()]}.
-type node_result() :: {node(), term()}.
-type single_result() ::
  {ok, node_result()} | {error, none_is_available | [node_result()]}.

-define(RAND(List),
  begin
    _@I = erlang:phash2(make_ref(),length(List)),
    lists:nth(_@I+1, List)
  end).

%===========================================================
%   CALLS
%===========================================================
%-----------call one----------------------------------------
-spec call_one([node()], module(), atom(), [term()]) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_one(Ns,M,F,As) ->
  call_one(Ns,M,F,As,_RpcErr = false).

-spec call_one([node()], module(), atom(), [term()], boolean()) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_one([],_M,_F,_As,_RpcErr) ->
  {error,none_is_available};
call_one(Ns,M,F,As,RpcErr) ->
  N = node(),
  case lists:member(N, Ns) of
    true ->
      case ecall_connection:call(N, M, F, As) of
        {ok,Result}->
          {ok,{N,Result}};
        {error,Error}->
          call_one( Ns --[N], M, F, As, [{N,Error}], RpcErr)
      end;
    false->
      call_one(Ns,M,F,As,[],RpcErr)
  end.

-spec call_one([node()], module(), atom(), [term()], [node_result()], boolean()) ->
  {ok, node_result()} | {error, [node_result()]}.
call_one([],_M,_F,_As,Errors,_RpcErr)->
  {error,Errors};
call_one( Ns,M,F,As,Errors,RpcErr)->
  N = ?RAND( Ns ),
  case ecall_connection:call(N, M, F, As) of
    {ok,Result}->
      {ok,{N,Result}};
    {error, Error}->
      Errors1 =
        case Error of
          {badrpc, _Reason} when not RpcErr ->
            Errors;
          _->
            [{N,Error} | Errors]
        end,
      call_one( Ns --[N], M, F, As, Errors1, RpcErr)
  end.

%-----------call any----------------------------------------
-spec call_any([node()], module(), atom(), [term()]) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_any(Ns,M,F,As) ->
  call_any(Ns,M,F,As,_RpcErr = false).

-spec call_any([node()], module(), atom(), [term()], boolean()) ->
  {ok, {node(), term()}} | {error, none_is_available | [{node(), term()}]}.
call_any([],_M,_F,_As,_RpcErr)->
  {error,none_is_available};
call_any(Ns,M,F,As,RpcErr)->
  N = node(),
  case lists:member(N, Ns) of
    true ->
      case ecall_connection:call(N, M, F, As) of
        {ok,Result}->
          cast_all(Ns -- [N], M, F, As),
          {ok,{N,Result}};
        {error,Error}->
          do_call_any(Ns -- [N], {M,F,As}, [{N,Error}], RpcErr,
            [Ns,M,F,As,RpcErr])
      end;
    false->
      do_call_any(Ns, {M,F,As}, [], RpcErr, [Ns,M,F,As,RpcErr])
  end.

-spec do_call_any([node()], call_mfa(), [node_result()], boolean(), [term()]) ->
  single_result().
do_call_any(Ns, MFA, Errors, RpcErr, Args)->
  group_call(call_any, Args, fun()->
    wait_any(spawn_callers(Ns, MFA), Errors, RpcErr)
  end).

-spec wait_any(pending_calls(), [node_result()], boolean()) -> single_result().
wait_any(Pending, Errors, RpcErr) when map_size(Pending) > 0 ->
  receive
    {'DOWN', Ref, process, _Caller, Reason}->
      {N, Pending1} = maps:take(Ref, Pending),
      case caller_result(Reason) of
        {ok, Result}->
          {ok, {N, Result}};
        {error, {badrpc, _Reason}} when not RpcErr->
          wait_any(Pending1, Errors, RpcErr);
        {error, Error}->
          wait_any(Pending1, [{N, Error} | Errors], RpcErr)
      end
  end;
wait_any(_Pending, [], _RpcErr)->
  {error, none_is_available};
wait_any(_Pending, Errors, _RpcErr)->
  {error, Errors}.

%-----------call all----------------------------------------
-spec call_all([node()], module(), atom(), [term()]) ->
  {ok, [{node(), term()}]} | {error, none_is_available | {node(), term()}}.
call_all(Ns,M,F,As) ->
  call_all(Ns,M,F,As,_RpcErr = false).

-spec call_all([node()], module(), atom(), [term()], boolean()) ->
  {ok, [{node(), term()}]} | {error, none_is_available | {node(), term()}}.
call_all([],_M,_F,_As,_RpcErr)->
  {error,none_is_available};
call_all(Ns,M,F,As,RpcErr)->
  group_call(call_all, [Ns,M,F,As,RpcErr], fun()->
    wait_all(spawn_callers(Ns, {M,F,As}), [], RpcErr)
  end).

-spec wait_all(pending_calls(), [node_result()], boolean()) ->
  {ok, [node_result()]} | {error, none_is_available | node_result()}.
wait_all(Pending, OKs, RpcErr) when map_size(Pending) > 0 ->
  receive
    {'DOWN', Ref, process, _Caller, Reason}->
      {N, Pending1} = maps:take(Ref, Pending),
      case caller_result(Reason) of
        {ok, Result}->
          wait_all(Pending1, [{N, Result} | OKs], RpcErr);
        {error, {badrpc, _Reason}} when not RpcErr->
          wait_all(Pending1, OKs, RpcErr);
        {error, Error}->
          {error, {N, Error}}
      end
  end;
wait_all(_Pending, [], _RpcErr)->
  {error, none_is_available};
wait_all(_Pending, OKs, _RpcErr)->
  {ok, lists:reverse(OKs)}.

%-----------call all wait----------------------------------------
-spec call_all_wait([node()], module(), atom(), [term()]) ->
  {[{node(), term()}], [{node(), term()}]}.
call_all_wait([],_M,_F,_As)->
  {[],[]};
call_all_wait(Ns,M,F,As)->
  group_call(call_all_wait, [Ns,M,F,As], fun()->
    wait_all_wait(spawn_callers(Ns, {M,F,As}), [], [])
  end).

-spec wait_all_wait(pending_calls(), [node_result()], [node_result()]) ->
  {[node_result()], [node_result()]}.
wait_all_wait(Pending, Replies, Rejects) when map_size(Pending) > 0 ->
  receive
    {'DOWN', Ref, process, _Caller, Reason}->
      {N, Pending1} = maps:take(Ref, Pending),
      case caller_result(Reason) of
        {ok, Result}->
          wait_all_wait(Pending1, [{N, Result} | Replies], Rejects);
        {error, Error}->
          wait_all_wait(Pending1, Replies, [{N, Error} | Rejects])
      end
  end;
wait_all_wait(_Pending, Replies, Rejects)->
  {lists:reverse(Replies), lists:reverse(Rejects)}.

%-----------group call protocol----------------------------------
-spec group_call(atom(), [term()], fun(() -> Result)) -> Result
  when Result :: term().
group_call(Function, Args, Call)->
  {Master, MRef} = spawn_monitor(fun()->
    Reply = Call(),
    % Keep outside try/catch: catching this exit would swallow the result.
    exit({ecall_result, Reply})
  end),
  receive
    {'DOWN', MRef, process, Master, {ecall_result, Reply}}->
      Reply;
    {'DOWN', MRef, process, Master, Reason}->
      exit({Reason, {ecall, Function, Args}})
  end.

-spec spawn_callers([node()], call_mfa()) -> pending_calls().
spawn_callers(Ns, {M,F,As})->
  maps:from_list([
    begin
      {_Caller, Ref} = spawn_monitor(fun()->
        Result = ecall_connection:call(N, M, F, As),
        % Keep outside try/catch: catching this exit would swallow the result.
        exit({ecall_result, Result})
      end),
      {Ref, N}
    end || N <- Ns
  ]).

-spec caller_result(term()) -> {ok, term()} | {error, term()}.
caller_result({ecall_result, {ok, _Value} = Result})->
  Result;
caller_result({ecall_result, {error, _Error} = Result})->
  Result;
caller_result(Reason)->
  {error, {badrpc, Reason}}.

%-------------CAST------------------------------------------
-spec cast_one(nonempty_list(node()), module(), atom(), [term()]) -> ok.
cast_one(Ns,M,F,As)->
  N =
    case lists:member(node(), Ns) of
      true -> node();
      _-> ?RAND(Ns)
    end,
  ecall_connection:cast(N, M, F, As),
  ok.

-spec cast_all([node()], module(), atom(), [term()]) -> ok.
cast_all(Ns,M,F,As)->
  [ ecall_connection:cast(N, M, F, As) || N <- Ns ],
  ok.
