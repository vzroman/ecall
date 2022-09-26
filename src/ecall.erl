
-module(ecall).

%%=================================================================
%% API
%%=================================================================
-export([
  call_one/4,call_one/5,
  call_any/4,call_any/5,
  call_all/4,call_all/5,
  call_all_wait/4,

  cast_one/4,
  cast_all/4
]).

-define(T,atom_to_list(?FUNCTION_NAME)++": ").
-define(LOGDEBUG(Text,Params),lager:debug(?T++Text,Params)).
-define(LOGINFO(Text,Params),lager:info(?T++Text,Params)).
-define(LOGWARNING(Text,Params),lager:warning(?T++Text,Params)).
-define(LOGERROR(Text,Params),lager:error(?T++Text,Params)).

-define(RAND(List),
  begin
    _@I = erlang:phash2(make_ref(),length(List)),
    lists:nth(_@I+1, List)
  end).

%===========================================================
%   CALLS
%===========================================================
%-----------call one----------------------------------------
call_one(Ns,M,F,As) ->
  call_one(Ns,M,F,As,_RpcErr = false).
call_one([],_M,_F,_As,_RpcErr) ->
  {error,none_is_available};
call_one(Ns,M,F,As,RpcErr) ->
  call_one(Ns,M,F,As,[],RpcErr).

call_one([],_M,_F,_As,Errors,_RpcErr)->
  {error,Errors};
call_one( Ns,M,F,As,Errors,RpcErr)->
  N = ?RAND( Ns ),
  ?LOGDEBUG("~p from ~p with ~p:~p(~p), RpcErr ~p",[N,Ns,M,F,As,RpcErr]),
  case rpc:call(N, M, F, As) of
    {ok,Result} ->
      ?LOGDEBUG("~p ok ~p",[N,Result]),
      {ok,{N,Result}};
    {error,Error}->
      ?LOGERROR("~p error ~p",[N,Error]),
      call_one( Ns --[N], M, F, As, [{N,Error}|Errors]);
    {badrpc, Reason} when RpcErr ->
      ?LOGERROR("~p badrpc error ~p",[N,Reason]),
      call_one( Ns --[N], M, F, As, [{N,{badrpc, Reason}}|Errors]);
    {badrpc, Reason} ->
      ?LOGWARNING("~p badrpc ~p",[N,Reason]),
      call_one( Ns --[N], M, F, As, Errors)
  end.

%-----------call any----------------------------------------
call_any(Ns,M,F,As) ->
  call_any(Ns,M,F,As,_RpcErr = false).
call_any([],_M,_F,_As,_RpcErr)->
  {error,none_is_available};
call_any(Ns,M,F,As,RpcErr)->
  ?LOGDEBUG("~p with ~p:~p(~p), RpcErr",[Ns,M,F,As,RpcErr]),
  Owner = self(),
  Master = spawn_link(fun()->async_call_any(Owner,Ns,M,F,As,RpcErr) end),
  receive
    {ok,Master,Result}->{ok,Result};
    {error,Master,Error}->{error,Error}
  end.

async_call_any(Owner,Ns,M,F,As,RpcErr)->
  Master = self(),
  Callers =
    [ spawn_link(fun()->Master ! {N,rpc:call(N, M, F, As)} end) || N <- Ns ],
  wait_any(Owner, length(Callers), _Errors=[], RpcErr).

wait_any(Owner, WaitFor, Errors, RpcErr) when WaitFor > 0 ->
  % I'm the master
  receive
    {N,{ok,Result}}->
      ?LOGDEBUG("~p ok ~p",[N,Result]),
      Owner ! {ok,self(),{N,Result}};
    {N,{error,E}}->
      ?LOGERROR("~p error ~p",[N,E]),
      wait_any(Owner, WaitFor-1,[{N,E}],RpcErr);
    {N,{badrpc, Reason}} when RpcErr->
      ?LOGERROR("~p badrpc error ~p",[N,Reason]),
      wait_any(Owner, WaitFor-1,[{N,{badrpc, Reason}}],RpcErr);
    {N,{badrpc, Reason}}->
      ?LOGWARNING("~p badrpc ~p",[N,Reason]),
      wait_any(Owner,WaitFor-1,Errors,RpcErr)
  end;
wait_any(Owner, _WaitFor=0, Errors,_RpcErr)->
  if
    length(Errors) >0 ->
      Owner ! {error,self(), Errors};
    true->
      % All were badrpc
      Owner ! {error,self(),none_is_available}
  end.

%-----------call all----------------------------------------
call_all(Ns,M,F,As) ->
  call_all(Ns,M,F,As,_RpcErr = false).
call_all([],_M,_F,_As,_RpcErr)->
  {error,none_is_available};
call_all(Ns,M,F,As,RpcErr)->
  ?LOGDEBUG("~p with ~p:~p(~p), RpcErr ~p",[Ns,M,F,As,RpcErr]),
  Owner = self(),
  Master = spawn_link(fun()->async_call_all(Owner,Ns,M,F,As,RpcErr) end),
  receive
    {ok,Master,Result}->{ok,Result};
    {error,Master,Error}->{error,Error}
  end.
async_call_all(Owner,Ns,M,F,As,RpcErr)->
  Master = self(),
  Callers =
    [ spawn_link(fun()->Master ! {N,rpc:call(N, M, F, As)} end) || N <- Ns ],
  wait_all(Owner, length(Callers), _OKs=[], RpcErr).

wait_all(Owner, WaitFor, OKs, RpcErr) when WaitFor > 0 ->
  % I'm the master
  receive
    {N,{ok,Result}}->
      ?LOGDEBUG("~p ok ~p",[N,Result]),
      wait_all(Owner, WaitFor-1,[{N,Result}|OKs],RpcErr);
    {N,{error,E}}->
      ?LOGERROR("~p error ~p",[N,E]),
      Owner ! {error,self(), {N,E}};
    {N,{badrpc, Reason}} when RpcErr->
      ?LOGERROR("~p badrpc as error ~p",[N,Reason]),
      Owner ! {error,self(), {N,{badrpc, Reason}}};
    {N,{badrpc, Reason}} ->
      ?LOGWARNING("~p badrpc ~p",[N,Reason]),
      wait_all(Owner,WaitFor-1,OKs,RpcErr)
  end;
wait_all(Owner, _WaitFor=0, OKs,_RpcErr)->
  if
    length(OKs) >0 ->
      Owner ! {ok,self(), lists:reverse(OKs)};
    true->
      % All were badrpc
      Owner ! {error,self(),none_is_available}
  end.

%-----------call all wait----------------------------------------
call_all_wait([],_M,_F,_As)->
  {error,none_is_available};
call_all_wait(Ns,M,F,As)->
  ?LOGDEBUG("~p with ~p:~p(~p)",[Ns,M,F,As]),
  Owner = self(),
  Master = spawn_link(fun()->async_call_all_wait(Owner,Ns,M,F,As) end),
  receive
    {Master,OKs, Errors}->{ OKs, Errors }
  end.
async_call_all_wait(Owner,Ns,M,F,As)->
  Master = self(),
  Callers =
    [ spawn_link(fun()->Master ! {N,rpc:call(N, M, F, As)} end) || N <- Ns ],
  wait_all_wait(Owner, length(Callers), _Replies=[],_Rejects=[]).

wait_all_wait(Owner, WaitFor, Replies, Rejects) when WaitFor > 0 ->
  % I'm the master
  receive
    {N,{ok,Result}}->
      ?LOGDEBUG("~p ok ~p",[N,Result]),
      wait_all_wait(Owner, WaitFor-1,[{N,Result}|Replies],Rejects);
    {N,{error,E}}->
      ?LOGERROR("~p error ~p",[N,E]),
      wait_all_wait(Owner, WaitFor-1,Replies,[{N,E}|Rejects]);
    {N,{badrpc, Reason}}->
      ?LOGERROR("~p badrpc as error ~p",[N,Reason]),
      wait_all_wait(Owner, WaitFor-1,Replies,[{N,{badrpc,Reason}}|Rejects])
  end;
wait_all_wait(Owner, _WaitFor=0, Replies, Rejects)->
  Owner ! {self(), lists:reverse(Replies),lists:reverse(Rejects)}.

%-------------CAST------------------------------------------
cast_one(Ns,M,F,As)->
  N = ?RAND(Ns),
  ?LOGDEBUG("~p from ~p with ~p:~p(~p)",[N,Ns,M,F,As]),
  rpc:cast(N, M, F, As),
  ok.

cast_all(Ns,M,F,As)->
  ?LOGDEBUG("~p with ~p:~p(~p)",[Ns,M,F,As]),
  [ rpc:cast(N, M, F, As) || N <- Ns ],
  ok.


