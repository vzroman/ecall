
-module(ecall).

-include("ecall.hrl").

%%=================================================================
%% API
%%=================================================================
-export([
  send/2,

  call_one/4,call_one/5,
  call_any/4,call_any/5,
  call_all/4,call_all/5,
  call_all_wait/4,

  cast_one/4,
  cast_all/4
]).

-define(RAND(List),
  begin
    _@I = erlang:phash2(make_ref(),length(List)),
    lists:nth(_@I+1, List)
  end).

%===========================================================
%   SEND
%===========================================================
send(To, Message)->
  ecall_connection:send(To, Message).

%===========================================================
%   CALLS
%===========================================================
%-----------call one----------------------------------------
call_one(Ns,M,F,As) ->
  call_one(Ns,M,F,As,_RpcErr = false).
call_one([],_M,_F,_As,_RpcErr) ->
  {error,none_is_available};
call_one(Ns,M,F,As,RpcErr) ->
  N = node(),
  case lists:member(N, Ns) of
    true ->
      case ecall_connection:call(N, M, F, As) of
        {ok,Result}->
          ?LOGDEBUG("~p ok ~p",[N,Result]),
          {ok,{N,Result}};
        {error,Error}->
          ?LOGERROR("~p error ~p",[N,Error]),
          call_one( Ns --[N], M, F, As, [{N,Error}], RpcErr)
      end;
    false->
      call_one(Ns,M,F,As,[],RpcErr)
  end.

call_one([],_M,_F,_As,Errors,_RpcErr)->
  {error,Errors};
call_one( Ns,M,F,As,Errors,RpcErr)->
  N = ?RAND( Ns ),
  ?LOGDEBUG("~p from ~p with ~p:~p(~p), RpcErr ~p",[N,Ns,M,F,As,RpcErr]),
  case ecall_connection:call(N, M, F, As) of
    {ok,Result}->
      ?LOGDEBUG("~p ok ~p",[N,Result]),
      {ok,{N,Result}};
    {error, Error}->
      Errors1 =
        case Error of
          {badrpc, Reason} when not RpcErr ->
            ?LOGWARNING("~p badrpc ~p",[N,Reason]),
            Errors;
          _->
            ?LOGERROR("~p error ~p",[N,Error]),
            [{N,Error} | Errors]
        end,
      call_one( Ns --[N], M, F, As, Errors1, RpcErr)
  end.

%-----------call any----------------------------------------
call_any(Ns,M,F,As) ->
  call_any(Ns,M,F,As,_RpcErr = false).
call_any([],_M,_F,_As,_RpcErr)->
  {error,none_is_available};
call_any(Ns,M,F,As,RpcErr)->
  N = node(),
  case lists:member(N, Ns) of
    true ->
      case ecall_connection:call(N, M, F, As) of
        {ok,Result}->
          ?LOGDEBUG("~p ok ~p",[N,Result]),
          cast_all(Ns -- [N], M, F, As),
          {ok,{N,Result}};
        {error,Error}->
          ?LOGERROR("~p error ~p",[N,Error]),
          do_call_any( Ns --[N], M, F, As, [{N,Error}], RpcErr)
      end;
    false->
      do_call_any(Ns,M,F,As,[],RpcErr)
  end.

do_call_any(Ns,M,F,As,Errors,RpcErr)->
  ?LOGDEBUG("~p with ~p:~p(~p), RpcErr",[Ns,M,F,As,RpcErr]),
  Owner = self(),
  Master = spawn(fun()->async_call_any(Owner,Ns,M,F,As,Errors,RpcErr) end),
  receive
    {ok,Master,Result}->{ok,Result};
    {error,Master,Error}->{error,Error}
  end.

async_call_any(Owner,Ns,M,F,As,Errors,RpcErr)->
  Master = self(),
  Callers =
    [ spawn(fun()->Master ! {N,ecall_connection:call(N, M, F, As)} end) || N <- Ns ],
  wait_any(Owner, length(Callers), Errors, RpcErr).

wait_any(Owner, WaitFor, Errors, RpcErr) when WaitFor > 0 ->
  % I'm the master
  receive
    {N,{ok,Result}}->
      ?LOGDEBUG("~p: result ~p",[N,Result]),
      Owner ! {ok,self(),{N,Result}};
    {N,{error,Error}}->
      Errors1 =
        case Error of
          {badrpc, Reason} when not RpcErr->
            ?LOGWARNING("~p badrpc ~p",[N,Reason]),
            Errors;
          _->
            ?LOGERROR("~p error ~p",[N,Error]),
            [{N, Error} | Errors]
        end,
      wait_any(Owner, WaitFor-1,Errors1,RpcErr)
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
  Master = spawn(fun()->async_call_all(Owner,Ns,M,F,As,RpcErr) end),
  receive
    {ok,Master,Result}->{ok,Result};
    {error,Master,Error}->{error,Error}
  end.
async_call_all(Owner,Ns,M,F,As,RpcErr)->
  Master = self(),
  Callers =
    [ spawn(fun()->Master ! {N,ecall_connection:call(N, M, F, As)} end) || N <- Ns ],
  wait_all(Owner, length(Callers), _OKs=[], RpcErr).

wait_all(Owner, WaitFor, OKs, RpcErr) when WaitFor > 0 ->
  % I'm the master
  receive
    {N,{ok,Result}}->
      ?LOGDEBUG("~p result: ~p",[N,Result]),
      wait_all(Owner, WaitFor-1,[{N,Result}|OKs],RpcErr);
    {N,{error,Error}}->
      case Error of
        {badrpc, Reason} when not RpcErr ->
          ?LOGWARNING("~p badrpc ~p",[N,Reason]),
          wait_all(Owner,WaitFor-1,OKs,RpcErr);
        _->
          ?LOGERROR("~p error ~p",[N,Error]),
          Owner ! {error,self(), {N,Error}}
      end
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
  {[],[]};
call_all_wait(Ns,M,F,As)->
  ?LOGDEBUG("~p with ~p:~p(~p)",[Ns,M,F,As]),
  Owner = self(),
  Master = spawn(fun()->async_call_all_wait(Owner,Ns,M,F,As) end),
  receive
    {Master,OKs, Errors}->{ OKs, Errors }
  end.
async_call_all_wait(Owner,Ns,M,F,As)->
  Master = self(),
  Callers =
    [ spawn_link(fun()->Master ! {N,ecall_connection:call(N, M, F, As)} end) || N <- Ns ],
  wait_all_wait(Owner, length(Callers), _Replies=[],_Rejects=[]).

wait_all_wait(Owner, WaitFor, Replies, Rejects) when WaitFor > 0 ->
  % I'm the master
  receive
    {N,{ok,Result}}->
      ?LOGDEBUG("~p result ~p",[N,Result]),
      wait_all_wait(Owner, WaitFor-1,[{N,Result}|Replies],Rejects);
    {N,{error,E}}->
      ?LOGERROR("~p error ~p",[N,E]),
      wait_all_wait(Owner, WaitFor-1,Replies,[{N,E}|Rejects])
  end;
wait_all_wait(Owner, _WaitFor=0, Replies, Rejects)->
  Owner ! {self(), lists:reverse(Replies),lists:reverse(Rejects)}.

%-------------CAST------------------------------------------
cast_one(Ns,M,F,As)->
  N =
    case lists:member(node(), Ns) of
      true -> node();
      _-> ?RAND(Ns)
    end,
  ?LOGDEBUG("~p from ~p with ~p:~p(~p)",[N,Ns,M,F,As]),
  ecall_connection:cast(N, M, F, As),
  ok.

cast_all(Ns,M,F,As)->
  ?LOGDEBUG("~p with ~p:~p(~p)",[Ns,M,F,As]),
  [ ecall_connection:cast(N, M, F, As) || N <- Ns ],
  ok.


