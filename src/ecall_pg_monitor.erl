
-module(ecall_pg_monitor).

-include("ecall.hrl").

-behaviour(gen_server).

%%=================================================================
%%	OTP API
%%=================================================================
-export([
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%=================================================================
%%	OTP
%%=================================================================
start_link()->
  gen_server:start_link({local,?MODULE},?MODULE, [], []).

-record(state,{ref}).
init([])->

  process_flag(trap_exit,true),

  {Ref, Neighbours} = pg:monitor( ?pg_scope, ?pg_group ),

  self() ! {Ref, join, ?pg_group, Neighbours},

  {ok,#state{ ref = Ref}}.

handle_call(Request, From, State) ->
  ?LOGWARNING("unexpected call resquest ~p from ~p",[Request,From]),
  {noreply,State}.

handle_cast(Request,State)->
  ?LOGWARNING("unexpected cast resquest ~p",[Request]),
  {noreply,State}.

% A joined member is a remote ecall_receive master: the connection master
% of its node acts on a join only while it waits for a pool.
handle_info({Ref, join, ?pg_group, Neighbours}, #state{ ref = Ref} = State)->

  [ try
      Node = node(N),
      ?LOGINFO("connecting to ~p",[ Node ]),
      case ecall_connection:connect( Node, N ) of
        ok -> ok;
        {error, Error} -> ?LOGERROR("unable to connect to ~p, error ~p",[ Node, Error ])
      end
    catch
      _:E -> ?LOGERROR("unable to connect to ~p, error ~p",[ node(N), E ])
    end|| N <- Neighbours, node(N) =/= node()],

  {noreply,State};

% leave is ignored: the incarnation process is linked to the remote receive
% master and covers every case a leave would report.
handle_info({Ref, leave, ?pg_group, _LeftNeighbours}, #state{ ref = Ref} = State)->
  {noreply,State};

handle_info(Message,State)->
  ?LOGWARNING("unexpected info message ~p",[Message]),
  {noreply,State}.

terminate(_Reason,_State)->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.









