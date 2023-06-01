
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

handle_info({Ref, join, ?pg_group, Neighbours}, #state{ ref = Ref} = State)->

  [ try
      Node = node(N),
      ?LOGINFO("connecting to ~p",[ Node ]),
      ecall_connection:connect( Node )
    catch
      _:E -> ?LOGERROR("unable to connect to ~p, error ~p",[ node(N), E ])
    end|| N <- Neighbours, node(N) =/= node()],

  {noreply,State};

handle_info({Ref, leave, ?pg_group, LeftNeighbours}, #state{ ref = Ref} = State)->

  [ try
      Node = node(N),
      ?LOGWARNING("disconnect from ~p",[Node]),
      ecall_connection:disconnect( Node )
    catch
      _:E -> ?LOGERROR("unable to disconnect to ~p, error ~p",[ node(N), E ])
    end|| N <- LeftNeighbours, node(N) =/= node()],

  {noreply,State};

handle_info(Message,State)->
  ?LOGWARNING("unexpected info message ~p",[Message]),
  {noreply,State}.

terminate(_Reason,_State)->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.









