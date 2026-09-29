
-module(ecall_receive).

-include("ecall.hrl").

%%=================================================================
%% API
%%=================================================================
-export([
  pool_size/0,
  get_pool/0
]).

%%=================================================================
%% OTP API
%%=================================================================
-export([
  start_link/1
]).

-export_type([pool_size/0, request/0]).

-type pool_size() :: pos_integer() | disabled.
-type request() ::
  {send, erlang:send_destination(), term()}
  | {cast, module(), atom(), [term()]}
  | {call, reference(), pid(), module(), atom(), [term()]}.

-record(state,{}).
% request of get_pool/0 to the receive master
-record(get_pool,{
  from :: pid()
}).
% reply of the receive master to get_pool/0
-record(reply_pool,{
  workers :: nonempty_list(pid())
}).

%%=================================================================
%% API
%%=================================================================
% The single validator of pool_size, called once by ecall_app at start.
-spec pool_size() -> pool_size() | {error, {invalid_pool_size, term()}}.
pool_size()->
  case application:get_env(ecall, pool_size) of
    undefined->
      default_pool_size();
    {ok, undefined}->
      default_pool_size();
    {ok, PoolSize} when is_integer( PoolSize ), PoolSize > 0->
      PoolSize;
    {ok, disabled}->
      disabled;
    {ok, Invalid}->
      {error, {invalid_pool_size, Invalid}}
  end.

% Called on this node by a remote connection master through erpc. Returns the
% receive master pid, which the remote incarnation process links to, and the
% workers. not_active: there is no receive pool on this node (pool_size
% disabled or ecall not started).
-spec get_pool() -> {ok, {pid(), nonempty_list(pid())}} | {error, term()}.
get_pool()->
  case whereis(?MODULE) of
    Master when is_pid(Master) ->
      Monitor = erlang:monitor(process, Master),
      Master ! #get_pool{from = self()},
      receive
        #reply_pool{workers = Workers} ->
          {ok, {Master, Workers}};
        {'DOWN', Monitor, process, _, Reason}->
          {error, Reason}
      end;
    _->
      {error, not_active}
  end.

-spec default_pool_size() -> pos_integer().
default_pool_size()->
  case erlang:system_info( logical_processors ) of
    unknown-> erlang:system_info( schedulers );
    PoolSize-> PoolSize
  end.

%%=================================================================
%% OTP API
%%=================================================================
-spec start_link(pos_integer()) ->
  {ok, pid()} | {error, {already_started, pid()}}.
start_link( PoolSize )->
  case whereis( ?MODULE ) of
    PID when is_pid( PID )->
      {error, {already_started, PID}};
    _->
      Parent = self(),
      {ok, spawn_link(fun()-> init_pool( Parent, PoolSize ) end)}
  end.

-spec init_pool(pid(), pos_integer()) -> no_return().
init_pool( Parent, PoolSize )->

  % Remote connections link their incarnation processes to this master,
  % so their exits (killed, noconnection, normal) must not take the pool down.
  process_flag( trap_exit, true ),

  register( ?MODULE, self() ),

  Workers =
    [ spawn_opt(fun()-> worker_loop(#state{}) end,
                [link, {message_queue_data, off_heap}])
      || _ <- lists:seq(1, PoolSize)],

  pg:join(?pg_scope, ?pg_group, self() ),

  master_loop( Parent, Workers ).

-spec master_loop(pid(), nonempty_list(pid())) -> no_return().
master_loop( Parent, Workers )->
  receive
    #get_pool{from = From}->
      catch From ! #reply_pool{workers = Workers},
      master_loop( Parent, Workers );
    {'EXIT', Parent, Reason}->
      exit( Reason );
    {'EXIT', Worker, Reason}->
      case lists:member( Worker, Workers ) of
        true ->
          exit( {worker_down, Worker, Reason} );
        false ->
          % a peer's incarnation process: killed, noconnection, normal
          master_loop( Parent, Workers )
      end;
    _->
      master_loop( Parent, Workers )
  end.


-spec worker_loop(#state{}) -> no_return().
worker_loop( State )->
  receive
    {batch, Batch}->
      State1 = handle_batch(Batch, State),
      erlang:garbage_collect(self()),
      worker_loop( State1 );
    {{'DOWN', Ref, ClientPID}, _MonRef, process, _PID, Reason}->
      if
        Reason =/= normal->
          catch ecall_connection:call_reply(ClientPID, {'DOWN', Ref, Reason});
        true ->
          ignore
      end,
      worker_loop( State );
    _Unexpected->
      worker_loop( State )
  end.

%%=================================================================
%% REMOTE API
%%=================================================================
-spec handle_batch([request()], #state{}) -> #state{}.
handle_batch([{send, To, Message}| Rest], State)->
  catch To ! Message,
  handle_batch( Rest, State );
handle_batch([{cast, Module, Function, Args}| Rest], State)->
  spawn(Module, Function, Args ),
  handle_batch( Rest, State);
handle_batch([{call, Ref, ClientPID,  Module, Function, Args}| Rest], State)->
  spawn_opt(
    fun()->
      Reply =
        try apply(Module, Function, Args) of
          Result -> {Ref, Result}
        catch
          _:Reason -> {'DOWN', Ref, Reason}
        end,
      ecall_connection:call_reply( ClientPID, Reply )
    end,
    [{monitor, [{tag, {'DOWN', Ref, ClientPID}}]}]),
  handle_batch( Rest, State);
handle_batch([], State)->
  State.
