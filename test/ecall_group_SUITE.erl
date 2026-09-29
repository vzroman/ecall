-module(ecall_group_SUITE).

%% Distributed group-call regression tests. Keep result, fault-injection and
%% process inspection helpers together: they exercise the same process tree.
-include_lib("common_test/include/ct.hrl").

%%=================================================================
%% COMMON TEST API
%%=================================================================
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

%%=================================================================
%% TEST CASES
%%=================================================================
-export([
  any_success/1,
  any_rejections/1,
  any_unreachable/1,
  any_mixed_results/1,
  all_success/1,
  all_rejection/1,
  all_unreachable/1,
  all_arrival_order/1,
  wait_mixed_results/1,
  wait_arrival_order/1,
  empty_groups/1,
  duplicate_nodes/1,
  result_fidelity/1,
  master_killed/1,
  master_killed_after_local_rejection/1,
  caller_killed_all/1,
  caller_killed_wait/1,
  caller_killed_any/1,
  no_links/1,
  early_return_keeps_callers/1,
  owner_hygiene/1,
  unrelated_owner_messages/1,
  owner_killed/1,
  no_error_reports/1
]).

%%=================================================================
%% REMOTE TEST API
%%=================================================================
-export([
  echo/1, reject/1, crash/0, sleep_echo/2, notify_echo/2, block/1,
  node_action/1
]).

%%=================================================================
%% LOGGER HANDLER API
%%=================================================================
-export([adding_handler/1, removing_handler/1, log/2]).

-record(flight, {owner, ref, master, callers, blocked, args}).

-spec suite() -> list().
suite() ->
  [{timetrap, {seconds, 30}}].

-spec all() -> [atom()].
all() ->
  [any_success, any_rejections, any_unreachable, any_mixed_results,
   all_success, all_rejection, all_unreachable, all_arrival_order,
   wait_mixed_results, wait_arrival_order, empty_groups, duplicate_nodes,
   result_fidelity, master_killed, master_killed_after_local_rejection,
   caller_killed_all, caller_killed_wait,
   caller_killed_any, no_links, early_return_keeps_callers, owner_hygiene,
   unrelated_owner_messages, owner_killed, no_error_reports].

-spec init_per_suite(list()) -> list().
init_per_suite(Config) ->
  true = erlang:is_alive(),
  process_flag(trap_exit, true),
  {ok, _} = application:ensure_all_started(ecall),
  {Peer1, Node1} = start_peer(ecall_group_peer_one),
  unlink(Peer1),
  try
    {Peer2, Node2} = start_peer(ecall_group_peer_two),
    unlink(Peer2),
    try
      wait_both_connected(Peer1, Node1),
      wait_both_connected(Peer2, Node2),
      %% This bounded internal name has the same host as the test node.
      [_Name, Host] = string:split(atom_to_list(node()), "@"),
      Missing = list_to_atom("ecall_group_missing@" ++ Host),
      [{peers, [Peer1, Peer2]}, {nodes, [Node1, Node2]},
       {missing, Missing} | Config]
    catch Class2:Reason2:Stack2 ->
      link(Peer2),
      stop_peer(Peer2),
      erlang:raise(Class2, Reason2, Stack2)
    end
  catch Class1:Reason1:Stack1 ->
    link(Peer1),
    stop_peer(Peer1),
    erlang:raise(Class1, Reason1, Stack1)
  end.

-spec end_per_suite(list()) -> ok.
end_per_suite(Config) ->
  process_flag(trap_exit, true),
  lists:foreach(fun(Peer) -> link(Peer), stop_peer(Peer) end,
                ?config(peers, Config)),
  ok = application:stop(ecall).

%%=================================================================
%% RESULT SHAPES
%%=================================================================
-spec any_success(list()) -> ok.
any_success(Config) ->
  lists:foreach(
    fun(Nodes) ->
      {ok, {Winner, value}} =
        ecall:call_any(Nodes, ?MODULE, notify_echo, [self(), value]),
      true = lists:member(Winner, Nodes),
      case lists:member(node(), Nodes) of
        true -> Winner = node();
        false -> ok
      end,
      assert_same(Nodes, [await_called() || _ <- Nodes])
    end, node_lists(Config)).

-spec any_rejections(list()) -> ok.
any_rejections(Config) ->
  Local = node(),
  {error, [{Local, rejected}]} =
    ecall:call_any([Local], ?MODULE, reject, [rejected]),
  lists:foreach(
    fun(Nodes) ->
      {error, Errors} = ecall:call_any(Nodes, ?MODULE, reject, [rejected]),
      assert_same([{N, rejected} || N <- Nodes], Errors)
    end, node_lists(Config)).

-spec any_unreachable(list()) -> ok.
any_unreachable(Config) ->
  Missing = ?config(missing, Config),
  {error, none_is_available} = ecall:call_any([Missing], ?MODULE, echo, [ok]),
  {error, [{Missing, {badrpc, noconnection}}]} =
    ecall:call_any([Missing], ?MODULE, echo, [ok], true),
  %% A rejected local-first attempt remains an ordinary error.
  {error, [{Local, rejected}]} =
    ecall:call_any([node(), Missing], ?MODULE, reject, [rejected]),
  Local = node(),
  ok.

-spec any_mixed_results(list()) -> ok.
any_mixed_results(Config) ->
  [First, Second] = ?config(nodes, Config),
  lists:foreach(
    fun(Nodes) ->
      Actions = #{node() => {reject, [local_rejected]},
                  First => {crash, []}, Second => {echo, [success]}},
      {ok, {Second, success}} =
        ecall:call_any(Nodes, ?MODULE, node_action, [Actions]),
      Failures = Actions#{Second => {reject, [rejected]}},
      {error, Errors} =
        ecall:call_any(Nodes, ?MODULE, node_action, [Failures]),
      {First, {exit, _Reason}} = lists:keyfind(First, 1, Errors),
      {Second, rejected} = lists:keyfind(Second, 1, Errors),
      assert_same(Nodes, [N || {N, _Error} <- Errors])
    end, node_lists(Config)).

-spec all_success(list()) -> ok.
all_success(Config) ->
  lists:foreach(
    fun(Nodes) ->
      {ok, Results} = ecall:call_all(Nodes, ?MODULE, echo, [value]),
      assert_same([{N, value} || N <- Nodes], Results)
    end, node_lists(Config)).

-spec all_rejection(list()) -> ok.
all_rejection(Config) ->
  [Rejecting | _Rest] = ?config(nodes, Config),
  lists:foreach(
    fun(Nodes) ->
      Actions = maps:from_list([{N, {echo, [value]}} || N <- Nodes]),
      {error, {Rejecting, rejected}} = ecall:call_all(
        Nodes, ?MODULE, node_action,
        [Actions#{Rejecting => {reject, [rejected]}}]),
      {error, {Rejecting, {exit, _Reason}}} = ecall:call_all(
        Nodes, ?MODULE, node_action, [Actions#{Rejecting => {crash, []}}])
    end, node_lists(Config)).

-spec all_unreachable(list()) -> ok.
all_unreachable(Config) ->
  Missing = ?config(missing, Config),
  lists:foreach(
    fun(Nodes) ->
      {ok, Results} = ecall:call_all([Missing | Nodes], ?MODULE, echo, [ok]),
      assert_same([{N, ok} || N <- Nodes], Results),
      {error, {Missing, {badrpc, noconnection}}} =
        ecall:call_all([Missing | Nodes], ?MODULE, echo, [ok], true)
    end, node_lists(Config)),
  {error, none_is_available} = ecall:call_all([Missing], ?MODULE, echo, [ok]),
  ok.

-spec all_arrival_order(list()) -> ok.
all_arrival_order(Config) ->
  [Slow, Fast] = Nodes = ?config(nodes, Config),
  Actions = #{Slow => {sleep_echo, [300, slow]},
              Fast => {sleep_echo, [0, fast]}},
  {ok, [{Fast, fast}, {Slow, slow}]} =
    ecall:call_all(Nodes, ?MODULE, node_action, [Actions]),
  ok.

-spec wait_mixed_results(list()) -> ok.
wait_mixed_results(Config) ->
  [First, Second] = ?config(nodes, Config),
  Missing = ?config(missing, Config),
  lists:foreach(
    fun(Nodes) ->
      Actions = #{node() => {echo, [local_value]},
                  First => {echo, [value]}, Second => {reject, [rejected]}},
      {Replies, Rejects} = ecall:call_all_wait(
        [Missing | Nodes], ?MODULE, node_action, [Actions]),
      assert_same([{Second, rejected}, {Missing, {badrpc, noconnection}}],
                  Rejects),
      assert_same(lists:delete(Second, Nodes), [N || {N, _Value} <- Replies]),
      Crashes = Actions#{First => {crash, []}},
      {CrashReplies, CrashRejects} = ecall:call_all_wait(
        [Missing | Nodes], ?MODULE, node_action, [Crashes]),
      {First, {exit, _Reason}} = lists:keyfind(First, 1, CrashRejects),
      assert_same([Missing, First, Second], [N || {N, _} <- CrashRejects]),
      assert_same(Nodes -- [First, Second], [N || {N, _} <- CrashReplies])
    end, node_lists(Config)).

-spec wait_arrival_order(list()) -> ok.
wait_arrival_order(Config) ->
  [Slow, Fast] = Nodes = ?config(nodes, Config),
  lists:foreach(
    fun(Value) ->
      Actions = #{Slow => {sleep_echo, [300, Value]},
                  Fast => {sleep_echo, [0, Value]}},
      Result = ecall:call_all_wait(Nodes, ?MODULE, node_action, [Actions]),
      case Value of
        {error, rejected} -> {[], [{Fast, rejected}, {Slow, rejected}]} = Result;
        value -> {[{Fast, value}, {Slow, value}], []} = Result
      end
    end, [value, {error, rejected}]).

-spec empty_groups(list()) -> ok.
empty_groups(_Config) ->
  {error, none_is_available} = ecall:call_any([], ?MODULE, echo, [ok]),
  {error, none_is_available} = ecall:call_any([], ?MODULE, echo, [ok], true),
  {error, none_is_available} = ecall:call_all([], ?MODULE, echo, [ok]),
  {error, none_is_available} = ecall:call_all([], ?MODULE, echo, [ok], true),
  {[], []} = ecall:call_all_wait([], ?MODULE, echo, [ok]),
  ok.

-spec duplicate_nodes(list()) -> ok.
duplicate_nodes(Config) ->
  lists:foreach(
    fun(Base) ->
      Nodes = Base ++ Base,
      {ok, Results} = ecall:call_all(Nodes, ?MODULE, echo, [value]),
      assert_same([{N, value} || N <- Nodes], Results),
      {Replies, []} = ecall:call_all_wait(Nodes, ?MODULE, echo, [value]),
      assert_same(Results, Replies),
      {error, Errors} = ecall:call_any(Nodes, ?MODULE, reject, [rejected]),
      assert_same([{N, rejected} || N <- Nodes], Errors)
    end, node_lists(Config)).

-spec result_fidelity(list()) -> ok.
result_fidelity(Config) ->
  lists:foreach(
    fun(Value) ->
      lists:foreach(
        fun(Nodes) ->
          {ok, {Winner, Value}} = ecall:call_any(Nodes, ?MODULE, echo, [Value]),
          true = lists:member(Winner, Nodes),
          {ok, Results} = ecall:call_all(Nodes, ?MODULE, echo, [Value]),
          assert_same([{N, Value} || N <- Nodes], Results),
          {Replies, []} = ecall:call_all_wait(Nodes, ?MODULE, echo, [Value]),
          assert_same(Results, Replies)
        end, node_lists(Config))
    end, [lists:seq(1, 100000), binary:copy(<<42>>, 1024 * 1024),
          {ecall_result, {ok, wrapped}}, {ecall_result, {error, wrapped}}]).

%%=================================================================
%% FAILURES AND LIFETIMES
%%=================================================================
-spec master_killed(list()) -> ok.
master_killed(Config) ->
  lists:foreach(
    fun({Function, Flags}) ->
      with_flight(Function, Flags, Config,
        fun(#flight{master = Master, args = Args} = Flight) ->
          exit(Master, kill),
          {'EXIT', {killed, {ecall, Function, Args}}} = await_result(Flight)
        end)
    end, group_variants()).

-spec master_killed_after_local_rejection(list()) -> ok.
master_killed_after_local_rejection(Config) ->
  Nodes = ?config(nodes, Config),
  Actions = maps:from_list(
    [{node(), {reject, [local_rejected]}} |
     [{N, {block, [self()]}} || N <- Nodes]]),
  Args = [[node() | Nodes], ?MODULE, node_action, [Actions]],
  with_flight_args(call_any, Args, Nodes,
    fun(#flight{master = Master, args = ContextArgs} = Flight) ->
      ArgsWithFlag = Args ++ [false],
      ArgsWithFlag = ContextArgs,
      exit(Master, kill),
      {'EXIT', {killed, {ecall, call_any, ArgsWithFlag}}} = await_result(Flight)
    end).

-spec caller_killed_all(list()) -> ok.
caller_killed_all(Config) ->
  lists:foreach(
    fun(Flags) ->
      with_flight(call_all, Flags, Config,
        fun(#flight{callers = [{KilledNode, Caller}, {Other, _}]} = Flight) ->
          kill_caller(Caller),
          case Flags of
            [true] ->
              {error, {KilledNode, {badrpc, killed}}} = await_result(Flight);
            [] ->
              release_blocks(Flight#flight.blocked),
              {ok, [{Other, {released, Other}}]} = await_result(Flight)
          end
        end)
    end, [[], [true]]).

-spec caller_killed_wait(list()) -> ok.
caller_killed_wait(Config) ->
  with_flight(call_all_wait, [], Config,
    fun(#flight{callers = [{KilledNode, Caller}, {Other, _}]} = Flight) ->
      kill_caller(Caller),
      release_blocks(Flight#flight.blocked),
      {[{Other, {released, Other}}], [{KilledNode, {badrpc, killed}}]} =
        await_result(Flight)
    end).

-spec caller_killed_any(list()) -> ok.
caller_killed_any(Config) ->
  lists:foreach(
    fun(Flags) ->
      with_flight(call_any, Flags, Config,
        fun(#flight{callers = [{_KilledNode, Caller}, {Other, _}]} = Flight) ->
          kill_caller(Caller),
          release_blocks(Flight#flight.blocked),
          {ok, {Other, {released, Other}}} = await_result(Flight)
        end),
      with_flight(call_any, Flags, Config,
        fun(#flight{callers = Callers} = Flight) ->
          [exit(Caller, kill) || {_Node, Caller} <- Callers],
          case Flags of
            [] -> {error, none_is_available} = await_result(Flight);
            [true] ->
              {error, Errors} = await_result(Flight),
              assert_same([{N, {badrpc, killed}} || {N, _Pid} <- Callers],
                          Errors)
          end
        end)
    end, [[], [true]]).

-spec no_links(list()) -> ok.
no_links(Config) ->
  lists:foreach(
    fun(Function) ->
      with_flight(Function, [], Config,
        fun(#flight{master = Master, callers = Callers} = Flight) ->
          [assert_no_links(Pid) || Pid <- [Master | [P || {_, P} <- Callers]]],
          release_blocks(Flight#flight.blocked),
          await_result(Flight),
          ok
        end)
    end, group_functions()).

-spec early_return_keeps_callers(list()) -> ok.
early_return_keeps_callers(Config) ->
  with_flight(call_any, [], Config,
    fun(#flight{master = Master, callers = [{First, _}, {Second, Caller}],
                blocked = Blocks} = Flight) ->
      release_blocks([lists:keyfind(First, 1, Blocks)]),
      {ok, {First, {released, First}}} = await_result(Flight),
      wait_dead(Master),
      true = is_process_alive(Caller),
      {Second, Remote} = lists:keyfind(Second, 1, Blocks),
      release_blocks([{Second, Remote}])
    end),
  with_flight(call_all, [true], Config,
    fun(#flight{master = Master, callers = [{First, Killed}, {_Second, Other}]}
        = Flight) ->
      exit(Killed, kill),
      {error, {First, {badrpc, killed}}} = await_result(Flight),
      wait_dead(Master),
      true = is_process_alive(Other)
    end).

-spec owner_hygiene(list()) -> ok.
owner_hygiene(Config) ->
  Nodes = ?config(nodes, Config),
  lists:foreach(
    fun(Function) ->
      Owner = self(),
      Ref = make_ref(),
      Controller = spawn(fun() ->
        Blocks = await_blocks(Nodes),
        Master = master_of(Owner),
        Owner ! {group_master, Ref, Master},
        release_blocks(Blocks)
      end),
      apply(ecall, Function, [Nodes, ?MODULE, block, [Controller]]),
      receive
        {group_master, Ref, Master} -> wait_dead(Master)
      after 5000 -> ct:fail(hygiene_controller_did_not_report)
      end,
      assert_clean_owner()
    end, group_functions()).

-spec unrelated_owner_messages(list()) -> ok.
unrelated_owner_messages(Config) ->
  Nodes = ?config(nodes, Config),
  lists:foreach(
    fun(Function) ->
      Ref = make_ref(),
      [self() ! {unrelated_group_message, Ref, I} || I <- lists:seq(1, 100)],
      apply(ecall, Function, [Nodes, ?MODULE, echo, [ok]]),
      lists:foreach(
        fun(I) ->
          receive {unrelated_group_message, Ref, I} -> ok
          after 0 -> ct:fail({unrelated_message_lost, I})
          end
        end, lists:seq(1, 100)),
      assert_clean_owner()
    end, group_functions()).

-spec owner_killed(list()) -> ok.
owner_killed(Config) ->
  lists:foreach(
    fun(Function) ->
      with_flight(Function, [], Config,
        fun(#flight{owner = Owner, master = Master, callers = Callers,
                    blocked = Blocks}) ->
          exit(Owner, kill),
          wait_dead(Owner),
          true = is_process_alive(Master),
          [true = is_process_alive(Pid) || {_Node, Pid} <- Callers],
          release_blocks(Blocks),
          [wait_dead(Pid) || Pid <- [Master | [P || {_, P} <- Callers]]],
          ok
        end)
    end, group_functions()).

-spec no_error_reports(list()) -> ok.
no_error_reports(Config) ->
  Nodes = ?config(nodes, Config),
  Missing = ?config(missing, Config),
  Ref = make_ref(),
  ok = logger:add_handler(ecall_group_test_log, ?MODULE,
                         #{level => error, config => #{owner => self(), ref => Ref}}),
  try
    {ok, _Replies} = ecall:call_all(Nodes, ?MODULE, echo, [ok]),
    {[], Rejects} = ecall:call_all_wait(
      [Missing | Nodes], ?MODULE, reject, [rejected]),
    assert_same([{Missing, {badrpc, noconnection}} |
                 [{N, rejected} || N <- Nodes]], Rejects),
    receive
      {group_log_error, Ref, Event} -> ct:fail({unexpected_error_report, Event})
    after 100 -> ok
    end
  after
    ok = logger:remove_handler(ecall_group_test_log)
  end.

%%=================================================================
%% REMOTE FUNCTIONS AND LOGGER HANDLER
%%=================================================================
-spec echo(term()) -> term().
echo(Value) -> Value.

-spec reject(term()) -> {error, term()}.
reject(Reason) -> {error, Reason}.

-spec crash() -> no_return().
crash() -> error(boom).

-spec sleep_echo(non_neg_integer(), term()) -> term().
sleep_echo(Milliseconds, Value) ->
  timer:sleep(Milliseconds),
  Value.

-spec notify_echo(pid(), term()) -> term().
notify_echo(Owner, Value) ->
  Owner ! {called, node()},
  Value.

-spec block(pid()) -> {released, node()}.
block(Owner) ->
  Owner ! {blocked, node(), self()},
  receive release -> {released, node()} end.

-spec node_action(#{node() => {atom(), list()}}) -> term().
node_action(Actions) ->
  {Function, Args} = maps:get(node(), Actions),
  apply(?MODULE, Function, Args).

-spec adding_handler(map()) -> {ok, map()}.
adding_handler(Config) -> {ok, Config}.

-spec removing_handler(map()) -> ok.
removing_handler(_Config) -> ok.

-spec log(map(), map()) -> ok.
log(#{level := error} = Event, #{config := #{owner := Owner, ref := Ref}}) ->
  Owner ! {group_log_error, Ref, Event},
  ok;
log(_Event, _Config) -> ok.

%%=================================================================
%% IN-FLIGHT GROUP PROTOCOL
%%=================================================================
with_flight(Function, Flags, Config, Test) ->
  Nodes = ?config(nodes, Config),
  Args = [Nodes, ?MODULE, block, [self()]] ++ Flags,
  with_flight_args(Function, Args, Nodes, Test).

with_flight_args(Function, Args, Nodes, Test) ->
  Ref = make_ref(),
  Parent = self(),
  Owner = spawn(fun() ->
    Result = catch apply(ecall, Function, Args),
    Parent ! {group_result, Ref, self(), Result}
  end),
  Blocks = await_blocks(Nodes),
  Master = master_of(Owner),
  Callers = callers_of(Master, Nodes),
  ContextArgs = case {Function, length(Args)} of
    {call_all_wait, 4} -> Args;
    {_Group, 4} -> Args ++ [false];
    {_Group, 5} -> Args
  end,
  Flight = #flight{owner = Owner, ref = Ref, master = Master,
                   callers = Callers, blocked = Blocks, args = ContextArgs},
  try Test(Flight)
  after
    release_blocks(Blocks),
    [wait_dead(Pid) || Pid <- [Owner, Master | [P || {_, P} <- Callers]]],
    receive {group_result, Ref, Owner, _Result} -> ok after 0 -> ok end
  end,
  ok.

await_result(#flight{owner = Owner, ref = Ref}) ->
  receive
    {group_result, Ref, Owner, Result} -> Result
  after 5000 -> ct:fail(group_call_hung)
  end.

await_blocks(Nodes) ->
  [receive
     {blocked, Node, Pid} -> {Node, Pid}
   after 5000 -> ct:fail({node_did_not_block, Node})
   end || Node <- Nodes].

release_blocks(Blocks) ->
  [Pid ! release || {_Node, Pid} <- Blocks],
  ok.

kill_caller(Caller) ->
  exit(Caller, kill),
  wait_dead(Caller).

%%=================================================================
%% PROCESS INSPECTION
%%=================================================================
%% process_info is intentionally confined to test diagnostics. Proxy callers
%% monitor a pool worker; the public connection_info master owns that worker
%% through a link, which lets the test associate callers with their nodes.
master_of(Owner) ->
  {monitors, [{process, Master}]} = process_info(Owner, monitors),
  Master.

callers_of(Master, Nodes) ->
  {monitors, Monitors} = process_info(Master, monitors),
  Callers = [Pid || {process, Pid} <- Monitors],
  Count = length(Nodes),
  Count = length(Callers),
  lists:sort([{caller_node(Pid, Nodes), Pid} || Pid <- Callers]).

caller_node(Caller, Nodes) ->
  {monitors, [{process, Proxy}]} = process_info(Caller, monitors),
  [Node] = [N || N <- Nodes, connection_owns_proxy(N, Proxy)],
  Node.

connection_owns_proxy(Node, Proxy) ->
  {ok, #{connection_pid := Master}} = ecall:connection_info(Node),
  {links, Links} = process_info(Master, links),
  lists:member(Proxy, Links).

assert_no_links(Pid) ->
  {links, []} = process_info(Pid, links),
  ok.

assert_clean_owner() ->
  {monitors, []} = process_info(self(), monitors),
  {message_queue_len, 0} = process_info(self(), message_queue_len),
  ok.

wait_dead(Pid) -> wait_dead(Pid, 250).

wait_dead(Pid, 0) -> ct:fail({process_still_alive, Pid});
wait_dead(Pid, Attempts) ->
  case is_process_alive(Pid) of
    false -> ok;
    true -> timer:sleep(20), wait_dead(Pid, Attempts - 1)
  end.

%%=================================================================
%% PEER UTILITIES
%%=================================================================
%% The control connection is independent of the distribution link. Peers use
%% this suite's code path so all called functions exist on the remote nodes.
start_peer(Name) ->
  {ok, Peer, Node} = peer:start_link(#{
    name => Name,
    longnames => false,
    connection => standard_io,
    args => ["-pa" | code:get_path()] ++
            ["-setcookie", atom_to_list(erlang:get_cookie())],
    wait_boot => 10000
  }),
  {ok, _} = peer:call(Peer, application, ensure_all_started, [ecall]),
  true = peer:call(Peer, net_kernel, connect_node, [node()]),
  {Peer, Node}.

stop_peer(Peer) ->
  ok = peer:stop(Peer),
  receive
    {'EXIT', Peer, _Reason} -> ok
  after 5000 -> ct:fail(peer_did_not_stop)
  end.

wait_both_connected(Peer, Node) ->
  wait_both_connected(Peer, Node, 250).

wait_both_connected(_Peer, _Node, 0) -> ct:fail(not_both_connected);
wait_both_connected(Peer, Node, Attempts) ->
  LocalInfo = ecall:connection_info(Node),
  PeerInfo = peer:call(Peer, ecall, connection_info, [node()]),
  case {LocalInfo, PeerInfo} of
    {{ok, #{status := connected}}, {ok, #{status := connected}}} -> ok;
    _NotReady ->
      timer:sleep(20),
      wait_both_connected(Peer, Node, Attempts - 1)
  end.

%%=================================================================
%% ASSERTION UTILITIES
%%=================================================================
node_lists(Config) ->
  Nodes = ?config(nodes, Config),
  [Nodes, [node() | Nodes]].

group_functions() -> [call_any, call_all, call_all_wait].

group_variants() ->
  [{call_any, []}, {call_any, [true]},
   {call_all, []}, {call_all, [true]}, {call_all_wait, []}].

await_called() ->
  receive {called, Node} -> Node
  after 5000 -> ct:fail(expected_call_or_cast_not_received)
  end.

assert_same(Expected, Actual) ->
  Sorted = lists:sort(Expected),
  Sorted = lists:sort(Actual),
  ok.
