
-module(ecall_test).

%% API
-export([
  erpc_call/2, do_erpc_call_request/1,
  erpc_cast/2, do_erpc_cast_request/1,
  simple_send/3, simple_send_receive/1,
  pool_send/3, pool_send_receive/1,
  pool_tcp_send/3, pool_tcp_listen/1
]).

-define(LOG(Text,Params),lager:info(Text,Params)).

-define(LOG_COUNT, 100).
-define(TS, erlang:system_time(millisecond)).

-define(DATA, #{
  archive1 => #{
    field1 => value1,
    field2 => value2,
    field3 => value3,
    field4 => value4,
    field5 => value5,
    field6 => value6,
    field7 => value7,
    field8 => value8,
    field9 => value9,
    field10 => value10
  },
  archive2 => #{
    field1 => value1,
    field2 => value2,
    field3 => value3,
    field4 => value4,
    field5 => value5,
    field6 => value6,
    field7 => value7,
    field8 => value8,
    field9 => value9,
    field10 => value10
  },
  archive3 => #{
    field1 => value1,
    field2 => value2,
    field3 => value3,
    field4 => value4,
    field5 => value5,
    field6 => value6,
    field7 => value7,
    field8 => value8,
    field9 => value9,
    field10 => value10
  }
}).


erpc_call( Node, Count )->
  ?LOG("call test start node ~p count ~p",[ Node, Count ]),
  [ spawn(fun()-> do_erpc_call(Node, ?LOG_COUNT, ?TS) end)  || _ <- lists:seq(1,Count)],
  ok.

do_erpc_call( Node, Count, TS ) when Count > 0 ->
  _= erpc:call( Node, ?MODULE, do_erpc_call_request, [ ?DATA ] ),
  do_erpc_call( Node, Count-1, TS );
do_erpc_call( Node, _Count, TS ) ->
  ?LOG("duration ~p",[ ?TS - TS ]),
  timer:sleep(100),
  do_erpc_call( Node, ?LOG_COUNT, ?TS ).

do_erpc_call_request( _Data )->
  ok.

erpc_cast( Node, Count )->
  ?LOG("cast test start node ~p count ~p",[ Node, Count ]),
  [ spawn(fun()-> do_erpc_cast(Node, ?LOG_COUNT, ?TS) end)  || _ <- lists:seq(1,Count)],
  ok.

do_erpc_cast( Node, Count, TS ) when Count > 0 ->
  _= erpc:cast( Node, ?MODULE, do_erpc_cast_request, [ ?DATA ] ),
  do_erpc_cast( Node, Count-1, TS );
do_erpc_cast( Node, _Count, TS ) ->
  ?LOG("duration ~p",[ ?TS - TS ]),
  timer:sleep(100),
  do_erpc_cast( Node, ?LOG_COUNT, ?TS ).

do_erpc_cast_request( _Data )->
  ok.

simple_send( Name, Node, Count )->
  ?LOG("simple send test start partner name ~p, node ~p, count ~p",[ Name, Node, Count ]),
  [ spawn(fun()-> do_simple_send(Name, Node, ?LOG_COUNT, ?TS) end)  || _ <- lists:seq(1,Count)],
  ok.

do_simple_send( Name, Node, Count, TS ) when Count > 0 ->
  {Name, Node} ! {request, ?DATA},
  do_simple_send( Name, Node, Count-1, TS );
do_simple_send( Name, Node, _Count, TS ) ->
  ?LOG("duration ~p",[ ?TS - TS ]),
  timer:sleep(100),
  do_simple_send( Name, Node, ?LOG_COUNT, ?TS ).

simple_send_receive( Name )->
  register( Name, self() ),
  simple_send_receive_loop( ?LOG_COUNT ).

simple_send_receive_loop( Count ) when Count > 0->
  receive
    {request, _}->simple_send_receive_loop( Count - 1 )
  end;
simple_send_receive_loop( _Count )->
  ?LOG("accept ~p",[?LOG_COUNT]),
  simple_send_receive_loop( ?LOG_COUNT ).


pool_send( Name, Node, Count )->
  ?LOG("pool send test start: partner name ~p, node ~p, count ~p",[ Name, Node, Count ]),
  Pool = [ {list_to_atom( atom_to_list( Name )++ integer_to_list(I) ), Node} || I <- lists:seq(1,16) ],
  [ spawn(fun()-> do_pool_send(Pool, Pool, ?LOG_COUNT, ?TS) end)  || _ <- lists:seq(1,Count)],
  ok.

do_pool_send( [P|Rest], Pool, Count, TS ) when Count > 0 ->
  erlang:send(P, term_to_binary({request, ?DATA}), [noconnect]),
  do_pool_send( Rest, Pool, Count-1, TS );
do_pool_send( [], Pool, Count, TS )->
  do_pool_send( Pool, Pool, Count, TS );
do_pool_send( Rest, Pool, _Count, TS ) ->
  ?LOG("duration ~p",[ ?TS - TS ]),
  timer:sleep(100),
  do_pool_send( Rest, Pool, ?LOG_COUNT, ?TS ).

pool_send_receive( Name )->
  Pool = [ list_to_atom( atom_to_list( Name )++ integer_to_list(I) ) || I <- lists:seq(1,16) ],
  [ spawn(fun()->register( P, self()), pool_send_receive_loop(?LOG_COUNT) end) || P <- Pool ],
  ok.

pool_send_receive_loop( Count ) when Count > 0->
  receive
    Data->
      binary_to_term( Data ),
      pool_send_receive_loop( Count - 1 )
  end;
pool_send_receive_loop( _Count )->
  ?LOG("accept ~p",[?LOG_COUNT]),
  pool_send_receive_loop( ?LOG_COUNT ).


pool_tcp_send( Host, Port, Count )->
  Self = self(),
  Pool = [ spawn(fun()->tcp_send_init( Host, Port, Self ) end) || _ <- lists:seq(1,16)],
  [ receive {ready, P}-> ok end || P <- Pool ],
  [ spawn(fun()-> do_tcp_pool_send(Pool, Pool, ?LOG_COUNT, ?TS) end)  || _ <- lists:seq(1,Count)],
  ok.

do_tcp_pool_send( [P|Rest], Pool, Count, TS ) when Count > 0 ->
  P ! {self(), {request, ?DATA}},
  receive {ok, P} -> ok end,
  do_tcp_pool_send( Rest, Pool, Count-1, TS );
do_tcp_pool_send( [], Pool, Count, TS )->
  do_tcp_pool_send( Pool, Pool, Count, TS );
do_tcp_pool_send( Rest, Pool, _Count, TS ) ->
  ?LOG("duration ~p",[ ?TS - TS ]),
  timer:sleep(100),
  do_tcp_pool_send( Rest, Pool, ?LOG_COUNT, ?TS ).

tcp_send_init( Host, Port, Owner )->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false},{packet, 4}] , 10000 ),
  Owner ! {ready, self()},
  tcp_send_loop( Socket, []).

tcp_send_loop( Socket, [] )->
  receive
    {PID, Data}-> tcp_send_loop(Socket,[{PID,Data}])
  end;
tcp_send_loop( Socket, Buffer ) when length(Buffer) < 1000->
  receive
    {PID,Data} -> tcp_send_loop( Socket, [{PID,Data} | Buffer] )
  after
    0 ->
      Data =  [begin PID ! {ok, self()}, D end || {PID, D} <- Buffer],
      ok = gen_tcp:send(Socket, term_to_binary(Data)),
      tcp_send_loop( Socket, [] )
  end;
tcp_send_loop( Socket, Buffer )->
  Data =  [begin PID ! {ok, self()}, D end || {PID, D} <- Buffer],
  ok = gen_tcp:send(Socket, term_to_binary(Data)),
  tcp_send_loop( Socket, [] ).


pool_tcp_listen( Port )->
  spawn(fun()->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false},{packet, 4}]),
    accept_loop( ListenSocket )
  end).

accept_loop( ListenSocket )->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun()->in_connection( Socket, ?LOG_COUNT ) end),
  accept_loop( ListenSocket ).

in_connection( Socket, Count ) when Count > 0->
  case gen_tcp:recv(Socket, 0, infinity) of
    {ok, Data}->
      _ = binary_to_term( Data ),
      in_connection( Socket, Count -1 );
    {error, Error}->
      ?LOG("socket error ~p",[Error])
  end;
in_connection( Socket, _Count )->
  ?LOG("accept ~p",[?LOG_COUNT]),
  in_connection( Socket, ?LOG_COUNT ).
