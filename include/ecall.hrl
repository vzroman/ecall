
-ifndef(ecall).
-define(ecall,1).

%---------------pg constants-------------------------------------------
-define(pg_scope, ecall).
-define(pg_group, nodes).

%---------------supervisor defaults------------------------------------
-define(MAX_RESTARTS,10).
-define(MAX_PERIOD,1000).
-define(STOP_TIMEOUT,1000).

%--------------CONSTANTS-----------------------------------------------
-define(CONNECT_TIMEOUT, 10000).
-define(BATCH_SIZE, 1000).

%---------------logging------------------------------------------------
-define(T,atom_to_list(?FUNCTION_NAME)++": ").
-define(LOGDEBUG(Text,Params),lager:debug(?T++Text,Params)).
-define(LOGINFO(Text,Params),lager:info(?T++Text,Params)).
-define(LOGWARNING(Text,Params),lager:warning(?T++Text,Params)).
-define(LOGERROR(Text,Params),lager:error(?T++Text,Params)).

-endif.
