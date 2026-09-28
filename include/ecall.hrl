
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

%%-------------------------------------------------------------------------------
%% LOGGING
%%-------------------------------------------------------------------------------

-ifndef(TEST).

-define(MFA_METADATA, #{
  mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
  line => ?LINE
}).

-define(LOGERROR(Text),          logger:error(Text, [], ?MFA_METADATA)).
-define(LOGERROR(Text,Params),   logger:error(Text, Params, ?MFA_METADATA)).
-define(LOGWARNING(Text),        logger:warning(Text, [], ?MFA_METADATA)).
-define(LOGWARNING(Text,Params), logger:warning(Text, Params, ?MFA_METADATA)).
-define(LOGINFO(Text),           logger:info(Text, [], ?MFA_METADATA)).
-define(LOGINFO(Text,Params),    logger:info(Text, Params, ?MFA_METADATA)).
-define(LOGDEBUG(Text),          logger:debug(Text, [], ?MFA_METADATA)).
-define(LOGDEBUG(Text,Params),   logger:debug(Text, Params, ?MFA_METADATA)).

-else.

% ct:pal works only on the node that runs common_test (rebar3 rewrites it to
% cthr:pal, which exists only there). Peer nodes started by a suite run the
% same beams and log through logger, peer forwards their output to the test node.
-define(CT_PAL(Text, Params),
  case whereis(ct_util_server) of
    undefined -> logger:notice(Text, Params);
    _ -> ct:pal(Text, Params)
  end).

-define(LOGERROR(Text),           ?CT_PAL("error: " ++ Text, [])).
-define(LOGERROR(Text, Params),   ?CT_PAL("error: " ++ Text, Params)).
-define(LOGWARNING(Text),         ?CT_PAL("warning: " ++ Text, [])).
-define(LOGWARNING(Text, Params), ?CT_PAL("warning: " ++ Text, Params)).
-define(LOGINFO(Text),            ?CT_PAL("info: " ++ Text, [])).
-define(LOGINFO(Text, Params),    ?CT_PAL("info: " ++ Text, Params)).
-define(LOGDEBUG(Text),           ?CT_PAL("debug: " ++ Text, [])).
-define(LOGDEBUG(Text, Params),   ?CT_PAL("debug: " ++ Text, Params)).

-endif.


-endif.
