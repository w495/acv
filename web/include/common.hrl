% -----------------------------------------------------------------------------
%% common defines
% -----------------------------------------------------------------------------

-define(FMT(F,P), lists:flatten(io_lib:format(F,P)) ).
-define(APP, web).


-define(VK_STREAMER_DEFAULT, "http://192.168.2.156:7000").

-define( CFG_PROCS, [{gen_server, m_pinger},
                     {gen_event, error_logger}]
       ).


%%% ---------------------------------------------------------------------------
%%% LOGING
%%% ---------------------------------------------------------------------------

-define( INFO(P),  flog:info(P) ).
-define( ERROR(P), flog:error(P) ).

-define( I(F, P),   ?INFO(?FMT(F,P)) ).
-define( E(F, P),   ?ERROR(?FMT(F,P)) ).

-ifdef(debug).
    -ifdef(ext_debug).
        -define( DEBUG(P), io:format(P,[]) ).
        -define( D(F, P),   io:format(F,P) ).
    -else.
        -define( DEBUG(P), flog:debug(P) ).
        -define( D(F, P),  ?DEBUG(?FMT(F,P)) ).
    -endif.
-else.
    -define( DEBUG(P), true).
    -define( D(F, P),  true).
-endif.



%%% ---------------------------------------------------------------------------
%%% EVENTS
%%% ---------------------------------------------------------------------------

-define(SIGNUP_EVENT, signup_event).
-define(ACVVID_EVENT, acvvid_event).
-define(ACVBAN_EVENT, acvban_event).
