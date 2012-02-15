% -----------------------------------------------------------------------------
%% common defines
% -----------------------------------------------------------------------------

-define(FMT(F,P), lists:flatten(io_lib:format(F,P)) ).
-define(APP, web).


-define(VK_STREAMER_DEFAULT, "http://192.168.2.187:8000").



-define( INFO(P),  flog:info(P) ).
-define( ERROR(P), flog:error(P) ).
%-define( DEBUG(P), flog:debug(P) ).

-define( DEBUG(P), io:format(P,[]) ).

-define( I(F, P),   ?INFO(?FMT(F,P)) ).
-define( E(F, P),   ?ERROR(?FMT(F,P)) ).
%-define( D(F, P),   ?DEBUG(?FMT(F,P)) ).
-define( D(F, P),   io:format(F,P) ).

%-define( DEBUG_INFO(F, P),  ?DEBUG(?FMT(F,P)) ).

-define( DEBUG_INFO(F, P),  io:format(F,P) ).


-define( CFG_PROCS, [{gen_server, m_pinger},
                     {gen_event, error_logger}]
       ).