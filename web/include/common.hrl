%%% ---------------------------------------------------------------------------
%%% САМООПРЕДЕЛЕНИЕ
%%% ---------------------------------------------------------------------------

-define(APP, web).

-define(WEB_APP_TIMEOUT,    5000).
-define(WEB_APP_DELAY,      2000).
-define(WEB_APP_NUMBER,     10).

-define(WEB_APP_HTTP,   web_web_http_80).
-define(WEB_APP_HTTPS,  web_web_https_8443).

-define(DEFAULT_HTTP_IP,        "0.0.0.0").
-define(DEFAULT_HTTPS_IP,       "0.0.0.0").
-define(DEFAULT_HTTP_PORT,      8000).
-define(DEFAULT_HTTPS_PORT,     8443).
-define(DEFAULT_HTTPS_CERTFILE, "priv/https-files/cert.pem").
-define(DEFAULT_HTTPS_KEYFILE,  "priv/https-files/key.pem").

-define(HTTP_IP,        config:get(http_host,       ?DEFAULT_HTTP_IP)).
-define(HTTP_PORT,      config:get(http_port,       ?DEFAULT_HTTP_PORT)).
-define(HTTPS_IP,       config:get(https_host,      ?DEFAULT_HTTPS_IP)).
-define(HTTPS_PORT,     config:get(https_port,      ?DEFAULT_HTTPS_PORT)).
-define(HTTPS_CERTFILE, config:get(https_certfile,  ?DEFAULT_HTTPS_CERTFILE)).
-define(HTTPS_KEYFILE,  config:get(https_keytfile,  ?DEFAULT_HTTPS_KEYFILE)).

-define(SYS_MAIL_NAME_DEFAULT,      "Система рекламы tvzavr.ru").
-define(SYS_MAIL_USERNAME_DEFAULT,  "nikitin.i@tvzavr.ru").
-define(SYS_MAIL_RELAY_DEFAULT,     "active-video.ru").
-define(SYS_MAIL_PASSWORD_DEFAULT,  "maiqu6Ce6aht").

-define(SYS_MAIL_NAME,      config:get(sys_mail_name,       ?SYS_MAIL_NAME_DEFAULT)).
-define(SYS_MAIL_USERNAME,  config:get(sys_mail_username,   ?SYS_MAIL_USERNAME_DEFAULT)).
-define(SYS_MAIL_RELAY,     config:get(sys_mail_relay,      ?SYS_MAIL_RELAY_DEFAULT)).
-define(SYS_MAIL_PASSWORD,  config:get(sys_mail_password,   ?SYS_MAIL_PASSWORD_DEFAULT)).

-define(SYS_MAIL_OPTIONS,
    [
        {relay,    ?SYS_MAIL_RELAY},
        {username, ?SYS_MAIL_USERNAME},
        {password, ?SYS_MAIL_PASSWORD}
    ]).


-define(SYS_BILL_PRODUCT_ID_DEFAULT,  "006268-0001-0001").
-define(SYS_BILL_SHOP_ID_DEFAULT,     "adv").
-define(SYS_BILL_PS_ID_DEFAULT,       "chronopay").
-define(SYS_BILL_SECRET_DEFAULT,      "ahGoh6elShoa5vei").

-define(SYS_DNS_DEFAULT, "http://"
    ++ config:get(http_host, ?DEFAULT_HTTP_IP)
    ++ ":"
    ++ config:get(http_port, ?DEFAULT_HTTP_PORT)
).

-define(SYS_DNS,        config:get(sys_dns, ?SYS_DNS_DEFAULT)).

-define(SYS_BILL_PRODUCT_ID,   config:get(sys_bill_product_id,  ?SYS_BILL_PRODUCT_ID_DEFAULT)).
-define(SYS_BILL_SHOP_ID,      config:get(sys_bill_shop_id,     ?SYS_BILL_SHOP_ID_DEFAULT)).
-define(SYS_BILL_PS_ID,        config:get(sys_bill_ps_id,       ?SYS_BILL_PS_ID_DEFAULT)).
-define(SYS_BILL_SECRET,       config:get(sys_bill_secret,      ?SYS_BILL_SECRET_DEFAULT)).

-define(SYS_BILL_SURL,      ?SYS_DNS ++ "/surl").
-define(SYS_BILL_FURL,      ?SYS_DNS ++ "/furl").

-define(VK_STREAMER_DEFAULT, "http://192.168.2.156:7000").


-define( CFG_PROCS, [{gen_server, m_pinger},
                     {gen_event, error_logger}]
       ).


%%% ---------------------------------------------------------------------------
%%% LOGING
%%% ---------------------------------------------------------------------------

-define(FMT(F,P), lists:flatten(io_lib:format(F,P)) ).

-define( INFO(P),  flog:info(P) ).
-define( ERROR(P), flog:error(P) ).

-define( I(F, P),   ?INFO(?FMT(F,P)) ).
-define( E(F, P),   ?ERROR(?FMT(F,P)) ).

%%% 
%%% -ifdef(debug).
%%%     -ifdef(ext_debug).
%%%         -define( DEBUG(P), io:format(P,[]) ).
%%%         -define( D(F, P),   io:format(F,P) ).
%%%     -else.
%%%         -define( DEBUG(P), flog:debug(P) ).
%%%         -define( D(F, P),  ?DEBUG(?FMT(F,P)) ).
%%%     -endif.
%%% -else.
%%%     -define( DEBUG(P), true).
%%%     -define( D(F, P),  true).
%%% -endif.
%%%

-define( DEBUG(P), io:format(P,[]) ).
-define( D(F, P),   io:format(F,P) ).

