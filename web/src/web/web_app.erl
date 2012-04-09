%%% @file
%%% Основной файл приложения,
%%%     c помощью него приложение стартует.
%%%

-module(web_app).
-author('zAVr TeaM').

-behaviour(application).
-export([start/2,stop/1,start_phase/3]).

-include("common.hrl").

%
% TODO: разнести по hrl
%
-define(WEB_APP_TIMEOUT,    5000).
-define(WEB_APP_DELAY,      2000).
-define(WEB_APP_NUMBER,     10).

%
% TODO: разнести по hrl
%
-define(WEB_APP_HTTP,   web_web_http_80).
-define(WEB_APP_HTTPS,  web_web_https_8443).

%
% TODO: разнести по hrl
%
-define(DEFAULT_HTTP_IP,        "0.0.0.0").
-define(DEFAULT_HTTPS_IP,       "0.0.0.0").
-define(DEFAULT_HTTP_PORT,      8000).
-define(DEFAULT_HTTPS_PORT,     8443).
-define(DEFAULT_HTTPS_CERTFILE, "priv/https-files/cert.pem").
-define(DEFAULT_HTTPS_KEYFILE,  "priv/https-files/key.pem").

%
% TODO: разнести по hrl
%
-define(HTTP_IP,        config:get(http_host,       ?DEFAULT_HTTP_IP)).
-define(HTTP_PORT,      config:get(http_port,       ?DEFAULT_HTTP_PORT)).
-define(HTTPS_IP,       config:get(https_host,      ?DEFAULT_HTTPS_IP)).
-define(HTTPS_PORT,     config:get(https_port,      ?DEFAULT_HTTPS_PORT)).
-define(HTTPS_CERTFILE, config:get(https_certfile,  ?DEFAULT_HTTPS_CERTFILE)).
-define(HTTPS_KEYFILE,  config:get(https_keytfile,  ?DEFAULT_HTTPS_KEYFILE)).

%%%
%%% Имена модулей вынесены
%%%     на случай переименования этих модулей в будущем
%%%
-define(WEB_APP_WEB,   web_web).
-define(WEB_APP_SUP,   web_sup).


addMysqlPoolConnection(_, 0, _) ->
    done;
addMysqlPoolConnection(Mysql, Count, Params={Host, Port, User, Pass, DB}) ->
    mysql:connect(Mysql, Host, Port, User, Pass, DB, true),
    addMysqlPoolConnection(Mysql, Count-1, Params).

%%% @doc
%%% Стартуем само приложение
%%%
start(Type, _StartArgs) ->
    web_deps:ensure(),
    ok = error_logger:add_report_handler(flog, [web]),
    web:reload_cfg(),
    Rc = ?WEB_APP_SUP:start_link(),
    io:format("#################################~n~n"),
%    mysql:connect(mySqlConPool, config:get(vk_db_host, "localhost"), undefined, config:get(vk_db_user, "root"),  config:get(vk_db_password, "1111"), config:get(vk_db_name, "vk"), true),

    addMysqlPoolConnection(mySqlConPool, 9, {
        config:get(vk_db_host, "localhost"), 
        undefined, 
        config:get(vk_db_user, "root"), 
        config:get(vk_db_password, "1111"), 
        config:get(vk_db_name, "vk")
    }),

    addMysqlPoolConnection(mysqlStat, 9, {
        config:get(stat_db_host, "localhost"), 
        undefined, 
        config:get(stat_db_user, "root"), 
        config:get(stat_db_password, "1111"), 
        config:get(stat_db_name, "vk")
    }),


    case Type of
        {takeover,_} -> ok;
        _ -> internal_start(?WEB_APP_NUMBER,?WEB_APP_DELAY)
    end,
    Rc.

%%% @doc
%%% TODO: Не понятно, что это и зачем оно нужно
%%%
start_phase(go, {takeover,FromNode}, _) ->
    case rpc:call(FromNode, ?WEB_APP_WEB, stop, []) of
        {ok, ok} -> internal_start(?WEB_APP_NUMBER,?WEB_APP_DELAY);
        Error -> {error,{takeover,FromNode,Error}}
    end;
start_phase(go, _Type, _ ) ->
    ok.

%%% @doc
%%% Стартуем обработку запросов по контроллекрам
%%% Тут испьзуем два приложения http и https. 
%%%
internal_start(N,Delay)->
    Start_http  = start_http(N,Delay),
    Start_https = start_https(N,Delay),
    {Start_http, Start_https}.

%%% @doc
%%% Стартуем обработку http запросов по контроллекрам
%%%
start_http(0,_) -> {error,{start_http,eaddrinuse}};
start_http(N,Delay) ->
    Http_config = [
        {name, ?WEB_APP_HTTP},
        {ip, ?HTTP_IP},
        {port, ?HTTP_PORT}
    ],
    Http_web = {?WEB_APP_HTTP, {?WEB_APP_WEB, start, [Http_config]},
        permanent, ?WEB_APP_TIMEOUT, worker, dynamic},
    case supervisor:start_child(?WEB_APP_SUP, Http_web) of
        {ok, _} -> ok;
        {error, {eaddrinuse,_}} ->
            timer:sleep(Delay),
            start_http(N - 1,Delay)
    end.

%%% @doc
%%% Стартуем обработку https запросов по контроллекрам
%%%
%%% Вообще https можно настроить средствами nginx,
%%%     но оставляем дополнительную надстройку на уровне нашего приложения.
%%%
start_https(0,_) -> {error,{start_https,eaddrinuse}};
start_https(N,Delay) ->
    Https_config = [
        {name, ?WEB_APP_HTTPS},
        {ip, ?HTTPS_IP},
        {ssl, true},
        {ssl_opts, [
            {certfile,?HTTPS_CERTFILE},
            {keyfile, ?HTTPS_KEYFILE}
        ]},
        {port, ?HTTPS_PORT}
    ],
    Https_web = {?WEB_APP_HTTPS, {?WEB_APP_WEB, start, [Https_config]},
        permanent, ?WEB_APP_TIMEOUT, worker, dynamic},
    case supervisor:start_child(?WEB_APP_SUP, Https_web) of
        {ok, _} -> ok;
        {error, {eaddrinuse,_}} ->
            timer:sleep(Delay),
            start_https(N - 1,Delay)
    end.

%%% @doc
%%% Останавливает полностью все компоненты приложения.
%%% В НАШЕМ СЛУЧАЕ НЕ ИСПОЛЬЗУЕТСЯ.
%%% 
stop(_State) ->
    error_logger:delete_report_handler(flog),
    ok = supervisor:terminate_child(?WEB_APP_SUP, ?WEB_APP_HTTP),
    ok = supervisor:delete_child(?WEB_APP_SUP,    ?WEB_APP_HTTP),

    % ok = supervisor:terminate_child(?WEB_APP_SUP, ?WEB_APP_HTTPS),
    % ok = supervisor:delete_child(?WEB_APP_SUP,    ?WEB_APP_HTTPS),

    ok.

