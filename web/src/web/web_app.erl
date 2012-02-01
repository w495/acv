-module(web_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1,start_phase/3]).

-include("../include/common.hrl").

-define(WEB_APP_TIMEOUT, 5000).
-define(WEB_APP_NUMBER, 10).
-define(WEB_APP_DELAY, 2).

-define(WEB_APP_HTTP,   web_web_http_80).
-define(WEB_APP_HTTPS,  web_web_https_8443).

%%%
%%% Имена модулей вынесены
%%%     на случай переименования этих модулей в будущем
%%%
-define(WEB_APP_WEB,   web_web).
-define(WEB_APP_SUP,   web_sup).

%%% @doc
%%% Стартуем само приложение
%%%
start(Type, _StartArgs) ->
    web_deps:ensure(),
    ok = error_logger:add_report_handler(flog, [web]),
    web:reload_cfg(),
    Rc = ?WEB_APP_SUP:start_link(),
    case Type of
        {takeover,_} -> ok;
        _ -> intermal_start(?WEB_APP_NUMBER,?WEB_APP_DELAY)
    end,
    Rc.

start_phase(go, {takeover,FromNode}, _) ->
    case rpc:call(FromNode, ?WEB_APP_WEB, stop, []) of
        ok -> intermal_start(?WEB_APP_NUMBER,?WEB_APP_DELAY);
        Error -> {error,{takeover,FromNode,Error}}
    end;
start_phase(go, _Type, _ ) ->
    ok.

%%% @doc
%%% Стартуем обработку запросов по контроллекрам
%%% Тут испьзуем два приложения http и https. 
%%%
intermal_start(N,Delay)->
    start_http(N,Delay),
    start_https(N,Delay).

%%% @doc
%%% Стартуем обработку http запросов по контроллекрам
%%%
start_http(0,_) -> {error,{start_http,eaddrinuse}};
start_http(N,Delay) ->
    Http_config = [
        {name, ?WEB_APP_HTTP},
        {ip, config:get(http_host, "0.0.0.0")},
        {port, config:get(http_port, 8000)}
    ],
    Http_web = {?WEB_APP_HTTP, {?WEB_APP_WEB, start, [Http_config]},
        permanent, ?WEB_APP_TIMEOUT, worker, dynamic},
    case supervisor:start_child(?WEB_APP_SUP, Http_web) of
        {ok, _} -> ok;
        {error, {eaddrinuse,_}} ->
            timer:sleep(Delay*1000),
            start_http(N-1,Delay)
    end.

%%% @doc
%%% Стартуем обработку https запросов по контроллекрам
%%%
%%% Вообще https можно настроить средствами nginx,
%%%     но оставляем дополнительную надстройку на уровне нашего приложения
%%%
start_https(0,_) -> {error,{start_https,eaddrinuse}};
start_https(N,Delay) ->
    Https_config = [
        {name, ?WEB_APP_HTTPS},
        {ip, config:get(https_host, "0.0.0.0")},
        {ssl, true},
        {ssl_opts, [
            {certfile,config:get(https_certfile,"priv/https-files/cert.pem")},
            {keyfile, config:get(https_keyfile, "priv/https-files/key.pem")}
        ]},
        {port, config:get(https_port, 8443)}
    ],
    Https_web = {?WEB_APP_HTTPS, {?WEB_APP_WEB, start, [Https_config]},
        permanent, ?WEB_APP_TIMEOUT, worker, dynamic},
    case supervisor:start_child(?WEB_APP_SUP, Https_web) of
        {ok, _} -> ok;
        {error, {eaddrinuse,_}} ->
            timer:sleep(Delay*1000),
            start_https(N-1,Delay)
    end.


stop(_State) ->
    error_logger:delete_report_handler(flog),
    ok = supervisor:terminate_child(?WEB_APP_SUP, ?WEB_APP_HTTP),
    ok = supervisor:delete_child(?WEB_APP_SUP,    ?WEB_APP_HTTP),

    % ok = supervisor:terminate_child(?WEB_APP_SUP, ?WEB_APP_HTTPS),
    % ok = supervisor:delete_child(?WEB_APP_SUP,    ?WEB_APP_HTTPS),

    ok.

