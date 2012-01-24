%%% @file outside.erl
%%%
%%%     Контроллеры, если пользователи пока не заходили
%%%

-module(outside).

-export([
    index/1,
    about/1,
    login/1,
    signup/1,
    test/0,
    test/1
]).


-import(mochiweb_cookies, [cookie/2]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/web.hrl").

%% 
%% возврщает головную страницу
%%
index(Req) ->
    Xsl_path = "priv/xsl/normal/outside/index.xsl",
    Meta = [
            {"current-path",        Req:get(path)}
    ],
    Xml  = xml:encode_data(
        [
            {"meta",    Meta}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% возврщает страницу с описанием проекта
%%
about(Req) ->
    Xsl_path = "priv/xsl/normal/outside/index.xsl",
    Meta = [
            {"current-path",        Req:get(path)}
    ],
    Xml  = xml:encode_data(
        [
            {"meta",    Meta}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% возврaщает страницу входа.
%%  альтернативный вариант
%%
login(Req) ->
    Xsl_path = "priv/xsl/normal/outside/index.xsl",
    Meta = [
            {"current-path",        Req:get(path)}
    ],
    Xml  = xml:encode_data(
        [
            {"meta",    Meta}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.


%%
%% возврaщает страницу регистрации
%%
signup(Req) ->
    Xsl_path = "priv/xsl/normal/outside/index.xsl",
    Meta = [
            {"current-path",        Req:get(path)}
    ],
    Xml  = xml:encode_data(
        [
            {"meta",    Meta}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

test()->
    ok.

test(speed)->
    ok.

