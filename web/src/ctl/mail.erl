%%% @file outside.erl
%%%
%%%     Контроллеры, если пользователи пока не заходили
%%%

-module(mail).

-define(UPDATER_ID, 1).

-export([
    signup/2,
    signdown/2,
    test/0,
    test/1
]).


-import(mochiweb_cookies, [cookie/2]).

-include("web_session.hrl").
-include("common.hrl").
-include("web.hrl").

-define(CATCHA_COOKIE, "captcha_codehex").


signup(Req, Param) ->
    Xsl_path = "xsl/mail/outside/signup.xsl",

    Meta = [
            {"current-path",        Req:get(path)}
    ],
    Xml  = xml:encode_data(
        [
            {"meta",    Meta}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_TEXT, [], [Outty]}.

signdown(Req, Param) ->
    Xsl_path = "xsl/normal/outside/index.xsl",
    Meta = [
            {"current-path",        Req:get(path)}
    ],
    Xml  = xml:encode_data(
        [
            {"meta",    Meta}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_TEXT, [], [Outty]}.

test()->
    % erlang 13 ->
    %   http:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []).
    % erlang 15 ->
    %   httpc:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []).
    %?HTTPC:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []),

    ok.

test(speed) ->

    ok.