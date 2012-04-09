%%% @file outside.erl
%%%
%%%     Контроллеры, если пользователи пока не заходили
%%%

-module(outside).

-define(UPDATER_ID, 1).

-export([
    index/1,
    about/1,
    docs/1,
    % Страница не требуется
	%docs_video/1,
	docs_audience/1,
    signin/1,
    signin_post/1,
    signup/1,
    signup_post/1,
    captcha/1,
    pers/1,
    test/0,
    test/1
]).


-import(mochiweb_cookies, [cookie/2]).

-include("web_session.hrl").
-include("common.hrl").
-include("web.hrl").

-define(CATCHA_COOKIE, "captcha_codehex").

%%
%% Возврщает список вспомогательной информации для страницы
%%

meta([Req|_]) ->
    [
            {"login",               authorization:auth_getlogin(Req)},
            {"current-path",        Req:get(path)}
    ].

%% 
%% возврщает головную страницу
%%
index(Req) ->
    Xsl_path = "xsl/normal/outside/index.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),

    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% возврщает страницу с описанием проекта
%%
about(Req) ->
    Xsl_path = "xsl/normal/outside/about.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% Документация
%%
docs(Req) ->
    Xsl_path = "xsl/normal/outside/documents.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ), 
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

% Страница не требуется
%%
%% Документация видео
%%
docs_video(Req) ->
    Xsl_path = "xsl/normal/outside/documents_video.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ), 
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.
 
%%
%% Документация видео
%%
docs_audience(Req) ->
    Xsl_path = "xsl/normal/outside/documents_audience.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ), 
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.



%% @doc
%% Возврaщает страницу регистрации
%%
signin(Req) ->
    signin(Req, {normal}).

signin(Req, State) ->
    Xsl_path = "xsl/normal/outside/signin.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.


%% @doc
%% Выполняет авторизацию
%%
signin_post(Req) ->
    signin_post(Req, {normal}).

signin_post(Req, State) ->
    Data = Req:parse_post(),
    Login = proplists:get_value("login", Data),
    Password = proplists:get_value("password", Data),
    signin_post(Login, Password, State).

signin_post(Login, Password, State) ->
    ?D("Login  = ~p~n", [Login]),
    ?D("Password = ~p~n", [Password]),
    try
        Val = auth_biz:login(Login, Password),
        throw({ok, {redirect, "/pers",
            [mochiweb_cookies:cookie(?AUTHCOOKIE, Val, ?F_COOKIEOPTIONS)]}})
    catch
        throw:{ok, Ret} -> throw(Ret);
        throw:Error ->
            ?D("Error  = ~p~n", [Error]);
        Throw:Error ->
            ?D("~p  = ~p~n", [Throw, Error])
    end.

%% @doc
%% Возврaщает страницу регистрации
%%
signup(Req) ->
    signup(Req, {normal}).

signup(Req, {error, Error, Val}) ->
    Xsl_path = "xsl/normal/outside/signup.xsl",

    ?D(" Error => ~p", [Error]),

    Xml  = xml:encode_data(
        [
            {"val",     Val},
            {"error",   Error},
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),
    ?D("Xml = ~p~n", [Xml]),

    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]};

signup(Req, State) ->
    Xsl_path = "xsl/normal/outside/signup.xsl",

    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%% @doc
%% Выполняет регистрацию пользователя
%%
signup_post(Req) ->
    signup_post(Req, {normal}).


signup_post(Req, State) ->
    ?D("signup_post~n", []),
    Data = Req:parse_post(),
    ?D("signup_post~n", []),

    Pass        = proplists:get_value("password",     Data, ""),
    Pass_conf   = proplists:get_value("password-c",    Data, ""),
    Updater_id  = ?UPDATER_ID,

    Code_hex = Req:get_cookie_value("captcha_codehex"),
    Code     = proplists:get_value("captcha",    Data, ""),

    ?D("Data = ~p ~n", [Data]),

    case captcha:check(Code_hex, Code) of 
        true ->
            ?D("captcha:check > true~n", []),
            case Pass of
                Pass_conf ->
                    ?D("pass:check => true~n", []),
                    case Pass of
                        "null" ->
                            Pashash = null;
                        _ when length(Pass) /= 0 ->
                            Pashash = lists:flatten([io_lib:format("~2.16.0B", [X])
                                || X <- binary_to_list(erlang:md5(Pass))]);
                        _ ->
                            Pashash = null
                    end,
                    ?D("pass:check <= true~n", []),
                    E = norm:extr(Data, [{"firstname",      [string]},
                                        {"lastname",       [string]},
                                        {"patronimic",     [string]},
                                        {"login",          [string]},
                                        {"email",          [nullable, string]},
                                        {"city",           [nullable, string]},
                                        {"organization",   [nullable, string]},
                                        {"position",       [nullable, string]}]),

                    ?D("E:check > true~n", []),
                    Res = dao_customer:update_customer({E, Pashash, [], Updater_id}),
                    case Res  of
                        {ok, User_id} ->
                            Login = proplists:get_value("login", Data, ""),
                    
                            % Кидаем событие о создании пользователя
                            % gen_event:notify(?SIGNUP_EVENT, Data),

                            signin_post(Login, Pass, {normal});
                        {error, Error} ->
                            signup(Req, {error, Error, Data})
                    end;
                _ ->
                    ?D("Pass_conf  = [~s]~n", [Pass_conf]),
                    signup(Req, {error, password, Data})
            end;
        false ->
            ?D("captcha:check > false~n", []),
            signup(Req, {error, captcha, Data})
    end.

%%
%% Возврщает страницу предварающую страницу пользователя
%%
pref(Req) ->
    try
        authorization:auth_required(Req)
    catch
        throw:auth_required -> throw({redirect, "/", []})
    end,
    Xsl_path = "xsl/normal/outside/pref.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% Возврщает страницу пользователя
%%
pers(Req) ->
    try
        authorization:auth_required(Req)
    catch
        throw:auth_required -> throw({redirect, "/", []})
    end,
    Xsl_path = "xsl/normal/outside/pers.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.


captcha(Req) ->
    {CodeHex, BinPng} = captcha:new(),
    throw({cookize, "image/png", mochiweb_cookies:cookie(?CATCHA_COOKIE, CodeHex, ?F_COOKIEOPTIONS), BinPng}).

-define(HTTPC, httpc).

test()->
    % erlang 13 ->
    %   http:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []).
    % erlang 15 ->
    %   httpc:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []).

    % ?HTTPC:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []).

    ok.

test(speed)->
    ok.

