%%% @file outside.erl
%%%
%%%     Контроллеры, если пользователи пока не заходили
%%%

-module(outside).

-define(UPDATER_ID, 1).

-export([
    hexstring/1,
    index/1,
    about/1,
    docs_index/1,
    % Страница не требуется
	%docs_video/1,
	docs_audience/1,
	docs_content/1,
    docs_howto/1,
    docs_offer/1,
    docs_contact/1,
	docs_contact_send_message/1,
    signin/1,
    signin/2,
    signin_post/1,
    signin_post/2,
    signup/1,
    signup_post/1,
    captcha/1,
    pay/1,
    pay/2,
    pers/1,
    surl/1,
    furl/1,
    curl/1,
    test/0,
    test/1
]).


-import(mochiweb_cookies, [cookie/2]).

-include("web_session.hrl").
-include("common.hrl").
-include("web.hrl").


%%
%% Возврщает список вспомогательной информации для страницы
%%

meta([Req, {'self-retpath', Retpath}|_]) ->
    [
            {"customer_id",         authorization:get_customer_id(Req)},
            {"login",               authorization:auth_getlogin(Req)},
            {"self-retpath",        Retpath},
            {"current-path",        Req:get(path)}
    ];

meta([Req|_]) ->
    [
            {"customer_id",         authorization:get_customer_id(Req)},
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
docs_index(Req) ->
    Xsl_path = "xsl/normal/outside/docs-index.xsl",
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
    Xsl_path = "xsl/normal/outside/docs-video.xsl",
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
    Xsl_path = "xsl/normal/outside/docs-audience.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ), 
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% Документация Контент
%%
docs_content(Req) ->
    Xsl_path = "xsl/normal/outside/docs-content.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ), 
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% Документация HowTo
%%
docs_howto(Req) ->
    Xsl_path = "xsl/normal/outside/docs-howto.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% Документация c расценками
%%
docs_offer(Req) ->
    Xsl_path = "xsl/normal/outside/docs-offer.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% Документация контакты
%%
docs_contact(Req) ->
    Xsl_path = "xsl/normal/outside/docs-contact.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.

%%
%% Отправить сообщение
%%
docs_contact_send_message(Req) -> 
    Request_data = Req:parse_post(), 
    Message_text = proplists:get_value("text", Request_data), 
	%evman_customer:delete(({data, [{"id", YOUR_ID}]}),
    mail_utils:mail({"kirill.a.zhuravlev@gmail.com", "Contact Center"}, "Contact Us Form",  erlang:list_to_binary(Message_text)),
	%% Не обращаем внимание на имя шаблона error ))
    XslPath = "xsl/normal/outside/error.xsl",
    Xml  = xml:encode_data(
        [
            {
				"meta",  [ 	{"errormessage",Message_text} ]
			}            
        ]
    ), 
    ResultHtml = xslt:apply(XslPath, Xml), 
    Req:ok({?OUTPUT_HTML, [], [ResultHtml]}).
	 


signin(Req) ->
    signin(Req, []).

%% @doc
%% Возврaщает страницу регистрации
%%
signin(Req, Retpath) ->
    ?D("~n~nRetpath = ~p~n~n", [Retpath]),
    signin(Req, Retpath, {normal}).

signin(Req, Retpath, State) ->
    Xsl_path = "xsl/normal/outside/signin.xsl",
    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req, {'self-retpath', Retpath}])}             % описание запроса
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.


signin_post(Req) ->
    ?D("signin_post(Req)", []),
    signin_post(Req, []).

%% @doc
%% Выполняет авторизацию
%%
signin_post(Req, Retpath) ->
    ?D("signin_post(Req, Retpath)", []),
    signin_post(Req, Retpath, {normal}).

signin_post(Req, Retpath, State) ->


    ?D("signin_post(Req, Retpath, State)", []),


    Data = Req:parse_post(),
    Login = proplists:get_value("login", Data),
    Password = proplists:get_value("password", Data),
    signin_post(Login, Password, State, Req, Retpath).

signin_post(Login, Password, State, Req, Retpath) ->
    ?D("signin_post(Login, Password, State, Req, Retpath)", []),
    ?D("Login  = ~p~n", [Login]),
    ?D("Password = ~p~n", [Password]),
    try
        Val = auth_biz:login(Login, Password),
        ?D("~n~nRetpath = ~p~n~n", [Retpath]),
        case Retpath of
            [] ->
                throw({ok, {redirect, "/pers",
                    [mochiweb_cookies:cookie(?AUTHCOOKIE, Val, ?F_COOKIEOPTIONS)]}}),
                ok;
            Retpath ->
                throw({ok, {redirect, Retpath,
                    [mochiweb_cookies:cookie(?AUTHCOOKIE, Val, ?F_COOKIEOPTIONS)]}}),
                ok
        end
    catch
        throw:{ok, Ret} -> throw(Ret);
        throw:Error ->
            ?D("Error  = ~p~n", [Error]), 
        	Req:respond({302, [{"Location", "/"}, {"Content-Type", ?OUTPUT_HTML}] ,""});
        Throw:Error ->
            ?D("~p  = ~p~n", [Throw, Error]),
        	Req:respond({302, [{"Location", "/"}, {"Content-Type", ?OUTPUT_HTML}] ,""})
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
                                        {"telephone",      [string]},
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

                            signin_post(Login, Pass, {normal}, Req, []);
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Yb
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%% Возврщает страницу пользователя
%%
pers(Req) -> 
	case authorization:is_auth(Req) of
		false -> 
			signin(Req, Req:get(raw_path));
		_ ->
            case authorization:auth_if(Req, "insider") of
                false ->
                    signin(Req, Req:get(raw_path));
                _ ->
                    Xsl_path = "xsl/normal/outside/pers.xsl",
                    Xml  = xml:encode_data(
                        [
                            {"meta",    meta([Req])}             % описание запроса
                        ]
                    ),
                    Outty = xslt:apply(Xsl_path, Xml),
                    {?OUTPUT_HTML, [], [Outty]}
            end
	end.

%%
%% Возврщает страницу пользователя
%%
surl(Req) ->
    case authorization:is_auth(Req) of
        false ->
            signin(Req, Req:get(raw_path));
        Another ->
            Xsl_path = "xsl/normal/outside/surl.xsl",
            Xml  = xml:encode_data(
                [
                    {"meta",    meta([Req])}
                ]
            ),
            Outty = xslt:apply(Xsl_path, Xml),
            {?OUTPUT_HTML, [], [Outty]}
    end.

%%
%% Возврщает страницу пользователя
%%
furl(Req) ->
    case authorization:is_auth(Req) of
        false ->
            signin(Req, Req:get(raw_path));
        Another ->
            Xsl_path = "xsl/normal/outside/furl.xsl",
            Xml  = xml:encode_data(
                [
                    {"meta",    meta([Req])}
                ]
            ),
            Outty = xslt:apply(Xsl_path, Xml),
            {?OUTPUT_HTML, [], [Outty]}
    end.

%%
%% Возврщает страницу пользователя
%%
curl(Req) ->
    %%%     curl?user_id=1&product_id=006268-0001-0001&amount=10.0&sign=ef03688421e3f9ec816b38f6ee5bf64d1cee3b4a4329294ec2e88f5ea9d80639bbabd8657cd29bcf8cdc4f5bc10efb52de971a9e87635a29e3763f0da
    Xsl_path = "xsl/normal/outside/curl.xsl",

    Data = Req:parse_qs(),
    ?D("Data = ~p", [Data]),
    Acv_video_id = convert:to_integer(proplists:get_value("shop_f1", Data)),

    {ok, [Acv_video], _, _} = dao_acv_video:get_acv_video(Acv_video_id),
    Active = proplists:get_value("active", Acv_video),
    Pay_status = proplists:get_value("pay_status", Acv_video),

    case {Active, Pay_status} of
        {true, false} ->
            dao_acv_video:paybill(Acv_video_id),
            evman_acv_video:paybill({data, Acv_video});
        _ ->
            ok
    end,

    Xml  = xml:encode_data(
        [
            {"meta",    meta([Req])}
        ]
    ),
    Outty = xslt:apply(Xsl_path, Xml),
    {?OUTPUT_HTML, [], [Outty]}.


%%
%% Возврщает страницу пользователя
%%
pay(Req) ->
    Data = Req:parse_qs(),
    ?D("Data = ~p~n", [Data]),
    case proplists:get_value("id", Data) of
        undefined ->
            throw(not_found);
        Rawid ->
            pay(Req, Rawid)
    end.

pay(Req, Rawid) ->

    case authorization:is_auth(Req) of
        false ->
            ?D("Req:get(raw_path) = ~p", [Req:get(raw_path)]),
            signin(Req, Req:get(raw_path));
        Another ->
            try
                Id = convert:to_integer(Rawid),
                authorization:auth_required(Req, % доступ для владельца и admin
                    {fun dao_acv_video:is_owner/2, Id, "admin"}),
                case dao_acv_video:get_acv_video(Id) of
                    {ok, [Acv_video], _, _} ->
                        Acv_video_id = proplists:get_value("id", Acv_video),
                        Customer_id = proplists:get_value("customer_id", Acv_video),
                        Sum = proplists:get_value("sum", Acv_video),
                        Pay_status = proplists:get_value("pay_status", Acv_video),
                        Deleted = proplists:get_value("deleted", Acv_video),

                        case Deleted of
                            true ->
                                Xsl_path = [], throw(not_found);
                            false ->
                                case Pay_status of
                                    false ->
                                        Xsl_path = "xsl/normal/outside/pay.xsl";
                                    true ->
                                        Xsl_path = "xsl/normal/outside/surl.xsl";
                                    null ->
                                        Xsl_path = [], throw(not_found)
                                end
                        end,

                        %shop_f1, shop_f2, shop_f3, shop_f4, shop_f5
                        Payfields = [
                            {"user_id",         Customer_id},
                            {"product_id",      ?SYS_BILL_PRODUCT_ID},
                            {"amount",          Sum},
                            {"shop_id",         ?SYS_BILL_SHOP_ID},
                            {"ps_id",           ?SYS_BILL_PS_ID},
                            {"success_url",     ?SYS_BILL_SURL},
                            {"failure_url",     ?SYS_BILL_FURL},
                            {"shop_f1",         Acv_video_id},
                            {"shop_f2",         "2"},
                            {"shop_f3",         "3"},
                            {"shop_f4",         "4"},
                            {"shop_f5",         "5"},
                            {"secret",          ?SYS_BILL_SECRET}
                        ],

                        Sign = sha512(Payfields),
                        ?D("Sign = ~p~n", [Sign]),

                        Xml  = xml:encode_data(
                            [
                                {"meta",     meta([Req])},             % описание запрос
                                {"pay",      Payfields},             % описание запроса
                                {"sign",     Sign}             % описание запроса
                            ]
                        ),
                        Outty = xslt:apply(Xsl_path, Xml),
                        {?OUTPUT_HTML, [], [Outty]};
                    _ ->
                        throw(not_found)
                end
            catch
                throw:{error,{to_integer,bad_arg,Rowid}} ->
                    throw(not_found);
                throw:not_found ->
                    throw(not_found);
                throw:Error ->
                    ?D("Error  = ~p~n", [Error]),
                    throw(not_found)
            end
    end.

sha512(Proplist)->
    Str = lists:concat(lists:map(fun({Name, Item}) ->
        convert:to_list(Item) end, Proplist)),
    ?D("~nStr = ~p~n", [Str]),
    hexstring(
        sha2:sha512(Str)
    ).

hexstring(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(
        fun(X) -> io_lib:format("~2.16.0b", [X]) end,
        binary_to_list(Binary)));
hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).




captcha(_Req) ->
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

