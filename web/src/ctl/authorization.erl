-module(authorization).
-export([do_login/1, auth_required/1, do_logout/1, do_change_pass/1, login/1]).
-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").

-include("../../common/include/customer.hrl").


-include("web.hrl").
-include("common.hrl").

-define(CATCHA_COOKIE, "captcha_codehex").


-compile(export_all).

auth_required(Req) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined -> throw(auth_required);
        A ->    case auth_biz:get_session(A) of
                    [] -> throw(auth_required);
                    [H|_T] -> H
                end
    end.

get_captcha(_Req) ->
    {CodeHex, BinPng} = captcha:new(),
    throw({cookize, "image/png", cookie(?CATCHA_COOKIE, CodeHex, [{max_age, config:get(expcookie, 18000)}]), BinPng}).

%%%
%%%
%%%

get_customer_id(Req) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined -> Customer_id = undefined;
        A ->    case auth_biz:get_session(A) of
                    [] ->       Customer_id = undefined;
                    [H|_T] ->   #web_session{customer_id=Customer_id} = H
                end
    end,
    ?DEBUG_INFO(?FMT("~p:Customer_id: ~p", [?MODULE, Customer_id])),
    Customer_id.


auth_getlogin(Req) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined -> Login = undefined;
        A ->    case auth_biz:get_session(A) of
                    [] ->       Login = undefined;
                    [H|_T] ->   #web_session{login=Login} = H
                end
    end, Login.

%%%
%%% Возвращает сессию,
%%%     если текущий запрос удовлетворяет правам.
%%% Если не удовлетворяет выкидывает исключение.
%%%
auth_required(Req, Perm) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined -> throw(auth_required);
        A ->
            case auth_biz:get_session(A) of
                [] -> throw(auth_required);
                [H=#web_session{permissions=PList}|_T] -> 
                    case lists:member(Perm, PList) of
                        true -> 
                            H;
                        false -> 
                            ?INFO(?FMT("Permission required: ~p~n",[Perm])),
                            throw({permission_required, Perm})
                    end
            end
    end.

%%%
%%% Возвращает true,
%%%     если текущий запрос удовлетворяет правам.
%%%
auth_if(Req, Perm) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined -> throw(auth_required);
        A ->
            case auth_biz:get_session(A) of
                [] -> throw(auth_required);
                [#web_session{permissions=PList}|_T] ->
                    lists:member(Perm, PList)
            end
    end.


login(Req) ->
    innerLogin(Req, []).

innerLogin(_Req, Params) ->
    io:format("P: ~p~n", [Params]),
    Outty = loginTMPL:render(Params ++ [{owner, config:get(site_owner, "threeline")}]),
%    io:format(""
    {"text/html", [], [Outty]}.

sanit([H|T], Ret) ->
    R = if
        H >= $0, H =< $9 -> Ret ++ [H];
        true -> Ret
    end,
    sanit(T, R);
sanit([], Ret) ->
    Ret.

do_login(Req) ->
    ?DEBUG(?FMT("do_login", [])),
    Data = Req:parse_post(),
    Login = proplists:get_value("login", Data),
    Password = proplists:get_value("password", Data),
    ?DEBUG(?FMT("do_login  login = ~p", [Login])),
    ?DEBUG(?FMT("do_login  password = ~p", [Password])),
    try 
        Val = auth_biz:login(Login, Password),
        throw({ok, {redirect, "/" ++ ?QOOXDOO_BUILD ++ "/index.html",
            [cookie(?AUTHCOOKIE, Val, ?F_COOKIEOPTIONS)]}})
    catch
        throw:{ok, Ret} -> throw(Ret);
        throw:Error -> 
            %csrv:reg_rpc(customerActivityDAO, create, {Login, web, login, Error}),
            innerLogin(Req, [{login, Login}, {error, Error}]);
        T:E ->
            ?DEBUG(?FMT("do_login unknown exception ~p:~p", [T, E])),
            innerLogin(Req, [{login, Login}, {error, "bad_customer"}])
    end.

do_logout(Req) ->
    auth_biz:logout(Req:get_cookie_value(?AUTHCOOKIE)),
    throw({js_redirect, "/login", []}).

checkNumb([H|_T]) when H < 48; H > 57 ->
    throw(bad_consist);
checkNumb([_H|T]) ->
    checkNumb(T);
checkNumb([])->
    ok.

do_change_pass(Req) ->
    Data = Req:parse_qs(),
    OldPassword =  proplists:get_value("oldpassword", Data),
    Password1 = proplists:get_value("password1", Data),
    Password2 = proplists:get_value("password2", Data),
    Res = if 
        length(Password1) -> {struct, [{<<"result">>, <<"bad_length">>}]};
        Password1 =:= Password2 -> 
            try checkNumb(Password1) of
                ok ->
                    #web_session{login=Login} = authorization:auth_required(Req),
                    try auth_biz:login(Login, OldPassword) of
                        _Val -> 
                            auth_biz:change_pass(Login, Password1),
                            {struct, [{<<"result">>, <<"change_done">>}]}
                    catch
                        throw:_ -> {struct, [{<<"result">>, <<"bad_oldpass">>}]}
                    end
            catch throw:bad_consist -> {struct, [{<<"result">>, <<"bad_consist">>}]}
            end;
        true -> {struct, [{<<"result">>, <<"bad_passwords">>}]}
    end,
    {"application/json", [], [mochijson2:encode(Res)]}.

