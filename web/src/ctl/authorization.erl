-module(authorization).
-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("web_session.hrl").
-include("customer.hrl").

-include("web.hrl").
-include("common.hrl").

-compile(export_all).

-define(LOGOUT, "/").


%
% Возвращает true если пользователь авторизован
%
is_auth(Request) ->
    Cookie = Request:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined -> 
			false;
        A ->
            case auth_biz:get_session(A) of
                [] -> 
					false;
                [_H=#web_session{permissions=PList}|_T] -> 
					true
            end
    end.

has_perm(Req, Perm) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined ->
            false;
        A ->    case auth_biz:get_session(A) of
                    [] ->
                        false;
                    [H|_T] ->
                        #web_session{customer_id=Customer_id} = H,
                        dao_customer:has_perm(Customer_id, Perm)
                end
    end.


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
%    ?DEBUG_INFO(?FMT("~p:Customer_id: ~p", [?MODULE, Customer_id])),
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


auth_required_front(Req) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined ->
            throw(auth_required_front);
        A ->    case auth_biz:get_session(A) of
                    [] ->
                        throw(auth_required_front);
                    [H|_T] -> H
                end
    end.

auth_required(Req) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined ->
            %%% error:return_json(Req, "Auth Required");

            %%% --------------------------------------------------------------
            %%%
            %%% Идея хорошая, только не сильно применимая
            %%%     потому, что совсем по-хорошему
            %%%     надо как-то прерывать исполнение,
            %%%         для этого и вызываем исключение.
            %%% Если хочешь вернуть специцифичное сообщение для ощибки,
            %%%        попробуй прокинуть его через обработку ошибок в web_web
            %%%
            %%% ~~---- w-495
            %%% --------------------------------------------------------------

            throw(auth_required);
        A ->    case auth_biz:get_session(A) of
                    [] ->
                        %error:return_json(Req, "Auth Required");
                        throw(auth_required);
                    [H|_T] -> H
                end
    end.

%%%
%%% Возвращает сессию,
%%%     если текущий запрос удовлетворяет правам.
%%% В данном случае используется некоторый костыл,
%%% для определения прав доступа к acv_video
%%% Доступ предоставляется или владельцу Instance,
%%% или пользователю с правами Alt_perm.
%%% Если не удовлетворяет выкидывает исключение.
%%% 
%%% Is_owner_fun(Customer_id, Obj) -> true | false
%%%

auth_required(Req, {Is_owner_fun, Obj, Perm}) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined ->
			%error:return_json(Req, "Auth Required");
            throw(auth_required);
        A ->
            case auth_biz:get_session(A) of
                [] -> 
					%error:return_json(Req, "Auth Required");
					throw(auth_required);
                [H = #web_session{customer_id = Customer_id, permissions=PList}|_T] ->
                    case Is_owner_fun(Customer_id, Obj) of
                        true -> H;
                        false ->
                            case lists:member(Perm, PList) of
                                true -> H;
                                false ->
                                    ?INFO(?FMT("Permission required: ~p~n",[Perm])),
									%error:return_json(Req, "Permission Required")
                                    throw({permission_required, Perm})
                            end
                    end
            end
    end;

%%%
%%% Возвращает сессию,
%%%     если текущий запрос удовлетворяет правам.
%%% Если не удовлетворяет выкидывает исключение.
%%%
auth_required(Req, Perm) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined -> 
			%error:return_json(Req, "Auth Required");
			throw(auth_required);
        A ->
            case auth_biz:get_session(A) of
                [] -> 
					%error:return_json(Req, "Auth Required");
                    throw(auth_required);
                [H=#web_session{permissions=PList}|_T] -> 
                    case lists:member(Perm, PList) of
                        true -> 
                            H;
                        false -> 
                            ?INFO(?FMT("Permission required: ~p~n",[Perm])), 
							%error:return_json(Req, "Permission Required")
                            throw({permission_required, Perm})
                    end
            end
    end.

%%%
%%% Возвращает true,
%%%     если текущий запрос удовлетворяет правам.
%%%
%%%
%%%
auth_if(Req, Perm) ->
    Cookie = Req:get_cookie_value(?AUTHCOOKIE),
    case Cookie of
        undefined -> 
			%error:return_json(Req, "Auth Required");
			throw(auth_required);
        A ->
            case auth_biz:get_session(A) of
                [] -> 
					%error:return_json(Req, "Auth Required");
					throw(auth_required);
                [#web_session{permissions=PList}|_T] ->
                    ?D("~nPerm = ~p ~n", [Perm]),
                    ?D("~nPList = ~p ~n", [PList]),
                    lists:member(Perm, PList)
            end
    end.

do_logout(Req) ->
    auth_biz:logout(Req:get_cookie_value(?AUTHCOOKIE)),
    throw({js_redirect, ?LOGOUT, []}).

do_logout_with_redirect(Req) ->
    auth_biz:logout(Req:get_cookie_value(?AUTHCOOKIE)),
    throw({redirect, ?LOGOUT, []}).


%%%
%%% @Depricated
%%%

% login(Req) ->
%     innerLogin(Req, []).
% 
% innerLogin(_Req, Params) ->
%     io:format("P: ~p~n", [Params]),
%     Outty = loginTMPL:render(Params ++ [{owner, config:get(site_owner, "threeline")}]), 
% %    io:format(""
%     {"text/html", [], [Outty]}.
% 
% sanit([H|T], Ret) ->
%     R = if
%         H >= $0, H =< $9 -> Ret ++ [H];
%         true -> Ret
%     end,
%     sanit(T, R);
% sanit([], Ret) ->
%     Ret.
% 
% do_login(Req) ->
%     ?DEBUG(?FMT("do_login", [])),
%     Data = Req:parse_post(),
%     Login = proplists:get_value("login", Data),
%     Password = proplists:get_value("password", Data),
%     ?DEBUG(?FMT("do_login  login = ~p", [Login])),
%     ?DEBUG(?FMT("do_login  password = ~p", [Password])), 
%     try 
%         Val = auth_biz:login(Login, Password),
%         throw({ok, {redirect, "/" ++ ?QOOXDOO_BUILD ++ "/index.html",
%             [cookie(?AUTHCOOKIE, Val, ?F_COOKIEOPTIONS)]}})
%     catch
%         throw:{ok, Ret} -> throw(Ret);
%         throw:Error ->  
%             %csrv:reg_rpc(customerActivityDAO, create, {Login, web, login, Error}),
%             innerLogin(Req, [{login, Login}, {error, Error}]),
% 			io:format("asfasdfsdfsdfsdfsdfsdf");
%         T:E -> 
%             ?DEBUG(?FMT("do_login unknown exception ~p:~p", [T, E])),
%             innerLogin(Req, [{login, Login}, {error, "bad_customer"}])
%     end.
%
% checkNumb([H|_T]) when H < 48; H > 57 ->
%     throw(bad_consist);
% checkNumb([_H|T]) ->
%     checkNumb(T);
% checkNumb([])->
%     ok.
% 
% do_change_pass(Req) ->
% 	auth_required(Req),
%     Data = Req:parse_qs(),
%     OldPassword =  proplists:get_value("oldpassword", Data),
%     Password1 = proplists:get_value("password1", Data),
%     Password2 = proplists:get_value("password2", Data),
%     Res = if 
%         length(Password1) -> {struct, [{<<"result">>, <<"bad_length">>}]};
%         Password1 =:= Password2 -> 
%             try checkNumb(Password1) of
%                 ok ->
%                     #web_session{login=Login} = authorization:auth_required(Req),
%                     try auth_biz:login(Login, OldPassword) of
%                         _Val -> 
%                             auth_biz:change_pass(Login, Password1),
%                             {struct, [{<<"result">>, <<"change_done">>}]}
%                     catch
%                         throw:_ -> {struct, [{<<"result">>, <<"bad_oldpass">>}]}
%                     end
%             catch throw:bad_consist -> {struct, [{<<"result">>, <<"bad_consist">>}]}
%             end;
%         true -> {struct, [{<<"result">>, <<"bad_passwords">>}]}
%     end,
%     {"application/json", [], [mochijson2:encode(Res)]}.

