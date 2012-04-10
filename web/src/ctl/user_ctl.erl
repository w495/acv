%%%%  
%%%%
%%%%     Конотроллер для действий от имени пользователя
%%%%

-module(user_ctl).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2]).
-include("../include/web_session.hrl").
-include("../include/common.hrl").


call_after({Req, Result}) ->
    {Req, Result}.



% Пользователь обновляет свою информацию
update_customer_profile(Req) ->
	authorization:auth_required(Req),
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    Pass = proplists:get_value("password", Data, ""),
    case Pass of
        "null" ->
            Pashash = null;
        _ when length(Pass) /= 0 ->
            Pashash = lists:flatten([io_lib:format("~2.16.0B", [X])
                || X <- binary_to_list(erlang:md5(Pass))]);
        _ ->
            Pashash = null
    end,
    E = norm:extr(Data, [{"firstname", [string]},
                         {"lastname", [string]},
                         {"patronimic", [string]}, 
                         {"pic_url", [string]},
                         {"email", [nullable, string]},
                         {"city", [nullable, string]},
                         {"organization", [nullable, string]},
                         {"position", [nullable, string]}]),
 
    io:format("~p~n", [E]),  
    Res = dao:dao_call(dao_customer, update_customer_profile, {E, Pashash, UID}),
        {"application/json", [], [mochijson2:encode(Res)]}.

% Пользователь получает свою информацию
get_customer_profile(Req) ->
	authorization:auth_required(Req),
    #web_session{customer_id=UID} = authorization:auth_required(Req), 
 
    case dao_customer:get_customer(UID) of
        {ok, Val, Vals} ->
            Res1 = db2json:encode(Val),
            Res2 = mochijson2:encode({struct, [{<<"value">>, Res1}, {<<"groups">>, Vals}]});
        {error, E} ->
            Res2 = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Res2]}.
