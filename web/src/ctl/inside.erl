%%% \file inside.erl
%%%
%%%     Контроллеры, активируются, когда пользователь уже зашел в систему
%%%

-module(inside).
-compile(export_all).


-export([
    %call_before/1,
        call_after/1,
    % PERMISSIONS
        get_permissions/1,
    % CUSTOMER_GROUPS
        get_customer_groups/1,
        get_customer_group_info/1,
        update_customer_group/1,
        delete_customer_group/1,
    % CUSTOMERS
        get_customers/1,
        get_customer_info/1,
        update_customer/1,
        delete_customer/1]).
    % DIR


-import(mochiweb_cookies, [cookie/2]).
-include("../include/web_session.hrl").
-include("../include/common.hrl").


call_after({Req, Result}) ->
    {Req, Result}.

% ============================================================================
% % PERMISSIONS
% ============================================================================

get_permissions(_Req) ->
    Res = dao:dao_call(dao_customer, get_permissions, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

% ============================================================================
% % CUSTOMER_GROUPS
% ============================================================================

get_customer_groups(_Req) ->
    Res = dao:dao_call(dao_customer, get_customer_groups, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_customer_group_info(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),

    io:format("Id = ~p~n", [Id]),

    case dao_customer:get_customer_group(Id) of
        {ok, Val, Perms} ->
            Res1 = db2json:encode(Val),
            Res2 = mochijson2:encode({struct, [{<<"value">>, Res1}, {<<"permissions">>, Perms}]});
        {error, E} -> Res2 = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Res2]}.

update_customer_group(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    PermissionList = [utils:to_integer(X) || X <- proplists:get_all_values("permissions", Data)],

    E = norm:extr(Data, [{"id", [nullable, integer]}, 
                         {"name", [string]},
                         {"description", [string]}]),
    Res = dao:dao_call(dao_customer, update_customer_group, {E, PermissionList, UID}),
    {"application/json", [], [mochijson2:encode(Res)]}.

delete_customer_group(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:dao_call(dao_customer, delete_customer_group, {Id, UID}),
    {"application/json", [], [mochijson2:encode(Res)]}.

% ============================================================================
% % CUSTOMER
% ============================================================================

get_customers(_Req) ->
    Res = dao:dao_call(dao_customer, get_customers, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_experts(_Req) ->
    Res = dao:dao_call(dao_customer, getExperts, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_customer_info(Req) ->
    Data = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Data = [{"key", "value"}, ... ]

    Id = utils:to_integer(proplists:get_value("id", Data)),
    case dao_customer:get_customer(Id) of
        {ok, Val, Vals} ->
            Res1 = db2json:encode(Val),
            Res2 = mochijson2:encode({struct, [{<<"value">>, Res1}, {<<"groups">>, Vals}]});
        {error, E} ->
            Res2 = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Res2]}.

update_customer(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    Pass = proplists:get_value("password", Data, ""),
    case Pass of
        "null" ->
            Pashash = null;
        _ when length(Pass) /= 0 ->
            Pashash = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(erlang:md5(Pass))]);
        _ ->
            Pashash = null
    end,
    E = norm:extr(Data, [{"id", [nullable, integer]},
                         {"firstname", [string]},
                         {"lastname", [string]},
                         {"patronimic", [string]},
                         {"login", [string]},
                         {"pic_url", [string]},
                         {"email", [nullable, string]},
                         {"city", [nullable, string]},
                         {"organization", [nullable, string]},
                         {"position", [nullable, string]}]),

    io:format("~p~n", [E]),
    Group_list = [utils:to_integer(X) || X <- proplists:get_all_values("groups", Data)],
    Res = dao:dao_call(dao_customer, update_customer, {E, Pashash, Group_list, UID}),
        {"application/json", [], [mochijson2:encode(Res)]}.

delete_customer(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),

    Data = Req:parse_post(),
    % % помни parse_post --- работает с POST
    % % Data = [{"key", "value"}, ... ]

    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:dao_call(dao_customer, delete_customer, {Id, UID}),
        {"application/json", [], [mochijson2:encode(Res)]}.



get_adv_coms(_Req) ->
    Res = dao:dao_call(dao_adv_com, getAdvComs, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_adv_com(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:dao_call(dao_adv_com, getAdvCom, Id),
    {"application/json", [], [mochijson2:encode(Res)]}.

update_adv_com(Req) ->
    Data = Req:parse_post(),
    Info = norm:extr(Data, [
        {"id", [nullable, integer]},
        {"name", [string]},
        {"ref", [string]},
        {"datestart", [datetimeUnixtime]},
        {"datestop", [datetimeUnixtime]},
        {"banner_place_id", [integer]},
        {"pic_url", [string]}
    ]),
    Res = dao:dao_call(dao_adv_com, updateAdvCom, Info),
    {"application/json", [], [mochijson2:encode(Res)]}.

delete_adv_com() ->
    test1:est().

get_banner_places(_Req) ->
    Res = dao:dao_call(dao_adv_com, getBannerPlaces, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.



run(Cmd, Timeout) ->
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    loop(Port,[], Timeout).

loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Port, Data++NewData, Timeout);
        {Port, {exit_status, 0}} -> Data;
        {Port, {exit_status, S}} -> throw({commandfailed, S})
    after Timeout ->
        throw(timeout)
end.

get_encoding(_Req) ->
    Res = dao:dao_call(dao_src, getEncoding, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.


get_adv_coms_vid(_Req) ->
    Res = dao:dao_call(dao_adv_com, getAdvComsVid, [], values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_adv_com_vid(Req) ->
    Data = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    Res = dao:dao_call(dao_adv_com, getAdvComVid, Id),
    {"application/json", [], [mochijson2:encode(Res)]}.

update_adv_com_vid(Req) ->
    Data = Req:parse_post(),
    Info = norm:extr(Data, [
        {"id", [nullable, integer]},
        {"name", [string]},
        {"ref", [string]},
        {"datestart", [datetimeUnixtime]},
        {"datestop", [datetimeUnixtime]},
%        {"banner_place_id", [integer]},
        {"pic_url", [string]}
    ]),
    Res = dao:dao_call(dao_adv_com, updateAdvComVid, Info),
    {"application/json", [], [mochijson2:encode(Res)]}.


