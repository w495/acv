%%%% \file inside_test.erl
%%%%
%%%%     Контроллеры, активируются, когда пользователь уже зашел в систему
%%%%

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
    Res = dao:dao_call(dao_customer, get_permissions, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

% ============================================================================
% % CUSTOMER_GROUPS
% ============================================================================

get_customer_groups(_Req) ->
    Res = dao:dao_call(dao_customer, get_customer_groups, nil, values),
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
    Res = dao:dao_call(dao_customer, get_customers, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_experts(_Req) ->
    Res = dao:dao_call(dao_customer, getExperts, nil, values),
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
            Pashash = lists:flatten([io_lib:format("~2.16.0B", [X])
                || X <- binary_to_list(erlang:md5(Pass))]);
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
    Res = dao:dao_call(dao_adv_com, getAdvComs, nil, values),
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
    Res = dao:dao_call(dao_adv_com, getBannerPlaces, nil, values),
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
    Res = dao:dao_call(dao_src, getEncoding, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.


%%% ===========================================================================
%%% РЕГОИОНЫ
%%% ===========================================================================

%%%
%%% Возвращает список всех регионов (и страны и города)
%%%
get_all_geo_regions(_Req) ->
    Res = dao:dao_call(dao_geo_region, get_all_geo_regions, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает список всех районов (например СНГ)
%%%
get_geo_areas(_Req) ->
    Res = dao:dao_call(dao_geo_region, get_geo_areas, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.


%%%
%%% Возвращает список всех стран или стран конкретного района
%%%
get_contries(Req) ->
    Data = Req:parse_qs(),
    case proplists:get_value("id", Data) of
        undefined ->
            case proplists:get_value("name_en", Data ) of
            undefined ->
                Res = dao:dao_call(dao_geo_region, get_contries, nil, values);
            Name_en ->
                Res = dao:dao_call(dao_geo_region, get_contries, Name_en, values)
            end;
        Some ->
            Area_id = convert:to_integer(Some),
            Res = dao:dao_call(dao_geo_region, get_contries, Area_id, values)
    end,
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает список стран СНГ
%%%
get_contries_sng(_Req) ->
    Res = dao:dao_call(dao_geo_region, get_contries, "SNG", values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает список всех городов или городов конкретной страны
%%%
get_cities(Req) ->
    Data = Req:parse_qs(),
    case proplists:get_value("id", Data) of
        undefined ->
            case proplists:get_value("name_en", Data ) of
            undefined ->
                Res = dao:dao_call(dao_geo_region, get_cities, [], values);
            Name_en ->
                Res = dao:dao_call(dao_geo_region, get_cities, Name_en, values)
            end;
        Some ->
            Contriy_id = convert:to_integer(Some),
            Res = dao:dao_call(dao_geo_region, get_cities, Contriy_id, values)
    end,
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% ===========================================================================
%%%


%%%
%%% Возвращает полный список рекламных компаний для ВСЕХ ПОЛЬЗОВАТЕЛЕЙ
%%%
get_all_acv_videos(Req) ->
    authorization:auth_required(Req, "admin"),
    Res = dao:dao_call(dao_acv_video, get_all_acv_videos, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает полный спис реклам ДАННОГО ПОЛЬЗОВАТЕЛЯ
%%%
get_acv_videos(Req) ->
    Customer_id = authorization:get_customer_id(Req),
    Res = dao:dao_call(dao_acv_video, get_acv_videos, Customer_id, values),
	io:format("~p~n", [Res]),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает список статистики для всех рекламных компаний
%%%
get_all_acv_video_stats(Req) ->
    authorization:auth_required(Req, "admin"),
    Data = Req:parse_qs(),
    Info = norm:extr(Data , [
        {"fromdate", [datetimeUnixtime]},
        {"todate",   [datetimeUnixtime]}
    ]),
    Res = dao:dao_call(dao_acv_video, get_all_acv_video_stats, Info, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает полный статистики реклам ДАННОГО ПОЛЬЗОВАТЕЛЯ
%%%
get_acv_video_stats(Req) ->
    Customer_id = authorization:get_customer_id(Req),
    Data = Req:parse_qs(),
    Info = norm:extr(Data , [
        {"fromdate", [datetimeUnixtime]},
        {"todate",   [datetimeUnixtime]}
    ]),
    Res = dao:dao_call(dao_acv_video, get_acv_video_stats, {Customer_id, Info}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.


%%%
%%% Возвращает список статистики для конкретной рекламной компании
%%%
get_acv_video_stat(Req) ->
    get_acv_video_stat_by_films(Req).

%%%
%%% Возвращает список статистики ПО ФИЛЬМАМ
%%%      для конкретной рекламной компании
%%%
get_acv_video_stat_by_films(Req) ->
    Data = Req:parse_qs(),
    ?D("~n~nData  = ~p~n~n", [Data]),
    Info = norm:extr(Data , [
        {"fromdate", [datetimeUnixtime]},
        {"todate",   [datetimeUnixtime]},
        {"id",       [nullable,integer]}
    ]),
    ?D("~n~nInfo   = ~p~n~n", [Info]),
    Res = dao:dao_call(dao_stat, get_acv_video_stat_by_films, Info, values),
    ?D("Res  = ~p~n", [Res]),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает список статистики ПО ФИЛЬМАМ
%%%      для конкретной рекламной компании
%%%
get_acv_video_stat_by_film(Req) ->
    Data = Req:parse_qs(),
    ?D("~n~nData  = ~p~n~n", [Data]),
    Info = norm:extr(Data , [
        {"fromdate",    [datetimeUnixtime]},
        {"todate",      [datetimeUnixtime]},
        {"parent_id",   [integer]},
        {"video_url",   [string]}
    ]),
    ?D("~n~nInfo   = ~p~n~n", [Info]),
    Res = dao:dao_call(dao_stat, get_acv_video_stat_by_film, Info, values),
    ?D("Res  = ~p~n", [Res]),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%----------------------------------------------------------------------------


%%%
%%% Возвращает полный спис реклам для баннеров
%%%
get_all_acv_banners(Req) ->
    authorization:auth_required(Req, "admin"),
    Res = dao:dao_call(dao_acv_banner, get_all_acv_banners, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%----------------------------------------------------------------------------


%%%
%%% Возвращает полный спис категорий
%%%
get_all_cats(Req) ->
    Customer_id = authorization:get_customer_id(Req), % TODO зачем эта переменная?
    Res = mysql_dao:dao_call(dao_cat, get_all_cats, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает полный спис реклам ДАННОГО ПОЛЬЗОВАТЕЛЯ
%%%
get_acv_banners(Req) ->
    Customer_id = authorization:get_customer_id(Req),
    Res = dao:dao_call(dao_acv_banner, get_acv_banners, Customer_id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_acv_video_common(Req) ->
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    Res = dao:dao_value(dao_acv_video, get_acv_video_common, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_upload(Req) ->
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    Res = dao:dao_value(dao_acv_video, get_acv_video_upload, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_show(Req) ->
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    Res = dao:dao_value(dao_acv_video, get_acv_video_show, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_users(Req) ->
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    Res = dao:dao_value(dao_acv_video, get_acv_video_users, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_geos(Req) ->
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    Res = dao:dao_values(dao_acv_video, get_acv_video_geos, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_cats(Req) ->
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    Res = dao:dao_values(dao_acv_video, get_acv_video_cats, Acv_video_id),
    {"application/json", [], [Res]}.

%%%
%%% Изменяет рекламу для роликов
%%%
update_acv_video(Req) ->
    Customer_id = authorization:get_customer_id(Req),
    Data = Req:parse_post(),
    ?D("~n----------------------------------------------------------------~nData  = ~p", [Data ]),
    Geo_region_list =
        [convert:to_integer(X) || X <- proplists:get_all_values("geo_list", Data)],
    Cat_list =
        [convert:to_integer(X) || X <- proplists:get_all_values("cat_list", Data)],
    ?D("~n---------------------~nGeo_region_list = ~p", [Geo_region_list]),
    ?D("~n---------------------~nCat_list = ~p", [Cat_list]),

    % 
    % update_acv_video({null, Name, Datestart, Datestop, Url, Ref, Wish,
    %   Postroll, Preroll, Midroll, Pauseroll, User_male,
    %       Age_from, Age_to, Time_from, Time_to, Customer_id}) ->
    %

    Info_0 = norm:extr(Data, [
        %{"id",          [nullable, integer]},
        {"name",        [string]},
        {"datestart",   [datetimeUnixtime]},
        {"datestop",    [datetimeUnixtime]},
        {"url",         [string]},
        {"ref",         [string]},
        {"wish",        [integer]},
        {"postroll",    [boolean]},
        {"preroll",     [boolean]},
        {"midroll",     [boolean]},
        {"pauseroll",   [boolean]},
        {"user_male",   [boolean, nullable]},
        {"age_from",    [integer, nullable]},
        {"age_to",      [integer, nullable]},
        {"time_from",   [integer, nullable]},
        {"time_to",     [integer, nullable]},
        {"duration",    [integer, nullable]},
        {"link_title",  [string, nullable]},
        {"alt_title",   [string, nullable]},
        {"comment",           [string, nullable]},
        {"rerun_hours",       [integer, nullable]},
        {"rerun_minutes",     [integer, nullable]}
    ]),

    Info_1 = utils:append_to_tuple(Info_0, {[null], [Customer_id]}),

    Res = dao:dao_call(dao_acv_video, update_acv_video,
        {Info_1, Geo_region_list, Cat_list}, values),

    % Кидаем событие о создании кампании
    % gen_event:notify(?ACVVID_EVENT, Data),

    {"application/json", [], [mochijson2:encode(Res)]}.

start_acv_video(Req) ->
    Data = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Data)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),

    Res = dao:dao_call(dao_acv_video, start_acv_video, Id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

stop_acv_video(Req) ->
    Data = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Data)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),

    Res = dao:dao_call(dao_acv_video, stop_acv_video, Id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.


delete_acv_video(Req) ->
    Data = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Data)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),

    Res = dao:dao_call(dao_acv_video, delete_acv_video, Id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

activate_acv_video(Req) ->
    authorization:auth_required(Req, "admin"),
    Data = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Data)),
    Res = dao:dao_call(dao_acv_video, activate_acv_video, Id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

disactivate_acv_video(Req) ->
    authorization:auth_required(Req, "admin"),
    Data = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Data)),
    Res = dao:dao_call(dao_acv_video, disactivate_acv_video, Id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

full_delete_acv_video(Req) ->
    Data = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Data)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),

    Res = dao:dao_call(dao_acv_video, full_delete_acv_video, Id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.


get_acv_video(Req) ->
    Data = Req:parse_qs(),
    Id = convert:to_integer(proplists:get_value("id", Data)),
    case dao_acv_video:get_acv_video(Id) of
        {ok, ACV, CatList, GeoList} ->
            Res1 = db2json:encode(ACV),
            Res2 = db2json:encode(CatList, "values"),
            Res3 = db2json:encode(GeoList, "values"),
            Res = mochijson2:encode({struct, [{<<"value">>, Res1}, {<<"cats">>, Res2}, {<<"geo">>, Res3}]});
        {error, E} -> Res = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Res]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% КОНФИГУРАЦИЯ СИСТЕМЫ
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% Возвращает конфигурацию системы,
%%%
get_config(Req) ->
    authorization:auth_required(Req, "admin"),

    % Оставлено для совместимости
    Data = Req:parse_qs(),
    Id = convert:to_integer(proplists:get_value("id", Data)),
    Res = dao:dao_call(dao_config, get_config, Id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Изменяет конфигурацию системы,
%%%
update_config(Req) ->
    authorization:auth_required(Req, "admin"),
    Data = Req:parse_post(),

    Info = norm:extr(Data, [
        {"id",          [nullable, integer]},   % Оставлено для совместимости
        {"acv_video_loadnext",        [integer]}%,
    ]),

    Res = dao:dao_call(dao_config, update_config, Info, values),
    {"application/json", [], [mochijson2:encode(Res)]}.


test()->
    % erlang 13 ->
    %   http:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []).
    % erlang 15 ->
    %   httpc:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []).
    %?HTTPC:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []),

    ok.

test(speed) ->

    ok.
