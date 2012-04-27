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

get_permissions(Req) ->
%%	authorization:auth_required(Req, "admin"),
    authorization:auth_required(Req),
    Res = dao:dao_call(dao_customer, get_permissions, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

% ============================================================================
% % CUSTOMER_GROUPS
% ============================================================================

get_customer_groups(Req) -> 
	authorization:auth_required(Req, "admin"),
    Res = dao:dao_call(dao_customer, get_customer_groups, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_customer_group_info(Req) ->
	authorization:auth_required(Req, "admin"),

    Plfields = Req:parse_qs(),
    Id = utils:to_integer(proplists:get_value("id", Plfields)),
    io:format("Id = ~p~n", [Id]),
    case dao_customer:get_customer_group(Id) of
        {ok, Val, Perms} ->
            Res1 = db2json:encode(Val),
            Res2 = mochijson2:encode({struct, [{<<"value">>, Res1}, {<<"permissions">>, Perms}]});
        {error, E} -> Res2 = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Res2]}.

update_customer_group(Req) ->
	authorization:auth_required(Req, "admin"),
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Plfields = Req:parse_post(),
    PermissionList = [utils:to_integer(X) || X <- proplists:get_all_values("permissions", Plfields)],

    E = norm:extr(Plfields, [{"id", [nullable, integer]},
                         {"name", [string]},
                         {"description", [string]}]),
    Res = dao:dao_call(dao_customer, update_customer_group, {E, PermissionList, UID}),
    {"application/json", [], [mochijson2:encode(Res)]}.

delete_customer_group(Req) ->
	authorization:auth_required(Req, "admin"),
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Plfields = Req:parse_post(),
    Id = utils:to_integer(proplists:get_value("id", Plfields)),
    Res = dao:dao_call(dao_customer, delete_customer_group, {Id, UID}),
    {"application/json", [], [mochijson2:encode(Res)]}.

% ============================================================================
% % CUSTOMER
% ============================================================================

get_customers(Req) ->
	authorization:auth_required(Req, "admin"),
    Res = dao:dao_call(dao_customer, get_customers, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_experts(Req) ->
	authorization:auth_required(Req, "admin"),
    Res = dao:dao_call(dao_customer, getExperts, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_customer_info(Req) ->
	authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_qs(),
    % % помни parse_post --- работает с GET
    % % Plfields = [{"key", "value"}, ... ]

    Id = utils:to_integer(proplists:get_value("id", Plfields)),
    case dao_customer:get_customer(Id) of
        {ok, Val, Vals} ->
            Res1 = db2json:encode(Val),
            Res2 = mochijson2:encode({struct, [{<<"value">>, Res1}, {<<"groups">>, Vals}]});
        {error, E} ->
            Res2 = toBiz:mkErrorJson(E)
    end,
    {"application/json", [], [Res2]}.

update_customer(Req) ->
	authorization:auth_required(Req, "admin"),
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Plfields = Req:parse_post(),
    Pass = proplists:get_value("password", Plfields, ""),
    case Pass of
        "null" ->
            Pashash = null;
        _ when length(Pass) /= 0 ->
            Pashash = lists:flatten([io_lib:format("~2.16.0B", [X])
                || X <- binary_to_list(erlang:md5(Pass))]);
        _ ->
            Pashash = null
    end,
    E = norm:extr(Plfields, [
        {"id", [nullable, integer]},
        {"firstname", [string]},
        {"lastname", [string]},
        {"patronimic", [string]},
        {"login", [string]},
        {"telephone_number", [string]},
        {"pic_url", [string]},
        {"email", [nullable, string]},
        {"city", [nullable, string]},
        {"organization", [nullable, string]},
        {"position", [nullable, string]}
    ]),

    Id = convert:to_integer(proplists:get_value("id", Plfields, null)),

    Group_list =
        [
            utils:to_integer(X)
            || X <- proplists:get_all_values("groups", Plfields)
        ],

    {ok, _, Oldperms} = dao_customer:get_customer_perm({id, Id}),

    %%%
    %%% Вызываем dao_call
    %%%     с вызовом
    %%%     dao_customer:update_customer({E, Pashash, Group_list, UID})
    %%%     и callback определенным через fun(_)->
    %%%
    Res = dao:dao_call(
        dao_customer,
        update_customer,
        {E, Pashash, Group_list, UID},
        undefined,
        fun(Fr) ->
            {ok, [Custdata], Newperms} = dao_customer:get_customer_perm({id, Id}),
            ?D("----------------------------~n", []),
            ?D("Res = ~p~n", [Fr]),
            ?D("----------------------------~n", []),
            lists:foreach(fun (Oldperm) ->
                case lists:member(Oldperm, Newperms) of
                    false  ->
                        evman_customer:del_perm({
                            convert:to_atom(Oldperm),
                            {data, Custdata}
                        }),
                        ok;
                    _ ->
                        ok
                end
            end, Oldperms),
            lists:foreach(fun (Newperm) ->
                case lists:member(Newperm, Oldperms) of
                    false ->
                        evman_customer:add_perm({
                            convert:to_atom(Newperm),
                            {data, Custdata}
                        }),
                        ok;
                    _ ->
                        ok
                end
            end, Newperms)
        end
    ),

    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% @doc
%%%     Вызывает событие о том,
%%%         что пользователь стал авторизованным
%%% 
% customer_newperm(Cust, "insider") ->
%     ?I("Customer ~p is insider ~n", [proplists:get_value("id", Cust)]),
%     evman_customer:insider(true, Cust),
%     ok;
% 
% customer_newperm(Cust, Perm) ->
%     Id = proplists:get_value("id", Cust),
%     ?I("Customer ~p has permission ~p ~n", [Id, Perm]),
%     ok.
% 
% customer_oldperm(Cust, "insider") ->
%     ?I("Customer ~p is NOT insider ~n", [proplists:get_value("id", Cust)]),
%     evman_customer:insider(false, Cust),
%     ok;
% 
% customer_oldperm(Cust, Perm) ->
%     Id = proplists:get_value("id", Cust),
%     ?I("Customer ~p looses permission ~p ~n", [Id, Perm]),
%     ok.


delete_customer(Req) ->
	authorization:auth_required(Req, "admin"),
    #web_session{customer_id=UID} = authorization:auth_required(Req),

    Plfields = Req:parse_post(),
    % % помни parse_post --- работает с POST
    % % Plfields = [{"key", "value"}, ... ]

    Id = utils:to_integer(
        proplists:get_value("id", Plfields)
    ),
    Res = dao:dao_call(
        dao_customer,
        delete_customer,
        {Id, UID},
        undefined,
        fun(Fr) ->
            evman_customer:delete({data, [{"id", Id}]})
        end
    ),
    {"application/json", [], [mochijson2:encode(Res)]}.

%get_adv_coms(_Req) ->
%    Res = dao:dao_call(dao_adv_com, getAdvComs, nil, values),
%    {"application/json", [], [mochijson2:encode(Res)]}.

%get_adv_com(Req) ->
%    Plfields = Req:parse_qs(),
%    Id = utils:to_integer(proplists:get_value("id", Plfields)),
%    Res = dao:dao_call(dao_adv_com, getAdvCom, Id),
%    {"application/json", [], [mochijson2:encode(Res)]}.

%update_adv_com(Req) ->
%    Plfields = Req:parse_post(),
%    Info = norm:extr(Plfields, [
%        {"id", [nullable, integer]},
%        {"name", [string]},
%        {"ref", [string]},
%        {"datestart", [datetimeUnixtime]},
%        {"datestop", [datetimeUnixtime]},
%        {"banner_place_id", [integer]},
%        {"pic_url", [string]}
%    ]),
%    Res = dao:dao_call(dao_adv_com, updateAdvCom, Info),
%    {"application/json", [], [mochijson2:encode(Res)]}.

%delete_adv_com() ->
%    test1:est().

%get_banner_places(_Req) ->
%    Res = dao:dao_call(dao_adv_com, getBannerPlaces, nil, values),
%    {"application/json", [], [mochijson2:encode(Res)]}.



run(Cmd, Timeout) ->
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    loop(Port,[], Timeout).

loop(Port, Plfields, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Port, Plfields++NewData, Timeout);
        {Port, {exit_status, 0}} -> Plfields;
        {Port, {exit_status, S}} -> throw({commandfailed, S})
    after Timeout ->
        throw(timeout)
end.

%get_encoding(_Req) ->
%    Res = dao:dao_call(dao_src, getEncoding, nil, values),
%    {"application/json", [], [mochijson2:encode(Res)]}.


%%% ===========================================================================
%%% РЕГОИОНЫ
%%% ===========================================================================

%%%
%%% Возвращает список всех регионов (и страны и города)
%%%
get_all_geo_regions(Req) ->
	authorization:auth_required(Req),
    Res = dao:dao_call(dao_geo_region, get_all_geo_regions, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает список всех районов (например СНГ)
%%%
get_geo_areas(Req) ->
	authorization:auth_required(Req),
    Res = dao:dao_call(dao_geo_region, get_geo_areas, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.


%%%
%%% Возвращает список всех стран или стран конкретного района
%%%
get_contries(Req) ->
	authorization:auth_required(Req),
    Plfields = Req:parse_qs(),
    case proplists:get_value("id", Plfields) of
        undefined ->
            case proplists:get_value("name_en", Plfields ) of
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
get_contries_sng(Req) ->
	authorization:auth_required(Req),
    Res = dao:dao_call(dao_geo_region, get_contries, "SNG", values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает список всех городов или городов конкретной страны
%%%
get_cities(Req) ->
	authorization:auth_required(Req),
    Plfields = Req:parse_qs(),
    case proplists:get_value("id", Plfields) of
        undefined ->
            case proplists:get_value("name_en", Plfields ) of
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
	authorization:auth_required(Req),
    Customer_id = authorization:get_customer_id(Req),
    Res = dao:dao_call(dao_acv_video, get_acv_videos, Customer_id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает список статистики для всех рекламных компаний
%%%
get_all_acv_video_stats(Req) ->
    authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_qs(),
    Info = norm:extr(Plfields , [
        {"fromdate", [datetimeUnixtime]},
        {"todate",   [datetimeUnixtime]}
    ]),
    Res = dao:dao_call(dao_acv_video, get_all_acv_video_stats, Info, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает полный статистики реклам ДАННОГО ПОЛЬЗОВАТЕЛЯ
%%%
get_acv_video_stats(Req) ->
	authorization:auth_required(Req),
    Customer_id = authorization:get_customer_id(Req),
    Plfields = Req:parse_qs(),
    Info = norm:extr(Plfields , [
        {"fromdate", [datetimeUnixtime]},
        {"todate",   [datetimeUnixtime]}
    ]),
    Res = dao:dao_call(
        dao_acv_video,
        get_acv_video_stats,
        {Customer_id, Info},
        values
    ),
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
%%	authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_qs(),
    ?D("~n~nData  = ~p~n~n", [Plfields]),
    Info = norm:extr(Plfields , [
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
%%	authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_qs(),
    ?D("~n~nData  = ~p~n~n", [Plfields]),
    Info = norm:extr(Plfields , [
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
	authorization:auth_required(Req),
    Customer_id = authorization:get_customer_id(Req), % TODO зачем эта переменная?
    Res = mysql_dao:dao_call(dao_cat, get_all_cats, nil, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает полный спис реклам ДАННОГО ПОЛЬЗОВАТЕЛЯ
%%%
get_acv_banners(Req) ->
    authorization:auth_required(Req, "admin"),
    Customer_id = authorization:get_customer_id(Req),
    Res = dao:dao_call(dao_acv_banner, get_acv_banners, Customer_id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_acv_video_common(Req) -> 
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Acv_video_id, "admin"}),
    Res = dao:dao_value(dao_acv_video, get_acv_video_common, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_upload(Req) -> 
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Acv_video_id, "admin"}),
    Res = dao:dao_value(dao_acv_video, get_acv_video_upload, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_show(Req) -> 
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Acv_video_id, "admin"}),
    Res = dao:dao_value(dao_acv_video, get_acv_video_show, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_users(Req) -> 
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Acv_video_id, "admin"}),
    Res = dao:dao_value(dao_acv_video, get_acv_video_users, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_geos(Req) -> 
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Acv_video_id, "admin"}),
    Res = dao:dao_values(dao_acv_video, get_acv_video_geos, Acv_video_id),
    {"application/json", [], [Res]}.

get_acv_video_cats(Req) -> 
    Acv_video_id = convert:to_integer(proplists:get_value("id", Req:parse_qs())),
    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Acv_video_id, "admin"}),
    Res = dao:dao_values(dao_acv_video, get_acv_video_cats, Acv_video_id),
    {"application/json", [], [Res]}.

%%%
%%% Изменяет рекламу для роликов
%%%
update_acv_video(Req) ->
	%
	% Если данная функция будет работать как функция изменения информации о видео,
	% то необходимо будет заменить строку "authorization:auth_required(Req)" на строку
	% "authorization:auth_required(Req,{fun dao_acv_video:is_owner/2, Acv_video_id, "admin"}),"
 	% где Acv_video_id - идентификатор для видео, информацию о котором необхордимо изменить
	%
	authorization:auth_required(Req),
    Customer_id = authorization:get_customer_id(Req),
    Plfields = Req:parse_post(),
    Geo_region_list =
        [convert:to_integer(X) || X <- proplists:get_all_values("geo_list", Plfields)],
    Cat_list =
        [convert:to_integer(X) || X <- proplists:get_all_values("cat_list", Plfields)],

    % 
    % update_acv_video({null, Name, Datestart, Datestop, Url, Ref, Wish,
    %   Postroll, Preroll, Midroll, Pauseroll, User_male,
    %       Age_from, Age_to, Time_from, Time_to, Customer_id}) ->
    %

    Info_0 = norm:extr(Plfields, [
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

    Res = dao:dao_call(
        dao_acv_video,
        update_acv_video,
        {Info_1, Geo_region_list, Cat_list},
        values,
        fun({ok, Id})->
            %%% 
            %%% В callback кидаем событие о создании кампании
            %%%
            evman_acv_video:create({data, [{"id", Id}]})
        end
    ),

    {"application/json", [], [mochijson2:encode(Res)]}.

start_acv_video(Req) ->
    Plfields = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Plfields)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),

    Res = dao:dao_call(
        dao_acv_video,
        start_acv_video,
        Id,
        values,
        fun(_) ->
            evman_acv_video:start({data, [{"id", Id}]})
        end
    ),
    {"application/json", [], [mochijson2:encode(Res)]}.

stop_acv_video(Req) ->
    Plfields = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Plfields)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),

    Res = dao:dao_call(
        dao_acv_video,
        stop_acv_video,
        Id,
        values,
        fun(_) ->
            evman_acv_video:stop({data, [{"id", Id}]})
        end
    ),
    {"application/json", [], [mochijson2:encode(Res)]}.


delete_acv_video(Req) ->
    Plfields = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Plfields)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),

    Res = dao:dao_call(
        dao_acv_video,
        delete_acv_video,
        Id,
        values,
        fun(_) ->
            evman_acv_video:delete({data, [{"id", Id}]})
        end
    ),
    {"application/json", [], [mochijson2:encode(Res)]}.

chstate_acv_video(Req) ->
    authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_post(),

    State = norm:extr(Plfields, [
        {"id",           [nullable, integer]},
        {"active",       [nullable, boolean]},
        {"sum",          [nullable, integer]}
    ]),


    % Здесь мы не меняем pay_status
    %   pay_status меняем ниже через
    %       dao_acv_video:mkbill

    Acv_video_id = convert:to_integer(proplists:get_value("id", Plfields)),
    {ok, [Acv_video_prev], _, _} = dao_acv_video:get_acv_video(Acv_video_id),
    Res = dao:dao_call(
        dao_acv_video,
        chstate_acv_video,
        State,
        values,
        fun(_) ->
            {ok, [Acv_video_post], _, _} = dao_acv_video:get_acv_video(Acv_video_id),
            fsmbill_acv_video(Acv_video_prev, Acv_video_post)
        end
    ),
    {"application/json", [], [mochijson2:encode(Res)]}.

fsmbill_acv_video(Acv_video_prev, Acv_video_post) ->
    Active_prev      =  proplists:get_value("active", Acv_video_prev),
    Pay_status_prev  =  proplists:get_value("pay_status", Acv_video_prev),

    Id_post          =  proplists:get_value("id", Acv_video_post),
    Active_post      =  proplists:get_value("active", Acv_video_post),
    Pay_status_post  =  proplists:get_value("pay_status", Acv_video_post),

    case {Active_prev, Active_post, Pay_status_post} of
        {null, true, null} ->
            dao_acv_video:mkbill(Id_post),
            evman_acv_video:mkbill({data, Acv_video_post});
        {null, false, _} ->
            % Возможно нужно иное событие, еще одно.
            evman_acv_video:disactivate({data, Acv_video_post});
        {true, false, _} ->
            evman_acv_video:disactivate({data, Acv_video_post});
        {false, true, _} ->
            evman_acv_video:activate({data, Acv_video_post});
        _ ->
            ok
    end.

activate_acv_video(Req) ->
    authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Plfields)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),
    Res = dao:dao_call(
        dao_acv_video,
        activate_acv_video,
        Id,
        values,
        fun(_) ->
            evman_acv_video:activate({data, [{"id", Id}]})
        end
    ),
    {"application/json", [], [mochijson2:encode(Res)]}.

disactivate_acv_video(Req) ->
    authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Plfields)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),
    Res = dao:dao_call(
        dao_acv_video,
        disactivate_acv_video,
        Id,
        values,
        fun(_) ->
            evman_acv_video:disactivate({data, [{"id", Id}]})
        end
    ),
    {"application/json", [], [mochijson2:encode(Res)]}.

full_delete_acv_video(Req) ->
    Plfields = Req:parse_post(),
    Id = convert:to_integer(proplists:get_value("id", Plfields)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),

    Res = dao:dao_call(dao_acv_video, full_delete_acv_video, Id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.


get_acv_video(Req) ->
    Plfields = Req:parse_qs(),
    Id = convert:to_integer(proplists:get_value("id", Plfields)),

    authorization:auth_required(Req, % доступ для владельца и admin
        {fun dao_acv_video:is_owner/2, Id, "admin"}),
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

get_sysvars(Req) ->
    authorization:auth_required(Req, "admin"),
    Res = dao:dao_call(dao_sysvar, get_sysvars, {perm, "admin"}, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

get_sysvar_info(Req) ->
    authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_qs(),
    Id = convert:to_integer(proplists:get_value("id", Plfields)),
    Res = dao:dao_value(dao_sysvar, get_sysvar, Id),
    {"application/json", [], [Res]}.

update_sysvar(Req) ->
    authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_post(),
    Info = norm:extr(Plfields, [
        {"id",      [nullable, integer]},
        {"value",   [integer]}%,
    ]),

    Res = dao:dao_call(dao_sysvar, update_sysvar, Info, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Возвращает конфигурацию системы,
%%%
get_config(Req) ->
    authorization:auth_required(Req, "admin"),

    % Оставлено для совместимости
    Plfields = Req:parse_qs(),
    Id = convert:to_integer(proplists:get_value("id", Plfields)),
    Res = dao:dao_call(dao_config, get_config, Id, values),
    {"application/json", [], [mochijson2:encode(Res)]}.

%%%
%%% Изменяет конфигурацию системы,
%%%
update_config(Req) ->
    authorization:auth_required(Req, "admin"),
    Plfields = Req:parse_post(),

    Info = norm:extr(Plfields, [
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
