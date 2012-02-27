%%% @file dao_geo_region.erl
%%%
%%%    Администрирование регионов.
%%%

-module(dao_config).

-export([
    get_config/1,
    update_config/1,
    test/0,
    test/1
]).

-include("common.hrl").

%%% Поля geo_region:
%%%
%%%         id
%%%         name_en
%%%         name_ru
%%%

get_config(Config_id) ->
    Query =
        "select "
            " config.id, "
            " config.acv_video_loadnext "
        " from config "
        " where config.id = $1;",
    dao:simple(Query, [convert:to_integer(Config_id)]).

update_config({null, Acv_video_loadnext}) ->
    Query =
        "insert into config (acv_video_loadnext) "
        "values ($1)"
            " returning config.id;",
    case dao:simple_ret(Query, [Acv_video_loadnext])  of
        {ok, 1, _, [{Id}]} -> {ok, Id};
        Error -> Error
    end;

update_config({Id, Acv_video_loadnext}) ->
    Query =
        "update config set acv_video_loadnext = $2 where id=$1;",
    dao:simple(Query, [convert:to_integer(Id), convert:to_integer(Acv_video_loadnext)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Юнит тестирование
%%%

-define(MAX_INT, 576460752303423488). %% 60 бит

test()->

%     R_1 = erlang:integer_to_list(random:uniform(?MAX_INT)),
%     R_2 = erlang:integer_to_list(random:uniform(?MAX_INT)),
% 
%     Alias =         "name_en " ++ R_1,
%     Alias_new =     "name_en " ++ R_2,
% 
%     Name =          "name_ru " ++ R_1,
%     Name_new =      "name_ru " ++ R_2,
% 
%     ?MODULE:get_all_geo_regions([]),
% 
%     {ok, Geo_region_id} =
%         ?MODULE:update_geo_region({null, Alias, Name}),
%     ?MODULE:update_geo_region({Geo_region_id, Alias_new, Name_new}),
% 
%     ?assertEqual({ok,[[
%             {"name_ru",Name_new},
%             {"name_en",Alias_new},
%             {"id",Geo_region_id}]]},
%         ?MODULE:get_geo_region(Geo_region_id)),
% 
%     ?MODULE:delete_geo_region(Geo_region_id),
    ok.

%%%
%%% Нагрузочное тестирование
%%%

test(speed)->

    ok.

