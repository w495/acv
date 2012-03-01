%%% @file dao_geo_region.erl
%%%
%%%    Администрирование регионов.
%%%

-module(dao_geo_region).

-export([
    get_all_geo_regions/1,
    get_geo_region/1,
    get_contries/1,
    get_cities/1,
    get_geo_areas/1,
    update_geo_region/1,
    delete_geo_region/1,
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

get_all_geo_regions(_) ->
    Query =
        "select "
            " geo_region.id, "
            " geo_region.name_en, "
            " geo_region.name_ru "
        " from geo_region limit 10;",
    dao:simple(Query).

get_geo_region(Geo_region_id) ->
    Query =
        "select "
            " geo_region.id, "
            " geo_region.name_en, "
            " geo_region.name_ru "
         " from geo_region "
        "where geo_region.id = $1;",
    dao:simple(Query, [convert:to_integer(Geo_region_id)]).

%%% 
%%% get_contries
%%%

get_contries(Id) when erlang:is_integer(Id)->
    Query =
        "select "
            " geo_region.id, "
            " geo_region.name_en, "
            " geo_region.name_ru "
        " from geo_region "
            " where country_id is null "
                " and geo_region.geo_area_id = $1 "
                " and name_en != ''; ",
    dao:simple(Query, [Id]);

get_contries(Name_en) when erlang:is_list(Name_en)->
    Query =
        "select "
            " geo_region.id, "
            " geo_region.name_en, "
            " geo_region.name_ru "
        " from geo_region "
            " join geo_area on "
                " geo_area.name_en = $1 "
                " and geo_region.country_id is null "
                " and geo_region.geo_area_id = geo_area.id "
                " and geo_region.name_en != '' ; ",
    dao:simple(Query, [Name_en]);

get_contries(_) ->
    Query =
        "select "
            " geo_region.id, "
            " geo_region.name_en, "
            " geo_region.name_ru "
        " from geo_region "
            " where country_id is null "
                " and name_en != ''; ",
    dao:simple(Query).

%%%
%%% get_contries
%%%

get_cities(Country_id) when erlang:is_integer(Country_id)->
    Query =
        "select "
            " geo_region.id, "
            " geo_region.name_en, "
            " geo_region.name_ru "
        " from geo_region "
            " where "
                " country_id = $1 limit 10; ",
    dao:simple(Query, [Country_id]);

get_cities(Name_en) when erlang:is_integer(Name_en)->
    Query =
        "select "
            " city.id, "
            " city.name_en, "
            " city.name_ru "
        " from geo_region as city ",
            " join geo_region as country on "
                " country.name_en = $1 "
                " city.country_id = country.id limit 10; ",
    dao:simple(Query, [Name_en]);

get_cities(_) ->
    Query =
        "select "
            " geo_region.id, "
            " geo_region.name_en, "
            " geo_region.name_ru "
        " from geo_region "
            " where "
                " geo_region.country_id is not null limit 10;",
    dao:simple(Query).

%%%
%%% get_geo_areas
%%%

get_geo_areas(_)->
    Query =
        "select "
            " geo_area.id, "
            " geo_area.name_en, "
            " geo_area.name_ru "
        " from geo_area; ",
    dao:simple(Query).

update_geo_region({null, Alias, Name}) ->
    Query =
        "insert into geo_region (name_en, name_ru) "
        "values ($1, $2)"
            " returning geo_region.id;",

    case dao:simple_ret(Query, [Alias, Name])  of
        {ok, 1, _, [{Id}]} -> {ok, Id};
        Error -> Error
    end;

update_geo_region({Id, Alias, Name}) ->
    Query =
        "update geo_region set name_en = $2, name_ru = $3 where id=$1;",
    dao:simple(Query, [convert:to_integer(Id), Alias, Name]).

delete_geo_region(Id) ->
    Query_geo_region =
        "delete from geo_region where id = $1;",
    Query_acv_video2geo_region =
        "delete from acv_video2geo_region where geo_region_id = $1;",

    dao:with_transaction_fk(
        fun(Con) ->
            pgsql:equery(Con, Query_acv_video2geo_region, [convert:to_integer(Id)]),
            pgsql:equery(Con, Query_geo_region, [convert:to_integer(Id)])
        end
    ).

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

