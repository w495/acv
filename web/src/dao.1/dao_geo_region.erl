%%% @file dao_geo_region.erl
%%%
%%%    Администрирование регионов.
%%%

-module(dao_geo_region).

-export([
    get_all_geo_regions/1,
    get_geo_region/1,
    update_geo_region/1,
    delete_geo_region/1,
    test/0,
    test/1
]).

-include("common.hrl").

%%% Поля geo_region:
%%%
%%%         id
%%%         alias 
%%%         name
%%%

get_all_geo_regions(_) ->
    Query =
        "select "
            " geo_region.id, "
            " geo_region.alias, "
            " geo_region.name "
        " from geo_region;",
    dao:simple(Query).

get_geo_region(Geo_region_id) ->
    Query =
        "select "
            " geo_region.id, "
            " geo_region.alias, "
            " geo_region.name "
         " from geo_region "
        "where geo_region.id = $1;",
    dao:simple(Query, [convert:to_integer(Geo_region_id)]).

update_geo_region({null, Alias, Name}) ->

    Query =
        "insert into geo_region (alias, name) "
        "values ($1, $2)"
            " returning geo_region.id;",

    case dao:simple_ret(Query, [Alias, Name])  of
        {ok, 1, _, [{Id}]} -> {ok, Id};
        Error -> Error
    end;

update_geo_region({Id, Alias, Name}) ->

    Query =
        "update geo_region set alias = $2, name = $3 where id=$1;",

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

    R_1 = erlang:integer_to_list(random:uniform(?MAX_INT)),
    R_2 = erlang:integer_to_list(random:uniform(?MAX_INT)),

    Alias =         "alias " ++ R_1,
    Alias_new =     "alias " ++ R_2,

    Name =          "name " ++ R_1,
    Name_new =      "name " ++ R_2,

    ?MODULE:get_all_geo_regions([]),

    {ok, Geo_region_id} =
        ?MODULE:update_geo_region({null, Alias, Name}),
    ?MODULE:update_geo_region({Geo_region_id, Alias_new, Name_new}),

    ?assertEqual({ok,[[
            {"name",Name_new},
            {"alias",Alias_new},
            {"id",Geo_region_id}]]},
        ?MODULE:get_geo_region(Geo_region_id)),

    ?MODULE:delete_geo_region(Geo_region_id),
    ok.

%%%
%%% Нагрузочное тестирование
%%%

test(speed)->

    ok.

