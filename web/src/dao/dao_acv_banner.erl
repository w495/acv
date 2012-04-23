%%% @file dao_bAdvCom.erl
%%%
%%%     Администрирование баннеров.
%%%

-module(dao_acv_banner).

-export([
    get_all_acv_banners/1,
    get_acv_banners/1,
    get_acv_banner/1,
    update_acv_banner/1,
    update_acv_banner_internal/1,
    delete_acv_banner/1,
    test/0,
    test/1
]).

-include("common.hrl").

%%% Поля acv_banner:
%%%
%%%         id
%%%         name
%%%         datestart
%%%         datestop
%%%         url
%%%         ref
%%%         banner_place_id
%%%         customer_id
%%%


get_all_acv_banners(_) ->
    Query =
        "select "
            " acv_banner.id, "
            " acv_banner.name, "
            " acv_banner.datestart, "
            " acv_banner.datestop "
        " from acv_banner;",
    dao:simple(Query).

get_acv_banners(Customer_id) ->
    Query =
        "select "
            " acv_banner.id, "
            " acv_banner.name, "
            " acv_banner.datestart, "
            " acv_banner.datestop "
        " from acv_banner "
        " where customer_id = $1;",
    dao:simple(Query, [convert:to_integer(Customer_id)]).

get_acv_banner(Acv_banner_id) ->
    Query =
        "select "
            "acv_banner.id, acv_banner.name, "
            "acv_banner.datestart, acv_banner.datestop "
         " from acv_banner "
        "where acv_banner.id = $1;",
    dao:simple(Query, [convert:to_integer(Acv_banner_id)]).

%%% @doc
%%% Создает новый баннер Banner_place_alias --- это строка
%%%
update_acv_banner({null, Name, Datestart, Datestop, Url, Ref,
    Banner_place_name, Customer_id}) ->

    Query_select = "select banner_place.id from banner_place "
        "where banner_place.name = $1;", % Banner_place_name

    Query_insert  =
        "insert into acv_banner (name, datestart, datestop, url, ref, "
            " banner_place_id, customer_id) "
        "values ($1, $2, $3, $4, $5, $6, $7)"
            " returning acv_banner.id;",

    Pre_result = dao:with_transaction_fk(
        fun(Con) ->
            case dao:pgret(dao:equery(Con, Query_select,
                    [Banner_place_name])) of
                {ok,[[{"id",Banner_place_id}]]} ->
                    dao:equery(Con, Query_insert ,
                        [Name, Datestart, Datestop, Url, Ref, Banner_place_id,
                            convert:to_integer(Customer_id)]);
                Error -> Error
            end
        end
    ),
    case {dao:pgret(Pre_result), Pre_result} of
        {ok, {ok, 1, _, [{Id}]}} -> {ok, Id};
        {_, Error} -> Error
    end;

%%% @doc
%%% Изменяет баннер Banner_place_alias --- это строка
%%%
update_acv_banner({Id, Name, Datestart, Datestop, Url, Ref,
    Banner_place_name, Customer_id}) ->

    Query_select = "select banner_place.id from banner_place "
        "where banner_place.name = $1;", % Banner_place_name

    Query_update =
        "update acv_banner set name = $2, datestart = $3, datestop = $4, "
            " url = $5, ref = $6, banner_place_id = $7, "
                "customer_id = $8 where id=$1;",

    dao:pgret(
        dao:with_transaction_fk(
            fun(Con) ->
                case dao:pgret(dao:equery(Con, Query_select,
                        [Banner_place_name])) of
                    {ok,[[{"id",Banner_place_id}]]} ->
                        dao:equery(Con, Query_update ,
                            [convert:to_integer(Id), Name, Datestart,
                                Datestop, Url, Ref, Banner_place_id,
                                    convert:to_integer(Customer_id)]);
                    Error -> Error
                end
            end
        )
    ).

%%% @doc
%%% Создает новый баннер Banner_place_id --- это число
%%%
update_acv_banner_internal({null, Name, Datestart, Datestop, Url, Ref,
    Banner_place_id, Customer_id}) ->
    Query =
        "insert into acv_banner (name, datestart, datestop, url, ref, "
            " banner_place_id, customer_id) "
        "values ($1, $2, $3, $4, $5, $6, $7)"
            " returning acv_banner.id;",
    case dao:simple_ret(Query, [Name, Datestart, Datestop, Url, Ref,
            convert:to_integer(Banner_place_id),
                convert:to_integer(Customer_id)]) of
        {ok, 1, _, [{Id}]} -> {ok, Id};
        Error -> Error
    end;

%%% @doc
%%% Меняет баннер Banner_place_id --- это число
%%%
update_acv_banner_internal({Id, Name, Datestart, Datestop, Url, Ref,
    Banner_place_id, Customer_id}) ->
    Query =
        "update acv_banner set name = $2, datestart = $3, datestop = $4, "
            " url = $5, ref = $6, banner_place_id = $7, "
                "customer_id = $8 where id=$1;",
    dao:simple(Query, [convert:to_integer(Id),
        Name, Datestart, Datestop, Url, Ref,
        convert:to_integer(Banner_place_id),
        convert:to_integer(Customer_id)]).

delete_acv_banner(Id) ->
    Query =
        "delete from acv_banner where id = $1;",
    dao:simple(Query, [convert:to_integer(Id)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Юнит тестирование
%%%

test()->

    Name =                  "some name",
    Name_new =              "some new name",
    Datestart =             {{10865,3,14},{16,25,39.1}},
    Datestop =              {{10855,6,04},{07,16,30.0}},
    Url  =                  "url",
    Ref =                   "ref",
    Banner_place_id =       "2",
    Banner_place_name =     "right",
    Banner_place_name_new = "bottom",
    Customer_id =           "1",

    ?MODULE:get_all_acv_banners([]),
    ?MODULE:get_acv_banners(Customer_id),

    {ok, Acv_banner_id_1} =
        ?MODULE:update_acv_banner_internal({null,
            Name, Datestart, Datestop, Url, Ref,
                Banner_place_id, Customer_id}),
    ?MODULE:update_acv_banner_internal({Acv_banner_id_1,
        Name_new, Datestart, Datestop, Url, Ref,
            Banner_place_id, Customer_id}),
    ?assertEqual({ok,[[
            {"datestop",Datestop},
            {"datestart",Datestart},
            {"name",Name_new},
            {"id",Acv_banner_id_1}]]},
        ?MODULE:get_acv_banner(Acv_banner_id_1)),
    ?MODULE:delete_acv_banner(Acv_banner_id_1),

    {ok, Acv_banner_id_2} =
        ?MODULE:update_acv_banner({null,
            Name, Datestart, Datestop, Url, Ref,
                Banner_place_name, Customer_id}),
    ?MODULE:update_acv_banner({Acv_banner_id_2,
        Name_new, Datestart, Datestop, Url, Ref,
            Banner_place_name_new, Customer_id}),
    ?assertEqual({ok,[[
            {"datestop",Datestop},
            {"datestart",Datestart},
            {"name",Name_new},
            {"id",Acv_banner_id_2}]]},
        ?MODULE:get_acv_banner(Acv_banner_id_2)),
    ?MODULE:delete_acv_banner(Acv_banner_id_2),


    ok.

%%%
%%% Нагрузочное тестирование
%%%

test(speed)->

    ok.
