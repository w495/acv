%%% @file dao_acv_video.erl
%%%
%%%    Администрирование роликов.
%%%

-module(dao_acv_video).

-export([
    get_all_acv_videos/1,
    get_acv_videos/1,
    get_acv_video/1,
    update_acv_video/1,
    delete_acv_video/1,
    test/0,
    test/1
]).

-include("common.hrl").

%%% Поля acv_video:
%%%
%%%         id
%%%         name
%%%         datestart
%%%         datestop
%%%         url
%%%         ref
%%%         wish
%%%         postroll
%%%         preroll
%%%         midroll
%%%         pauseroll
%%%         user_male
%%%         age_from
%%%         age_to
%%%         time_from
%%%         time_to
%%%         customer_id

get_all_acv_videos(_) ->
    Query =
        "select "
            " acv_video.id, "
            " acv_video.name, "
            " acv_video.datestart, "
            " acv_video.datestop "
        " from acv_video;",
    dao:simple(Query).

get_acv_videos(Customer_id) ->
    Query =
        "select "
            " acv_video.id, "
            " acv_video.name, "
            " acv_video.datestart, "
            " acv_video.datestop "
        " from acv_video "
        " where customer_id = $1;",
    dao:simple(Query, [convert:to_integer(Customer_id)]).

get_acv_video(Acv_video_id) ->
    Query =
        "select "
            "acv_video.id, acv_video.name, "
            "acv_video.datestart, acv_video.datestop "
         " from acv_video "
        "where acv_video.id = $1;",
    dao:simple(Query, [convert:to_integer(Acv_video_id)]).

%%% @doc
%%% Создает рекламу видео
%%%
update_acv_video({null, Name, Datestart, Datestop, Url, Ref, Wish,
    Postroll, Preroll, Midroll, Pauseroll, User_male,
        Age_from, Age_to, Time_from, Time_to,
            Duration, Link_title, Alt_title, Shown,
                Customer_id}) ->
    Query =
        "insert into acv_video (name, datestart, datestop, url, ref, wish,"
            " postroll, preroll, midroll, pauseroll, "
                " user_male, age_from, age_to, time_from, time_to, "
                    "duration, link_title, alt_title, shown,"
                        "customer_id) "
        "values ($1, $2, $3, $4, $5, $6, $7, $8, $9, "
                "$10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20)"
            " returning acv_video.id;",
    case dao:simple_ret(Query, [Name, Datestart, Datestop, Url, Ref,
            convert:to_integer(Wish), Postroll, Preroll, Midroll,
                Pauseroll, User_male,
                    convert:to_integer(Age_from), convert:to_integer(Age_to),
                    convert:to_integer(Time_from), convert:to_integer(Time_to),
                        convert:to_integer(Duration), Link_title,
                        Alt_title, convert:to_integer(Shown),
                            convert:to_integer(Customer_id)])  of
        {ok, 1, _, [{Id}]} -> {ok, Id};
        Error -> Error
    end;

%%% @doc
%%% Изменяет рекламу видео
%%%
update_acv_video({Id, Name, Datestart, Datestop, Url, Ref, Wish,
    Postroll, Preroll, Midroll, Pauseroll, User_male,
        Age_from, Age_to, Time_from, Time_to,
            Duration, Link_title, Alt_title, Shown,
                Customer_id}) ->
    Query =
        "update acv_video set name = $2, datestart = $3, datestop = $4, "
            " url = $5, ref = $6, wish = $7, postroll = $8, preroll = $9, "
            " midroll = $10, pauseroll = $11, user_male = $12, "
            " age_from = $13, age_to = $14, time_from = $15, time_to = $16, "
            " duration = $17, link_title = $18, alt_title = $19, shown = $20,"
            " customer_id = $21 where id=$1;",
    dao:simple(Query, [convert:to_integer(Id), Name,
        Datestart, Datestop, Url, Ref,
        convert:to_integer(Wish), Postroll, Preroll, Midroll,
        Pauseroll, User_male,
        convert:to_integer(Age_from), convert:to_integer(Age_to),
        convert:to_integer(Time_from), convert:to_integer(Time_to),
        convert:to_integer(Duration), Link_title,
        Alt_title, convert:to_integer(Shown),
        convert:to_integer(Customer_id)]);

%%% @doc
%%% Создает рекламу видео и обвязки к ней
%%%
update_acv_video({{null, Name, Datestart, Datestop, Url, Ref, Wish,
    Postroll, Preroll, Midroll, Pauseroll, User_male,
        Age_from, Age_to, Time_from, Time_to,
            Duration, Link_title, Alt_title, Shown,
                Customer_id},
                    Geo_region_list, Cat_id_list}) ->

    Query_insert =
        "insert into acv_video (name, datestart, datestop, url, ref, wish,"
            " postroll, preroll, midroll, pauseroll, "
                " user_male, age_from, age_to, time_from, time_to, "
                    "duration, link_title, alt_title, shown,"
                        "customer_id) "
        "values ($1, $2, $3, $4, $5, $6, $7, $8, $9, "
                "$10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20)"
            " returning acv_video.id;",

    Pre_result = dao:with_transaction_fk(
        fun(Con) ->
            case pgsql:equery(Con, Query_insert,
                [Name, Datestart, Datestop, Url, Ref,
                    convert:to_integer(Wish), Postroll, Preroll, Midroll,
                        Pauseroll, User_male,
                            convert:to_integer(Age_from), convert:to_integer(Age_to),
                            convert:to_integer(Time_from), convert:to_integer(Time_to),
                                convert:to_integer(Duration), Link_title,
                                Alt_title, convert:to_integer(Shown),
                                    convert:to_integer(Customer_id)])  of
                {ok, 1, _, [{Id}]} ->
                    case length(Geo_region_list) of
                        0 ->    ok;
                        _ ->
                            Query_acv_video2geo_region =
                                "insert into acv_video2geo_region "
                                 "(acv_video_id, geo_region_id) values " ++
                                  make_brackets_string(Id, Geo_region_list),
                            {ok, _} = pgsql:equery(Con,
                                Query_acv_video2geo_region, [])
                    end,
                    case length(Cat_id_list) of
                        0 ->    ok;
                        _ ->
                            Query_acv_video2cat =
                                "insert into acv_video2cat "
                                    "(acv_video_id, cat_id) values " ++
                                        make_brackets_string(Id, Cat_id_list),
                            {ok, _} = pgsql:equery(Con,
                                Query_acv_video2cat, [])
                    end,
                    {ok, Id};
                Error -> Error
            end
        end
    ),
    case Pre_result of
        {ok, Id} -> {ok, Id};
        {_, Error} -> Error
    end;

%%% @doc
%%% Изменяет рекламу видео и обвязки к ней
%%%
update_acv_video({{Id, Name, Datestart, Datestop, Url, Ref, Wish,
    Postroll, Preroll, Midroll, Pauseroll, User_male,
        Age_from, Age_to, Time_from, Time_to,
            Duration, Link_title, Alt_title, Shown,
                Customer_id},
                    Geo_region_list, Cat_id_list}) ->

    Query_update =
        "update acv_video set name = $2, datestart = $3, datestop = $4, "
            " url = $5, ref = $6, wish = $7, postroll = $8, preroll = $9, "
            " midroll = $10, pauseroll = $11, user_male = $12, "
            " age_from = $13, age_to = $14, time_from = $15, time_to = $16, "
            " duration = $17, link_title = $18, alt_title = $19, shown = $20,"
            " customer_id = $21 where id=$1;",

    Pre_result = dao:with_transaction_fk(
        fun(Con) ->
            case pgsql:equery(Con, Query_update,
                [Id, Name, Datestart, Datestop, Url, Ref,
                convert:to_integer(Wish), Postroll, Preroll, Midroll,
                    Pauseroll, User_male,
                        convert:to_integer(Age_from), convert:to_integer(Age_to),
                        convert:to_integer(Time_from), convert:to_integer(Time_to),
                            convert:to_integer(Duration), Link_title,
                            Alt_title, convert:to_integer(Shown),
                                convert:to_integer(Customer_id)]) of
                {ok, _ } ->
                    case length(Geo_region_list) of
                        0 ->    ok;
                        _ ->
                            Query_video2region_d =
                                "delete from acv_video2geo_region "
                                    "where acv_video_id = $1", 
                            Query_acv_video2geo_region =
                                "insert into acv_video2geo_region "
                                 "(acv_video_id, geo_region_id) values " ++
                                  make_brackets_string(Id, Geo_region_list),
                            pgsql:equery(Con, Query_video2region_d, [Id]),
                            {ok, _} = pgsql:equery(Con,
                                Query_acv_video2geo_region, [])
                    end,
                    case length(Cat_id_list) of
                        0 ->    ok;
                        _ ->
                            Query_video2cat_d =
                                "delete from acv_video2cat "
                                    "where acv_video_id = $1", 
                            Query_acv_video2cat =
                                "insert into acv_video2cat "
                                    "(acv_video_id, cat_id) values " ++
                                        make_brackets_string(Id, Cat_id_list),
                            pgsql:equery(Con, Query_video2cat_d, [Id]),
                            {ok, _} = pgsql:equery(Con,
                                Query_acv_video2cat, [])
                    end,
                    {ok, Id};
                Error -> Error
            end
        end
    ).

%%% @doc
%%% Удаляет обвязки рекламы и ее сущность
%%%
delete_acv_video(Id) ->
    Query_video =
        "delete from acv_video where id = $1;",
    Query_video2geo_region =
        "delete from acv_video2geo_region where acv_video_id = $1;",
    Query_video2cat =
        "delete from acv_video2cat where acv_video_id = $1;",
    dao:with_transaction_fk(
        fun(Con) ->
            pgsql:equery(Con, Query_video2geo_region, [convert:to_integer(Id)]),
            pgsql:equery(Con, Query_video2cat, [convert:to_integer(Id)]),
            pgsql:equery(Con, Query_video, [convert:to_integer(Id)])
        end
    ).

make_brackets_string(Id, Id_list)->
    string:join([string:join(["(", convert:to_list(Id), ",",
        convert:to_list(X),")"], []) || X <- Id_list], ",").

make_brackets_string_(Id, Id_list)->
    string:join([lists:flatten(io_lib:format("(~p,~p)",
        [Id, X])) || X <- Id_list], ",").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Юнит тестирование
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

test()->
    test_eunit_1(),
    test_eunit_2(),
    ok.

%%%
%%% Просто
%%%
test_eunit_1()->
    Name =          "some name",
    Name_new =      "some new name",
    Datestart =     {{10865,3,14},{16,25,39.1}},
    Datestop =      {{10855,6,04},{07,16,30.0}},
    Url  =          "url",
    Ref =           "ref",
    Wish =          "1",
    Postroll =      "true",
    Preroll =       "true",
    Midroll =       "true",
    Pauseroll =     "true",
    User_male =     "true",
    Age_from  =     "1",
    Age_to =        "2",
    Time_from  =    "10",
    Time_to =       "11",

    Duration  =     "1",
    Link_title =    "Link_title",
    Alt_title  =    "Alt_title",
    Shown =         "11",

    Customer_id =   "1",

    ?MODULE:get_all_acv_videos([]),
    ?MODULE:get_acv_videos(Customer_id),
    {ok, Acv_video_id} =
        ?MODULE:update_acv_video({null,
            Name, Datestart, Datestop, Url, Ref, Wish,
                Postroll, Preroll, Midroll, Pauseroll, User_male,
                    Age_from, Age_to, Time_from, Time_to,
                        Duration, Link_title, Alt_title, Shown,
                            Customer_id}),
    ?MODULE:update_acv_video({Acv_video_id,
        Name_new, Datestart, Datestop, Url, Ref, Wish,
            Postroll, Preroll, Midroll, Pauseroll, User_male,
                Age_from, Age_to, Time_from, Time_to,
                    Duration, Link_title, Alt_title, Shown,
                        Customer_id}),
    ?assertEqual({ok,[[
            {"datestop",Datestop},
            {"datestart",Datestart},
            {"name",Name_new},
            {"id",Acv_video_id}]]},
        ?MODULE:get_acv_video(Acv_video_id)),
    ?MODULE:delete_acv_video(Acv_video_id),
    ok.

%%%
%%% Geo
%%%

test_eunit_2()->

    R_list = [ %%% Создаем набор регионов, который потом удалим
        begin
            {ok,1,_,[{Id}]} =
                dao:simple_ret("insert into geo_region (alias, name) "
                    "values ('" ++ R ++ "', 'name of alias " ++ R ++ "') "
                        "returning geo_region.id;", []),
                            erlang:integer_to_list(Id)
        end ||
        R <- [lists:concat(erlang:tuple_to_list(now())),
                lists:concat(erlang:tuple_to_list(now())),
                    lists:concat(erlang:tuple_to_list(now()))]
    ],

    R_list_new = [ %%% Создаем набор регионов, который потом удалим
        begin
            {ok,1,_,[{Id}]} =
                dao:simple_ret("insert into geo_region (alias, name) "
                    "values ('" ++ R ++ "', 'name of alias " ++ R ++ "') "
                        "returning geo_region.id;", []),
                            erlang:integer_to_list(Id)
        end ||
        R <- [lists:concat(erlang:tuple_to_list(now())),
                lists:concat(erlang:tuple_to_list(now())),
                    lists:concat(erlang:tuple_to_list(now())),
                        lists:concat(erlang:tuple_to_list(now())),
                            lists:concat(erlang:tuple_to_list(now()))]
    ],

    Name =          "name" ++ string:join(R_list, " "),
    Name_new =      string:join(R_list, " ") ++ " name",
    Datestart =     {{1970,3,14},{16,25,39.1}},
    Datestop =      {{1970,6,04},{07,16,30.0}},
    Url  =          "url",
    Ref =           "ref",
    Wish =          "1",
    Postroll =      "true",
    Preroll =       "true",
    Midroll =       "true",
    Pauseroll =     "true",
    User_male =     "true",
    Age_from  =     "1",
    Age_to =        "2",
    Time_from  =    "10",
    Time_to =       "11",

    Duration  =     "1",
    Link_title =    "Link_title",
    Alt_title  =    "Alt_title",
    Shown =         "11",

    Customer_id =   "1",

    {ok, Acv_video_id} = ?MODULE:update_acv_video({{null,
        Name, Datestart, Datestop, Url, Ref, Wish,
            Postroll, Preroll, Midroll, Pauseroll, User_male,
                Age_from, Age_to, Time_from, Time_to,
                    Duration, Link_title, Alt_title, Shown,
                        Customer_id}, R_list, []}),

    ?MODULE:update_acv_video({{Acv_video_id,
        Name_new, Datestart, Datestop, Url, Ref, Wish,
            Postroll, Preroll, Midroll, Pauseroll, User_male,
                Age_from, Age_to, Time_from, Time_to,
                    Duration, Link_title, Alt_title, Shown,
                        Customer_id}, R_list_new, []}),

    ?MODULE:delete_acv_video(Acv_video_id),
    lists:foreach(fun(R)->
        dao_geo_region:delete_geo_region(R),
    ok end,R_list),

    lists:foreach(fun(R)->
        dao_geo_region:delete_geo_region(R),
    ok end,R_list_new),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Нагрузочное тестирование
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(speed)->

    Times_1 = 1000000,
    List = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],

    ?assertEqual(make_brackets_string(0, List),make_brackets_string_(0, List)),

    %%%
    %%% join лучше
    %%%
    tests:print_speed("join",
        fun() -> % лучше
            make_brackets_string(0, List)
        end, Times_1 ),
    tests:print_speed("flatten",
        fun() ->
            make_brackets_string_(0, List)
        end, Times_1 ),
    ok.
