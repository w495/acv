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

update_acv_video({null, Name, Datestart, Datestop, Url, Ref, Wish,
    Postroll, Preroll, Midroll, Pauseroll, User_male,
        Age_from, Age_to, Customer_id}) ->

    Query =
        "insert into acv_video (name, datestart, datestop, url, ref, wish,"
            " postroll, preroll, midroll, pauseroll, "
            " user_male, age_from, age_to, customer_id) "
        "values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)"
            " returning acv_video.id;",

    
    case dao:simple_ret(Query, [Name, Datestart, Datestop, Url, Ref,
            convert:to_integer(Wish), Postroll, Preroll, Midroll,
                Pauseroll, User_male,
                    convert:to_integer(Age_from), convert:to_integer(Age_to),
                        convert:to_integer(Customer_id)])  of
        {ok, 1, _, [{Id}]} -> {ok, Id};
        Error -> Error
    end;

update_acv_video({Id, Name, Datestart, Datestop, Url, Ref, Wish,
    Postroll, Preroll, Midroll, Pauseroll, User_male,
        Age_from, Age_to, Customer_id}) ->

    Query =
        "update acv_video set name = $2, datestart = $3, datestop = $4, "
            " url = $5, ref = $6, wish = $7, postroll = $8, preroll = $9, "
            " midroll = $10, pauseroll = $11, user_male = $12, "
            " age_from = $13, age_to = $14, customer_id = $15 where id=$1;",

    dao:simple(Query, [convert:to_integer(Id), Name,
        Datestart, Datestop, Url, Ref,
        convert:to_integer(Wish), Postroll, Preroll, Midroll,
        Pauseroll, User_male,
        convert:to_integer(Age_from), convert:to_integer(Age_to),
        convert:to_integer(Customer_id)]).


delete_acv_video(Id) ->
    Query =
        "delete from acv_video where id = $1;",
    dao:simple(Query, [convert:to_integer(Id)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Юнит тестирование
%%%

test()->

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
    Customer_id =   "1",

    ?MODULE:get_all_acv_videos([]),
    ?MODULE:get_acv_videos(Customer_id),
    {ok, Acv_video_id} =
        ?MODULE:update_acv_video({null,
            Name, Datestart, Datestop, Url, Ref, Wish,
                Postroll, Preroll, Midroll, Pauseroll, User_male,
                    Age_from, Age_to, Customer_id}),
    ?MODULE:update_acv_video({Acv_video_id,
        Name_new, Datestart, Datestop, Url, Ref, Wish,
            Postroll, Preroll, Midroll, Pauseroll, User_male,
                Age_from, Age_to, Customer_id}),
    ?assertEqual({ok,[[
            {"datestop",Datestop},
            {"datestart",Datestart},
            {"name",Name_new},
            {"id",Acv_video_id}]]},
        ?MODULE:get_acv_video(Acv_video_id)),

    ?MODULE:delete_acv_video(Acv_video_id),
    ok.

%%%
%%% Нагрузочное тестирование
%%%

test(speed)->

    ok.

