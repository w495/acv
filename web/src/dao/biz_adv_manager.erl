%%% @file biz_adv_manager.erl
%%%
%%%    Бизнес логика раздачи рекламных креативов.
%%%

-module(biz_adv_manager).

-export([
    test/0,
    test/1
]).

-define(VK_STREAMER_DEFAULT, "http://192.168.2.187:8000").
-define(XML_TOP, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").
-define(DELIM, []).
-define(SPACE, " ").


-compile(export_all).

-include("common.hrl").

get_acv_mp4({Type, Resourse_mp4, _User_id}, Peer) ->
    {Resourse, _ } = lists:split(length(Resourse_mp4) - 4, Resourse_mp4),
    get_acv({Type, Resourse, _User_id}, Peer).

 %5615d6c0-79c3-4a90-bf34-9d23ae78c14a
get_acv({Type, Resourse, _User_id}, Peer) when Type =:= "preroll"; Type =:= "postroll"; Type =:= "midroll"; Type =:= "pauseroll"->

    ?D("Type = ~p~n", [Type]),
    ?D("Resourse = ~p~n", [Resourse]),
    ?D("_User_id = ~p~n", [_User_id]),
    ?D("_User_id = ~p~n", [Peer]),

    %   biz_adv_manager:get_acv({{"preroll", "354b9bd8-c2aa-4a92-81ee-0fccc85a9273", null}, 0})

    Query = <<"select mark.id from clip_mark join clip on clip_mark.clip_id = clip.id "
            "join mark on clip_mark.mark_id = mark.id "
            "join mark_type on mark.mark_type_id = mark_type.id "
            "where mark_type.name=\"Categories\" and clip.url=?;">>,

    mysql:prepare(get_clip_categories, Query),
    {data,{mysql_result, Cols, Vals, _X31, _X32}} = mysql:execute(mySqlConPool, get_clip_categories, [list_to_binary(Resourse)]),

    Ms_proplist = mysql_dao:simple(Query, [Resourse]),

    ?D("------------~n~p~n", [Vals]),

    % TODO перевести дататаймы в UTC и селектить NOW аналогично в UTC
    Q2S1 = "select distinct(acv_video.id), url, datestart, datestop, ref, wish, link_title, shown, duration "
            "from acv_video left join acv_video2cat on acv_video.id=acv_video2cat.acv_video_id "
                    "left join acv_video2geo_region on acv_video.id=acv_video2geo_region.acv_video_id "
            "where "
                "acv_video.datestart < (select NOW()) and acv_video.datestop > (select NOW()) and "
                %"acv_video.wish > acv_video.shown and "
                "acv_video." ++ Type ++ " = true and "
                "acv_video.user_male is NULL and acv_video.age_from is NULL and acv_video.age_to is NULL ",
    if
        length(Vals) > 0 ->
            Q2S2 = Q2S1 ++ " and (acv_video2cat.cat_id in (" ++ 
                string:join([integer_to_list(X) || X <- lists:flatten(Vals)], ",") ++
                ") or acv_video2cat.cat_id is NULL) ";
        true ->
            Q2S2 = Q2S1 ++ " and acv_video2cat.cat_id is NULL "
    end,

    Q2 = Q2S2 ++ ";",
    {ok, Ret} = dao:simple(Q2),
    Result_string = make_acv_xml_naive(Ret),

    ?D("Result_string  = ~s~n", [Result_string]),

    Result_string .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%
%%% @doc
%%% Возвращает строку с XML для плеера.
%%% В этой строки находится полное описание рекламы.
%%%
make_acv_xml(List) ->
    {Acc_str, Acc_duration} = make_acv_xml_item(List, {[], 0}),
    ?XML_TOP ++
    ?FMT("<block duration=\"~s\" loadnext=\"600\">", [Acc_duration]) ++
        Acc_str ++
    "</block>".

%%%
%%% @doc
%%% Возвращает строку с XML для плеера.
%%% В этой строки находится полное описание рекламы.
%%% НАИВНАЯ РЕАЛИЗАЦИЯ
%%%
make_acv_xml_item([], Acc) -> Acc;
make_acv_xml_item([Creative|Tail] = List, {Acc_str, Acc_duration})->
    Creative_string = creative_string(Creative),
    make_acv_xml_item(Tail,
        {Acc_str ++ Creative_string,
            Acc_duration + proplists:get_value("duration", Creative)}).

%%%
%%% @doc
%%% Возвращает строку с XML для плеера.
%%% В этой строки находится полное описание рекламы.
%%% НАИВНАЯ РЕАЛИЗАЦИЯ
%%%
make_acv_xml_naive(List) ->
    ?XML_TOP ++
    "<block duration=\"120\" loadnext=\"600\">" ++
   lists:concat([
        creative_string(Item)
        || Item <- List
    ]) ++
    "</block>".

%%%
%%% @doc
%%% Возвращает строку с XML одной рекламной сущности
%%% 
%%% Не самая эффективная реализация, т.к. используется FMT.
%%% При возникновении проблем нужно использовать конкатенацию.
%%% 
creative_string(Creative) ->
    ?FMT("<creative "
            " type=\"video\" click=\"~s\" "
            " link_title=\"~s\""
            " url=\"~s/~s\""
            " start=\"0\""
            " skip=\"no\" "
            " duration=\"~p\"  "
        "/>",
        [
            proplists:get_value("url", Creative),
            proplists:get_value("link_title", Creative),
            config:get(vk_streamer, ?VK_STREAMER_DEFAULT),
            string:strip(proplists:get_value("ref", Creative)),
            proplists:get_value("duration", Creative)
        ]
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Юнит тестирование
%%%

mysql_test_1() ->
    R = {data,{mysql_result, Cols, Vals, _X31, _X32}} = mysql:fetch(mySqlConPool, <<"select * from customer where uid = 1265;">>),
    ?D("------------~n~p~n", [R]),
    R2 = mysql_dao:make_proplist(Cols, Vals, []),
    ?D("------------~n~p~n", [R2]),
    ok.

mysql_test_2() ->
    R = mysql:fetch(mySqlConPool, <<"select ** from customer where uid = 1265111;">>),
    ?D("------------~n~p~n", [R]),
    ok.

test()->
    ok.

%%%
%%% Нагрузочное тестирование
%%%

test(speed)->

    ok.

