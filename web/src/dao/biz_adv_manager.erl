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
get_acv({Type, Resourse, User_id}, Peer) when Type =:= "preroll"; Type =:= "postroll"; Type =:= "midroll"; Type =:= "pauseroll"->

    ?D("Type = ~p~n", [Type]),
    ?D("Resourse = ~p~n", [Resourse]),
    ?D("User_id = ~p~n", [User_id]),
    ?D("Peer = ~p~n", [Peer]),

    %   biz_adv_manager:get_acv({{"preroll", "354b9bd8-c2aa-4a92-81ee-0fccc85a9273", null}, 0})

    Query = <<"select mark.id from clip_mark join clip on clip_mark.clip_id = clip.id "
            "join mark on clip_mark.mark_id = mark.id "
            "join mark_type on mark.mark_type_id = mark_type.id "
            "where mark_type.name=\"Categories\" and clip.url=?;">>,

    %TODO реюзать prepare
    mysql:prepare(get_clip_categories, Query),
    {data,{mysql_result, _Cols, Vals, _X31, _X32}} = mysql:execute(mySqlConPool, get_clip_categories, [list_to_binary(Resourse)]),

    ?D("------------~nFilm (~p) categories: ~p~n", [Resourse, Vals]),
    

    % TODO перевести дататаймы в UTC и селектить NOW аналогично в UTC
    Q2S1 = "select distinct(acv_video.id), url, datestart, datestop, ref, wish, link_title, shown, duration "
            "from acv_video left join acv_video2cat on acv_video.id=acv_video2cat.acv_video_id "
                    "left join acv_video2geo_region on acv_video.id=acv_video2geo_region.acv_video_id "
            "where "
                "acv_video.datestart < (select NOW()) and acv_video.datestop > (select NOW()) and "
                "acv_video.wish > acv_video.shown and "
                "acv_video." ++ Type ++ " = true and "
                "not exists (select * from acv_video_shown where acv_video_shown.acv_video_id = acv_video.id "
                                "and acv_video_shown.user_id='" ++ utils:to_list(User_id) ++ "') ",
    if
        length(Vals) > 0 ->
            Q2S2 = Q2S1 ++ " and (acv_video2cat.cat_id in (" ++ 
                string:join([integer_to_list(X) || X <- lists:flatten(Vals)], ",") ++
                ") or acv_video2cat.cat_id is NULL) ";
        true ->
            Q2S2 = Q2S1 ++ " and acv_video2cat.cat_id is NULL "
    end,

    QC = <<"select * from customer where uid=?;">>,
    %TODO реюзать prepare
    mysql:prepare(get_customer, QC),
    {data,{mysql_result, CCols, CVals, _X31, _X32}} = mysql:execute(mySqlConPool, get_customer, [User_id]),
    CL = mysql_dao:make_proplist(CCols, CVals),

    % таргетирование по параметрам кастомера
    case CL of
        [] ->
            ?D("~n------------------------~n", []),
            ?D("Q2S2 = ~p", [Q2S2]),
            ?D("~n------------------------~n", []),

            Q2S3 = Q2S2 ++ " and acv_video.user_male is NULL and acv_video.age_from is NULL and acv_video.age_to is NULL";
        [Customer] ->
            Q2S3 = string:join([
                Q2S2, 
                case proplists:get_value("gender", Customer) of
                    "male" ->
                        " and (acv_video.user_male=true or acv_video.user_male is NULL) ";
                    "female" ->
                        " and (acv_video.user_male=false or acv_video.user_male is NULL) ";
                    EGender ->
                        io:format("ERROR: invalid gender - ~p~n", [EGender]),
                        ""
                end,
                ?D("~n------------------------~n", []),
                ?D("Q2S3 = ~p", [Q2S2]),
                ?D("~n------------------------~n", []),

                case proplists:get_value("birthday") of
                    {_CYear, _CMonth, _CDay}=CBirthday ->
                        CAge = abs((calendar:date_to_gregorian_days(date()) - calendar:date_to_gregorian_days(CBirthday)) / 365),
                        ?FMT(" and (acv_video.age_from < ~p or acv_video.age_from is NULL) and "
                             "(acv_video.age_to > ~p or acv_video.age_to is NULL) ", [CAge, CAge]);
                    EBirthday ->
                        io:format("ERROR: invalid birthday - ~p~n", [EBirthday]),
                        ""
                end
            ], " ");
        Other -> 
            ?D("ERROR: get_acv - invalid customer: ~p~n", [Other]),
            Q2S3 = Q2S2 ++ "and acv_video.user_male is NULL and acv_video.age_from is NULL and acv_video.age_to is NULL"
    end,

    Q2 = Q2S3 ++ ";",

    io:format("===============~n~p~n", [Q2]),
    {ok, Ret} = dao:simple(Q2),

    if
        length(Ret) > 0 ->
            Rand_clip = lists:nth(random:uniform(length(Ret)), Ret),
            case proplists:get_value("rerun_hours", Rand_clip, null) of
                null -> 
                    done;
                Rerun_hours when User_id =/= null ->
                    Rerun_minutes = proplists:get_value("rerun_minutes", Rand_clip, 0),
                    Rerun_seconds = (Rerun_hours*60 + Rerun_minutes)*60,
                    Localtime = erlang:localtime(),
                    Gregorian_seconds = calendar:datetime_to_gregorian_seconds(Localtime),
                    Start_time = Gregorian_seconds + Rerun_seconds,
                    DTStart = calendar:gregorian_seconds_to_datetime(Gregorian_seconds + Rerun_seconds),
                    Q3 = "insert into acv_video_shown (user_id, acv_video_id, dateshow) values (%1, %2, %3);",
                    dao:simple(Q3, [User_id, proplists:get_value("id", Rand_clip), DTStart]);
                _ -> 
                    done
            end,
            Selected_clip = [Rand_clip];
        true ->
            Selected_clip = []
    end,

    Result_string = make_acv_xml_naive(Selected_clip),

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

        biz_adv_manager:get_acv_mp4({"preroll", "5615d6c0-79c3-4a90-bf34-9d23ae78c14a.mp4", "de130992-cf98-5284-a32b-bf256d15f4e6"}, 0),

    ok.

%%%
%%% Нагрузочное тестирование
%%%

test(speed)->

    ok.

