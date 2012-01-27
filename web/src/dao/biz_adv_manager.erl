%%% @file biz_adv_manager.erl
%%%
%%%    Бизнес логика раздачи рекламных креативов.
%%%

-module(biz_adv_manager).

-export([
    test/0,
    test/1
]).


-compile(export_all).

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

get_adv({Type, Resourse, null}, Peer) when Type =:= "preroll"; Type =:= "postroll"; Type =:= "midroll"; Type =:= "pauseroll"->
    Q = <<"select mark.id from clip_mark join clip on clip_mark.clip_id = clip.id "
                                "join mark on clip_mark.mark_id = mark.id "
                                "join mark_type on mark.mark_type_id = mark_type.id "
                    "where mark_type.name=\"Categories\" and clip.url=?;">>,

    mysql:prepare(get_clip_categories, Q),
    {data,{mysql_result, Cols, Vals, _X31, _X32}} = mysql:execute(mySqlConPool, get_clip_categories, [list_to_binary(Resourse)]),

    %R2 = mysql_dao:make_proplist(Cols, Vals, []),
    io:format("------------~n~p~n", [Vals]),


    % TODO перевести дататаймы в UTC и селектить NOW аналогично в UTC
    Q2S1 = "select distinct(acv_video.id), url, datestart, datestop, ref, wish, shown "
            "from acv_video left join acv_video2cat on acv_video.id=acv_video2cat.acv_video_id "
                    "left join acv_video2geo_region on acv_video.id=acv_video2geo_region.acv_video_id "
            "where "
                "acv_video.datestart > (select NOW()) and acv_video.datestop < (select NOW()) and "
                "acv_video.wish > acv_video.shown and "
                "acv_video." ++ Type ++ " = true and "
                "acv_video.user_male is NULL and acv_video.age_from is NULL and acv_video.age_to is NULL ",
    if
        length(Vals) > 0 ->
            Q2S2 = Q2S1 ++ " and (acv_video2cat.cat_id in (" ++ 
                string:join([integer_to_list(X) || X <- lists:flatten(Vals)], ",") ++
                ") or acv_video2cat.cat_id is NULL) ";
        true ->
            Q2S2 = Q2S1 ++ " and cv_video2cat.cat_id is NULL "
    end,
    
            %" and (acv_video2geo_region.geo_region_id in (!!GEO!!!) or acv_video2geo_region.geo_region_id is NULL);"
    Q2 = Q2S2 ++ ";",

    io:format("===========~n~p~n", [Q2]),

    Ret = dao:simple(Q2),

    io:format("=-=-=-=-=-=-=-=-= ~n~p~n", [Ret]),
    ok.
%get_adv({Type, Resourse, UID}) ->
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Юнит тестирование
%%%

test1() ->
    R = {data,{mysql_result, Cols, Vals, _X31, _X32}} = mysql:fetch(mySqlConPool, <<"select * from customer where uid = 1265;">>),
    io:format("------------~n~p~n", [R]),

    R2 = mysql_dao:make_proplist(Cols, Vals, []),

    io:format("------------~n~p~n", [R2]),

    ok.
test2() ->
    R = mysql:fetch(mySqlConPool, <<"select ** from customer where uid = 1265111;">>),
    io:format("------------~n~p~n", [R]),

    ok.


test()->
    ok.

%%%
%%% Нагрузочное тестирование
%%%

test(speed)->

    ok.

