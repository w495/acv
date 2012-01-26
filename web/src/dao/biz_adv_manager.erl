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

%get_adv({Type, Resourse, null}) ->
%    R = mysql:fetch(p1, <<"DELETE FROM developer">>)
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

test()->
    ok.

%%%
%%% Нагрузочное тестирование
%%%

test(speed)->

    ok.

