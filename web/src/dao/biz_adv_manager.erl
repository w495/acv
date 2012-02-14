%%% @file biz_adv_manager.erl
%%%
%%%    Бизнес логика раздачи рекламных креативов.
%%%

-module(biz_adv_manager).

-compile(export_all).

-include("common.hrl").

% -export([
%     get_acv_ext/2,
%     get_acv_ext/3,
%     get_acv/3,
%     test/0,
%     test_dirty/0,
%     test/1
% ]).


-define(VK_STREAMER_DEFAULT, "http://192.168.2.187:8000").
-define(XML_TOP, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").
-define(DELIM, []).
-define(SPACE, " ").

-define(DEFAULT_PEER,   0).
-define(DEFAULT_USERID, null).

-define(DAYS_IN_YEAR, 365.242199 ).
    %%% $ DAYS_IN_YEAR \ne 365.0 $
    %%% $ DAYS_IN_YEAR \approx 365.242199 $
-define(MIN_IN_HOUR, 60). % спс, кеп
-define(SEC_IN_MIN,  60). % спс, кеп


-define(EXT_LEN, 4).


get_acv_ext({Type, Resourse_ext, User_id}, Peer) ->
    get_acv_ext_by_len({Type, Resourse_ext, User_id}, Peer, ?EXT_LEN).

get_acv_ext_by_len({Type, Resourse_ext, User_id}, Peer, Ext_len) ->
    {Resourse, _ } =
        lists:split(erlang:length(Resourse_ext) - Ext_len, Resourse_ext),
    get_acv({Type, Resourse, User_id}, Peer).

%%%
%%% TODO:
%%%     ПОЧЕМУ НЕ ВСЕ В ОДНОЙ ТРАНЗАКЦИИ?
%%%
get_acv({Type, Resourse, User_id}, Peer) when
    Type =:= "preroll";
        Type =:= "postroll";
            Type =:= "midroll";
                Type =:= "pauseroll"->

    ?D("Type = ~p~n",       [Type]),
    ?D("Resourse = ~p~n",   [Resourse]),
    ?D("User_id = ~p~n",    [User_id]),
    ?D("Peer = ~p~n",       [Peer]),

    %   biz_adv_manager:get_acv({{"preroll",
    %       "354b9bd8-c2aa-4a92-81ee-0fccc85a9273", null}, 0})

    Query = <<"select distinct(mark.id) from clip_mark "
                " join clip on clip_mark.clip_id = clip.id "
                " join mark on clip_mark.mark_id = mark.id "
                " join mark_type on mark.mark_type_id = mark_type.id "
                " where mark_type.name=\"Categories\" and clip.url=?;">>,

    %TODO реюзать prepare
    mysql:prepare(get_clip_categories, Query),
    {data,{mysql_result, _Cols, Vals, _X31, _X32}} =
        mysql:execute(mySqlConPool, get_clip_categories,
            [list_to_binary(Resourse)]),

    ?D("------------~nFilm (~p) categories: ~p~n", [Resourse, Vals]),

    % TODO перевести дататаймы в UTC и селектить NOW аналогично в UTC
    Q2S1 = "select distinct(acv_video.id), url, datestart, datestop, "
                "rerun_hours, rerun_minutes, "
                " ref, wish, link_title, shown, duration "
            "from acv_video left "
                " join acv_video2cat "
                    " on acv_video.id = acv_video2cat.acv_video_id "
                "left join acv_video2geo_region "
                    " on acv_video.id = acv_video2geo_region.acv_video_id "
            "where "
                " acv_video.datestart < (select NOW()) and "
                " acv_video.datestop > (select NOW()) and "
                " acv_video.wish > acv_video.shown and "
                " acv_video." ++ Type ++ " = true and "
                %%% Ограничение по времени показа
                %%% acv_video.time_from < \now(hours) < acv_video.time_to
                " (acv_video.time_from < "
                        " (select (extract( hour from  now()::time))) "
                    " or acv_video.time_from is NULL) "
                        " and "
                " ((select (extract( hour from  now()::time))) "
                        " < acv_video.time_to "
                    " or acv_video.time_to is NULL) "
                        " and "
                "not exists "
                    " (select * from acv_video_shown "
                        " where "
                            " acv_video_shown.acv_video_id = acv_video.id and "
                            " acv_video_shown.user_id='"
                            ++ utils:to_list(User_id) ++ "') ",
    if
        length(Vals) > 0 ->
            Q2S2 = Q2S1 ++ " and (acv_video2cat.cat_id in (" ++ 
                string:join([integer_to_list(X)
                    || X <- lists:flatten(Vals)], ",") ++
                ") or acv_video2cat.cat_id is NULL) ";
        true ->
            Q2S2 = Q2S1 ++ " and acv_video2cat.cat_id is NULL "
    end,

    Customer_query = <<"select * from customer where uuid=?;">>,
    mysql:prepare(get_customer, Customer_query),
    {data,{mysql_result, Customer_cols, Customer_vals, _X31, _X32}} =
        mysql:execute(mySqlConPool, get_customer, [User_id]),
    Customer_list = mysql_dao:make_proplist(Customer_cols, Customer_vals),

    ?D("--~nUser_id = ~p, Customer_list (~p) ~n", [User_id, Customer_list]),
    ?D("~n------------------------~n", []),
    ?D("Q2S2 = ~p", [Q2S2]),
    ?D("~n------------------------~n", []),

    %%% Таргетирование по параметрам кастомера.
    case Customer_list of
        [] ->
            Q2S3 = Q2S2 ++ " and acv_video.user_male is NULL and "
                " acv_video.age_from is NULL and acv_video.age_to is NULL";
        [Customer] ->
            Q2S3 = string:join([
                Q2S2,
                %%% Обработка пола.
                case proplists:get_value("gender", Customer) of
                    "male" ->
                        " and (acv_video.user_male=true "
                            " or acv_video.user_male is NULL) ";
                    "female" ->
                        " and (acv_video.user_male=false "
                            " or acv_video.user_male is NULL) ";
                    Other_gender ->
                        ?E("ERROR: invalid gender - ~p~n", [Other_gender]),[]
                end,
                %%% Обработка возрасата.
                case proplists:get_value("birthday", Customer) of
                    {_year, _month, _day}=Customer_birthday ->
                        ?D("~n[!]~n[!]", []),
                        ?D("~n[!]Customer_birthday = ~p", [Customer_birthday]),
                        Customer_age = customer_age(Customer_birthday),
                        ?D("~n[!]Customer_age = ~p", [Customer_age]),
                        ?D("~n[!]~n[!]", []),
                        ?FMT( %%% acv_video.age_from < ~p < acv_video.age_to
                            " and ( acv_video.age_from < ~p "
                                " or acv_video.age_from is NULL)"
                            " and (  ~p < acv_video.age_to "
                                " or acv_video.age_to is NULL) ",
                            [Customer_age, Customer_age]);
                    Wrong_birthday ->
                        ?E("ERROR: invalid birthday - ~p~n", [Wrong_birthday]),
                        []
                end
            ], " ");
        Other -> 
            ?E("ERROR: get_acv - invalid customer: ~p~n", [Other]),
            Q2S3 = Q2S2 ++ " and acv_video.user_male is NULL and "
                "acv_video.age_from is NULL and acv_video.age_to is NULL"
    end,
    Q2 = Q2S3 ++ ";",
    ?D("===============~n~p~n", [Q2]),
    {ok, Acv_videos} = dao:simple(Q2),
    if
        length(Acv_videos) > 0 ->
            Random_acv_video = utils:random_nth(Acv_videos),
            case proplists:get_value("rerun_hours", Random_acv_video, null) of
                null ->
                    ?D("~n~n~ndone = no rerun_hours ~n~n~n", []),
                    done;
                Rerun_hours when User_id =/= null ->
                    Rerun_minutes =
                        proplists:get_value("rerun_minutes",Random_acv_video,0),
                    Start_datetime = start_datetime(Rerun_hours, Rerun_minutes),
                    Q3 =    "insert into acv_video_shown "
                                "(user_id, acv_video_id, dateshow) "
                            " values ($1, $2, $3);",
                    Return = dao:simple(Q3, [User_id,
                        proplists:get_value("id", Random_acv_video),
                            Start_datetime]),
                    ?D("~n~n~nReturn = ~p~n~n~n", [Return]),
                    done;
                _ -> 
                    done
            end,
            Selected_clip = [Random_acv_video];
        true ->
            Selected_clip = []
    end,
    Result_string = make_acv_xml(Selected_clip),
    ?D("~nType = ~p, Result_string  = ~s~n", [Type, Result_string]),
    Result_string.

%%% 
%%% @spec
%%% customer_age({Year::integer(), Month::integer(), Day::integer()})
%%%     ->  float().
%%% @doc
%%% Вычиляет возраст на основании даты рождения
%%%
customer_age(Customer_birthday) ->
    Now = calendar:date_to_gregorian_days(erlang:date()),
    Birthday = calendar:date_to_gregorian_days(Customer_birthday),
    {Year, Month, Day} =
        calendar:gregorian_days_to_date(erlang:abs(Now  - Birthday)),
    Year.


%%% 
%%% @spec
%%% start_datetime({Hours::integer(), Minutes::integer()) -> integer().
%%% @doc
%%% Количество секунд на основании текущего времени
%%%     и Rerun_hours, Rerun_minutes
%%%
start_datetime(Rerun_hours, Rerun_minutes) ->
    Rerun_seconds = (Rerun_hours*?MIN_IN_HOUR + Rerun_minutes)*?SEC_IN_MIN,
    Localtime = erlang:localtime(),
    Localtime_seconds = calendar:datetime_to_gregorian_seconds(Localtime),
    calendar:gregorian_seconds_to_datetime(Localtime_seconds + Rerun_seconds).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% @doc
%%% Возвращает строку с XML для плеера.
%%% В этой строки находится полное описание рекламы.
%%%
make_acv_xml(List) ->
    {Acc_str, Acc_duration} = make_acv_xml_item(List, {[], 0}),
    ?XML_TOP ++
    ?FMT("<block duration=\"~p\" loadnext=\"600\">", [Acc_duration]) ++
        Acc_str ++
    "</block>".

%%%
%%% @doc
%%% Возвращает строку с XML для плеера.
%%% В этой строки находится полное описание рекламы.
%%%
make_acv_xml_item([], Acc) -> Acc;
make_acv_xml_item([Creative|Tail], {Acc_str, Acc_duration})->
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
% 
% %%%
% %%% Юнит тестирование
% %%%
% 
% test_some_user() ->
%     Query = {data,{mysql_result, Cols, Vals, _X31, _X32}} =
%         mysql:fetch(mySqlConPool,
%             <<"select * from customer where uid = 1265;">>),
% 
%     % Proplists = [[{"uuid","a0144f36-4158-11df-affb-003048d95bc0"},
%     %               {"description",null},
%     %               {"publish_profile",1},
%     %               {"admin_rights",null},
%     %               {"mobile_number",null},
%     %               {"icq",null},
%     %               {"website",null},
%     %               {"city",null},
%     %               {"country",null},
%     %               {"birthday",{1983,3,9}},
%     %               {"regkey","6ccfc59db3ab213589f1f53e3d2c0e8b"},
%     %               {"company",null},
%     %               {"is_modified",null},
%     %               {"gender","female"},
%     %               {"name","lina"},
%     %               {"nickname","besttranslations@mail.ru"},
%     %               {"created",{{2010,4,6},{12,44,34}}},
%     %               {"uid",1265}]]
% 
%     ?D("TEST_FEMALE-----------~nQuery = ~p~n", [Query]),
%     Proplists = mysql_dao:make_proplist(Cols, Vals, []),
%     ?D("TEST_FEMALE-----------~nProplists = ~p~n", [Proplists]),
%     [Proplist] = Proplists,
%     proplists:get_value("uuid", Proplist).
% 
% %%%
% %%% Возвращает uuid случайного мужика
% %%%
% test_random_male() ->
%     {data,{mysql_result, Cols, Vals, _X31, _X32}} =
%         mysql:fetch(mySqlConPool,
%             <<"select * from customer where gender = \"male\" limit 1000;">>),
%     Proplists = mysql_dao:make_proplist(Cols, Vals, []),
%     Proplist = utils:random_nth(Proplists),
%     proplists:get_value("uuid", Proplist).
% 
% %%%
% %%% Возвращает uuid случайной бабы
% %%%
% test_random_female() ->
%     {data,{mysql_result, Cols, Vals, _X31, _X32}} =
%         mysql:fetch(mySqlConPool,
%             <<"select * from customer where gender = \"female\" limit 1000;">>),
%     Proplists = mysql_dao:make_proplist(Cols, Vals, []),
%     Proplist = utils:random_nth(Proplists),
%     proplists:get_value("uuid", Proplist).
% 
% %%%
% %%% Возвращает uuid случайного неопределившегося
% %%%
% test_random_it() ->
%     {data,{mysql_result, Cols, Vals, _X31, _X32}} =
%         mysql:fetch(mySqlConPool,
%             <<"select * from customer where gender = \"none\" limit 1000;">>),
%     Proplists = mysql_dao:make_proplist(Cols, Vals, []),
%     Proplist = utils:random_nth(Proplists),
%     proplists:get_value("uuid", Proplist).
% 
% %%%
% %%% Возвращает uuid случайного ребенка
% %%%
% test_random_child() ->
%     {data,{mysql_result, Cols, Vals, _X31, _X32}} =
%         mysql:fetch(mySqlConPool,
%             <<"select * from customer where date(birthday) "
%                 " between date_sub(date(now()), interval 18 year) "
%                     " and date(now()) limit 1000;">>),
% 
% 
%     Proplists = mysql_dao:make_proplist(Cols, Vals, []),
%     Proplist = utils:random_nth(Proplists),
%     ?D("Proplist = ~p~n", [Proplist]),
%     proplists:get_value("uuid", Proplist).
% 
% %%%
% %%% Возвращает uuid случайного взрослого
% %%%
% test_random_adult() ->
%     {data,{mysql_result, Cols, Vals, _X31, _X32}} =
%         mysql:fetch(mySqlConPool,
%             <<"select *  from customer where date(birthday) "
%                 " between date_sub(date(now()), interval 100 year) "
%                     " and date_sub(date(now()), interval 18 year)  "
%                         " limit 1000;">>),
%     Proplists = mysql_dao:make_proplist(Cols, Vals, []),
%     Proplist = utils:random_nth(Proplists),
%     proplists:get_value("uuid", Proplist).
% 
% %%%
% %%% Возвращает uuid случайного нежитя (родился до Революции)
% %%%
% test_random_undead() ->
%     {data,{mysql_result, Cols, Vals, _X31, _X32}} =
%         mysql:fetch(mySqlConPool,
%             <<"select * from customer "
%                 " where year(birthday) <  1917 limit 1000;">>),
%     Proplists = mysql_dao:make_proplist(Cols, Vals, []),
%     Proplist = utils:random_nth(Proplists),
%     proplists:get_value("uuid", Proplist).
% 
% %%%
% %%% Возвращает uuid случайного пришельца из будущего
% %%%
% test_random_fromfuture() ->
%     {data,{mysql_result, Cols, Vals, _X31, _X32}} =
%         mysql:fetch(mySqlConPool,
%             <<"select * from customer "
%                 " where date(birthday) >  date(now()) limit 1000;">>),
%     Proplists = mysql_dao:make_proplist(Cols, Vals, []),
%     Proplist = utils:random_nth(Proplists),
%     proplists:get_value("uuid", Proplist).
% 
% %%%
% %%% test_x$n$ (Datestart::{date(), time()}, Datestop::{date(), time()},
% %%%     RandomInt::integer()) -> Result::proplist().
% %%%
% 
% test_x0_example(_datestart, _datestop, _micro_sec) ->
%     %%% интерфейс ужасен, но функция будет переписываться,
%     %%%     и потом это просто тестирование.
%     [
%         {"original", []},
%         {"preroll",  []},
%         {"postroll", []},
%         {"midroll",  []}
%     ].
% 
% %%%
% %%% Тестирует XML первого фильма, первой категории.
% %%% Производит проверку таргетирования по категорям.
% %%% 
% %%% Создаем фильм и проверяем его по возвращаемому XML.
% %%% 
% test_x1_cat(Datestart, Datestop, Micro_sec) ->
%     {ok, [Cat| _ ]} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", Cat),
%     %%%     dao_acv_video:update_acv_video({{Acv_video_id,
%     %%%         Name_new, Datestart, Datestop, Url, Ref, Wish,
%     %%%             Postroll, Preroll, Midroll, Pauseroll, User_male,
%     %%%                 Age_from, Age_to, Time_from, Time_to,
%     %%%                     Duration, Link_title, Alt_title, Comment,
%     %%%                         Rerun_hours, Rerun_minutes,
%     %%%                             Customer_id}, R_list_new, []}),
%     {ok, Acv_video_id} = dao_acv_video:update_acv_video({{null,
%         "Name-test-1-c-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, null,
%                 null, null, null, null,
%                     1, "test_x1_cat", "test_x1_cat", "Comment",
%                         1, 1,
%                             null}, [], [Cat_id]}),
%     {ok, [Film| _ ]}  = dao_cat:get_clips_url(Cat_id),
%     Film_url = proplists:get_value("url", Film),
%     {ok, Acv_videos} = dao_acv_video:get_acv_video(Acv_video_id),
%     Result_original = make_acv_xml(Acv_videos),
%     Result_preroll    =
%         biz_adv_manager:get_acv({"preroll", Film_url,?DEFAULT_USERID},
%             ?DEFAULT_PEER),
%     Result_postroll   =
%         biz_adv_manager:get_acv({"postroll",Film_url,?DEFAULT_USERID},
%             ?DEFAULT_PEER),
%     Result_midroll    =
%         biz_adv_manager:get_acv({"midroll",Film_url,?DEFAULT_USERID},
%             ?DEFAULT_PEER),
%     ?D("~n[Cat_id = ~p, Film_url ~p]~n", [Cat_id, Film_url]),
%     [
%         {"original", Result_original},
%         {"preroll", Result_preroll},
%         {"postroll", Result_postroll},
%         {"midroll", Result_midroll}
%     ].
% 
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% %%% X2
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% %%%
% %%% Тестирует XML случайного фильма, случайной категории.
% %%% Производит проверку таргетирования по категорям.
% %%% Пока обрабатывает только ПОЛОЖИТЕЛЬНУЮ ситуацию.
% %%%
% test_x2_cat(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} =
%         test_x2_query(Datestart, Datestop, Micro_sec, [Cat_id]),
%     handle_random_cat(Cat_id, Acv_video_id).
% 
% %%%
% %%% Тестирует XML случайного фильма, случайной категории
% %%% Категории при этом не учитываются.
% %%% Пока обрабатывает только ПОЛОЖИТЕЛЬНУЮ ситуацию.
% %%% 
% test_x2_uncat(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x2_query(Datestart, Datestop, Micro_sec, []),
%     handle_random_cat(Cat_id, Acv_video_id).
% 
% %%%
% %%% Возвращает видео (образец)
% %%%
% test_x2_query(Datestart, Datestop, Micro_sec, Cats)->
%     dao_acv_video:update_acv_video({{null,
%         "Name-test-2-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, null,
%                 null, null, null, null,
%                     1, "test_x2_query", "test_x2_query", "Comment",
%                         1, 1,
%                             null}, [], Cats}).
% 
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% %%% X3
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% %%%
% %%% 
% %%% 
% %%% Пока обрабатывает только ПОЛОЖИТЕЛЬНУЮ ситуацию.
% %%% Только для мужиков
% %%%
% test_x3_male(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x3_male_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_male()).
% 
% %%%
% %%%
% %%%
% %%% Пока обрабатывает только ПОЛОЖИТЕЛЬНУЮ ситуацию.
% %%% Только для мужиков
% %%%
% test_x3_male_anti(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x3_male_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_female()).
% 
% %%%
% %%%
% %%%
% %%% обрабатывает только ОТРИЦАТЕЛЬНУЮ ситуацию.
% %%% Только для мужиков
% %%% Пришел пользователь с полом none
% %%%
% test_x3_male_anti_it(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x3_male_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_it()).
% 
% %%%
% %%%
% %%%
% %%% Пока обрабатывает только ПОЛОЖИТЕЛЬНУЮ ситуацию.
% %%% Только для баб
% %%%
% test_x3_female(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),  
%     {ok, Acv_video_id} = test_x3_female_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_female()).
% 
% %%%
% %%%
% %%%
% %%% Пока обрабатывает только ОТРИЦАТЕЛЬНУЮ ситуацию.
% %%% Только для баб
% %%%
% test_x3_female_anti(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x3_female_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_male()).
% 
% %%%
% %%%
% %%%
% %%% обрабатывает только ОТРИЦАТЕЛЬНУЮ ситуацию.
% %%% Только для баб
% %%% Пришел пользователь с полом none
% %%%
% test_x3_female_anti_it(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x3_female_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_it()).
% 
% %%%
% %%% Возвращает видео (образец) для неопределенного рода.
% %%%
% test_x3_it_query(Datestart, Datestop, Micro_sec)->
%     dao_acv_video:update_acv_video({{null,
%         "Name-test-3-it-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, null,
%                 null, null, null, null,
%                     1, "test_x3_it_query", "test_x3_it_query", "Comment",
%                         1, 1,
%                             null}, [], []}).
% %%%
% %%% Возвращает видео (образец) для мужского рода.
% %%%
% test_x3_male_query(Datestart, Datestop, Micro_sec)->
%     dao_acv_video:update_acv_video({{null,
%         "Name-test-3-male-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, true,
%                 null, null, null, null,
%                     1, "test_x3_male_query", "test_x3_male_query", "Comment",
%                         1, 1,
%                             null}, [], []}).
% %%%
% %%% Возвращает видео (образец) для женского рода.
% %%%
% test_x3_female_query(Datestart, Datestop, Micro_sec)->
%     dao_acv_video:update_acv_video({{null,
%         "Name-test-3-female-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, false,
%                 null, null, null, null,
%                     1, "test_x3_female_query", "test_x3_female_query", "Comment",
%                         1, 1,
%                             null}, [], []}).
% 
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% %%% X4
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% %%%
% %%% обрабатывает только ПОЛОЖИТЕЛЬНУЮ ситуацию.
% %%% Только для детей
% %%%
% test_x4_child(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x4_child_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_child()).
% 
% %%%
% %%%
% %%%
% %%% обрабатывает только ОТРИЦАТЕЛЬНУЮ ситуацию.
% %%% Только для детей
% %%%
% test_x4_child_anti(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x4_child_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_adult()).
% 
% 
% test_x4_child_query(Datestart, Datestop, Micro_sec)->
%     dao_acv_video:update_acv_video({{null,
%         "Name-test-4-child-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, null,
%                 0, 20, null, null,
%                     1, "test_x4_child_query", "test_x4_child_query", "Comment",
%                         1, 1,
%                             null}, [], []}).
% 
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% %%% X4
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% %%%
% %%% обрабатывает только ПОЛОЖИТЕЛЬНУЮ ситуацию.
% %%% Только для взрослых
% %%%
% test_x4_adult(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x4_adult_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_adult()).
% 
% %%%
% %%%
% %%%
% %%% обрабатывает только ОТРИЦАТЕЛЬНУЮ ситуацию.
% %%% Только для взрослых
% %%%
% test_x4_adult_anti(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x4_adult_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id, test_random_child()).
% 
% test_x4_adult_query(Datestart, Datestop, Micro_sec)->
%     dao_acv_video:update_acv_video({{null,
%         "Name-test-4-adult-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, null,
%                 25, 100, null, null,
%                     1, "test_x4_adult_query", "test_x4_adult_query", "Comment",
%                         1, 1,
%                             null}, [], []}).
% 
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% %%% X5
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% %%%
% %%% обрабатывает только ПОЛОЖИТЕЛЬНУЮ ситуацию.
% %%% Только для взрослых
% %%%
% test_x5_now(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x5_now_query(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id).
% 
% test_x5_now_anti(Datestart, Datestop, Micro_sec) ->
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     {ok, Acv_video_id} = test_x5_now_query_anti(Datestart, Datestop, Micro_sec),
%     handle_random_cat(Cat_id, Acv_video_id).
% 
% test_x5_now_query(Datestart, Datestop, Micro_sec)->
%     {Init_hours, _minutes, _seconds} = erlang:time(),
%     From_hours = Init_hours - 1,
%     To_hours = Init_hours + 1,
%     dao_acv_video:update_acv_video({{null,
%         "Name-test-5-now-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, null,
%                 null, null, From_hours, To_hours,
%                     1, "test_x5_now_query", "test_x5_now_query", "Comment",
%                         1, 1,
%                             null}, [], []}).
% 
% test_x5_now_query_anti(Datestart, Datestop, Micro_sec)->
%     {Init_hours, _minutes, _seconds} = erlang:time(),
%     From_hours = Init_hours + 2,
%     To_hours = Init_hours + 3,
%     dao_acv_video:update_acv_video({{null,
%         "Name-test-5-now-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, null,
%                 null, null, From_hours, To_hours,
%                     1, "test_x5_now_query", "test_x5_now_query", "Comment",
%                         1, 1,
%                             null}, [], []}).
% 
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% %%% X6
% %%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% 
% test_x6_now_query(Datestart, Datestop, Micro_sec)->
%     dao_acv_video:update_acv_video({{null,
%         "Name-test-6-now-" ++ erlang:integer_to_list(Micro_sec),
%             Datestart, Datestop, "http://ya.ru",
%             "static/data/acv-video/test/tvzavr-01.mp4", 100,
%             true, true, true, true, null,
%                 null, null, null, null,
%                     1, "test_x6_now_query", "test_x6_now_query", "Comment",
%                         0, 1,
%                             null}, [], []}).
% 
% 
% test_simple_x6() ->
%     test_truncate(),
%     {Macro_sec, Normal_sec, Micro_sec} = erlang:now(),
%     Datestart = calendar:now_to_universal_time
%         ({Macro_sec - 1, Normal_sec, Micro_sec}),
%     Datestop = calendar:now_to_universal_time
%         ({Macro_sec + 1, Normal_sec, Micro_sec}),
%     {ok, Cats} = dao_cat:get_all_cats([]),
%     Cat_id = proplists:get_value("id", utils:random_nth(Cats)),
%     Random_adult = test_random_adult(),
%     {ok, Acv_video_id} = test_x6_now_query(Datestart, Datestop, Micro_sec),
%     ?D("1", []),
%     Result_1 = handle_random_cat(Cat_id, Acv_video_id, Random_adult),
%     case proplists:get_value("film_url", Result_1) of
%         nil -> test_simple_x6();
%         _ ->
%             ?assertEqual(proplists:get_value("original", Result_1),
%                 proplists:get_value("preroll",  Result_1)),
%             ok
%     end,
%     Dbg_res = dao:simple("select * from acv_video_shown;"),
%     ?D("~n test_simple_x6 --> 1 passed ~p", [Dbg_res ]),
%     ?D("~n test_simple_x6 --> 1 passed ~n~n", []),
%     %timer:sleep(32000),
%     ?D("2", []),
%     Result_2 = handle_random_cat(Cat_id, Acv_video_id, Random_adult),
%     ?D("~n~n~nResult_2 = ~p ~n~n~n", [Result_2]),
%     case proplists:get_value("film_url", Result_2) of
%         nil -> test_simple_x6();
%         _ ->
%             ?assertNotEqual(proplists:get_value("original", Result_2),
%                 proplists:get_value("preroll",  Result_2)),
%             ok
%     end,
%     ?D("~n test_simple_x6 --> 2 passed ~n~n", []),
%     spawn(fun() ->
%         timer:sleep(32000),
%         dao_acv_video:delete_acv_video_shown_expired(),
%         ?D("3", []),
%         Result_3 = handle_random_cat(Cat_id, Acv_video_id, Random_adult),
%         case proplists:get_value("film_url", Result_3) of
%             nil -> test_simple_x6();
%             _ ->
%                 ?D("~n~n +++ ~n~n", []),
%                 Result_31 = proplists:get_value("original", Result_3),
%                 Result_32 = proplists:get_value("preroll",  Result_3),
%                 ?D("~n~n ~p ~n~n ~p ~n~n", [Result_31, Result_32]),
%                 ?assertEqual(Result_31, Result_32),
%                 ok
%         end,
%         ?D("~n test_simple_x6 --> 3 passed ~n~n", []),
%         ok
%     end),
%     ok.
% 
% %%% ----------------------------------------------------------------------
% %%% Обрабатывает случайную категорию,
% %%%     возвращает набор XML для случайного фильма
% %%% ----------------------------------------------------------------------
% 
% handle_random_cat(Cat_id, Acv_video_id) ->
%     {ok, Films}  = dao_cat:get_clips_url(Cat_id),
%     case dao_cat:get_clips_url(Cat_id) of
%         {ok, []} -> % Нет фильма --- нет xml.
%             Film_url = nil,
%             Result_original  = [],
%             Result_preroll   = [],
%             Result_postroll  = [],
%             Result_midroll   = [];
% 
%         {ok, Films} ->
%             Film_url = proplists:get_value("url", utils:random_nth(Films)),
%             {ok, Acv_videos} = dao_acv_video:get_acv_video(Acv_video_id),
%             Result_original = make_acv_xml(Acv_videos),
%             Result_preroll    =
%                 get_acv({"preroll",Film_url, ?DEFAULT_USERID}, ?DEFAULT_PEER),
%             Result_postroll   =
%                 get_acv({"postroll",Film_url, ?DEFAULT_USERID}, ?DEFAULT_PEER),
%             Result_midroll    =
%                 get_acv({"midroll",Film_url, ?DEFAULT_USERID}, ?DEFAULT_PEER),
%             ?D("~n[Cat_id = ~p, Film_url ~p]~n", [Cat_id, Film_url])
%     end,
%     [
%         {"cat_id",   Cat_id},
%         {"film_url", Film_url},
%         {"original", Result_original},
%         {"preroll",  Result_preroll},
%         {"postroll", Result_postroll},
%         {"midroll",  Result_midroll}
%     ].
% 
% handle_random_cat(Cat_id, Acv_video_id, Genger) ->
%     {ok, Films}  = dao_cat:get_clips_url(Cat_id),
%     case dao_cat:get_clips_url(Cat_id) of
%         {ok, []} -> % Нет фильма --- нет xml.
%             Film_url = nil,
%             Result_original  = [],
%             Result_preroll   = [],
%             Result_postroll  = [],
%             Result_midroll   = [];
% 
%         {ok, Films} ->
%             Film_url = proplists:get_value("url", utils:random_nth(Films)),
%             {ok, Acv_videos} = dao_acv_video:get_acv_video(Acv_video_id),
%             Result_original = make_acv_xml(Acv_videos),
%             Result_preroll    =
%                 biz_adv_manager:get_acv({"preroll",Film_url,  Genger},
%                     ?DEFAULT_PEER),
%             Result_postroll   =
%                 biz_adv_manager:get_acv({"postroll",Film_url, Genger},
%                     ?DEFAULT_PEER),
%             Result_midroll    =
%                 biz_adv_manager:get_acv({"midroll",Film_url,  Genger},
%                     ?DEFAULT_PEER),
%             ?D("~n[Cat_id = ~p, Film_url ~p]~n", [Cat_id, Film_url])
%     end,
%     [
%         {"cat_id",   Cat_id},
%         {"film_url", Film_url},
%         {"original", Result_original},
%         {"preroll",  Result_preroll},
%         {"postroll", Result_postroll},
%         {"midroll",  Result_midroll}
%     ].
% 
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%                                                                        %%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% %%%
% %%% @spec test_simple(Test_function::fun/3) -> ok.
% %%% Функция юнит тестирования
% %%%
% test_simple(Test_function) ->
%     test_truncate(),
%     {Macro_sec, Normal_sec, Micro_sec} = erlang:now(),
%     Datestart = calendar:now_to_universal_time
%         ({Macro_sec - 1, Normal_sec, Micro_sec}),
%     Datestop = calendar:now_to_universal_time
%         ({Macro_sec + 1, Normal_sec, Micro_sec}),
%     Result = Test_function(Datestart, Datestop, Micro_sec),
%     case proplists:get_value("film_url", Result) of
%         nil -> test_simple(Test_function);
%         _ ->
%             ?assertEqual(proplists:get_value("original", Result),
%                 proplists:get_value("preroll",  Result)),
%             ?assertEqual(proplists:get_value("original", Result),
%                 proplists:get_value("postroll", Result)),
%             ?assertEqual(proplists:get_value("original", Result),
%                 proplists:get_value("midroll",  Result)),
%             ok
%     end.
% 
% %%%
% %%% @spec test_simple(Test_function::fun/3) -> ok.
% %%% Функция юнит тестирования
% %%%
% test_simple_anti(Test_function) ->
%     test_truncate(),
%     {Macro_sec, Normal_sec, Micro_sec} = erlang:now(),
%     Datestart = calendar:now_to_universal_time
%         ({Macro_sec - 1, Normal_sec, Micro_sec}),
%     Datestop = calendar:now_to_universal_time
%         ({Macro_sec + 1, Normal_sec, Micro_sec}),
%     Result = Test_function(Datestart, Datestop, Micro_sec),
%     case proplists:get_value("film_url", Result) of
%         nil -> test_simple_anti(Test_function);
%         _ ->
% %            ?assertNotEqual(proplists:get_value("original", Result),
% %                proplists:get_value("preroll",  Result)),
% %            ?assertNotEqual(proplists:get_value("original", Result),
% %                proplists:get_value("postroll", Result)),
% %            ?assertNotEqual(proplists:get_value("original", Result),
% %                proplists:get_value("midroll",  Result)),
%             ok
%     end.
% 
% %%%
% %%% @spec test_simple(Test_function_1::fun/3, Test_function_2::fun/3) -> ok.
% %%% Функция юнит тестирования
% %%%
% test_simple(Test_function_1, Test_function_2) ->
%     test_truncate(),
%     {Macro_sec, Normal_sec, Micro_sec} = erlang:now(),
%     Datestart = calendar:now_to_universal_time
%         ({Macro_sec - 1, Normal_sec, Micro_sec}),
%     Datestop = calendar:now_to_universal_time
%         ({Macro_sec + 1, Normal_sec, Micro_sec}),
%     Result_1 = Test_function_1(Datestart, Datestop, Micro_sec),
%     case proplists:get_value("film_url", Result_1) of
%         nil -> test_simple(Test_function_1, Test_function_2);
%         _ ->
%             Result_2 = Test_function_2(Datestart, Datestop, Micro_sec),
%             case proplists:get_value("film_url", Result_2) of
%                 nil -> test_simple(Test_function_1, Test_function_2);
%                 _ ->
%                     ?assertEqual(proplists:get_value("original", Result_1),
%                         proplists:get_value("preroll",  Result_2)),
%                     ?assertEqual(proplists:get_value("original", Result_1),
%                         proplists:get_value("postroll", Result_2)),
%                     ?assertEqual(proplists:get_value("original", Result_1),
%                         proplists:get_value("midroll",  Result_2)),
%                     ok
%             end
%     end.
% 
% 
% test_truncate() ->
%     %%% truncate or not truncate that is the question
%     dao:simple("truncate table acv_video cascade;", []),
%     ok.
% 
% %%% 
% %%% Тестирует:
% %%%     + показ рекламы без таргетирования
% %%%     + таргетирование по одной категории.
% %%% 
% test_cats()->
%     ok = test_simple(fun test_x1_cat/3),  % Первый фильм первая категория.
%     ok = test_simple(fun test_x2_cat/3),  % Случайный фильм случайная категория.
%     ok = test_simple(fun test_x2_uncat/3),% Случайный фильм нет категорий.
%     ok = test_simple(fun test_x2_uncat/3, fun test_x2_uncat/3),
%         % Два случайных фильма нет категорий.
%     % ok = test_simple(fun test_x2_uncat/3, fun test_x1_cat/3),
%     %     % Случайный фильм нет категорий + Первый фильм первая категория.
%     ok.
% 
% %%%
% %%% Тестирует:
% %%%     + таргетирование по полу кастомера
% %%%     + таргетирование по возрасту кастомера
% %%% 
% test_users()->
%     ok = test_simple(fun test_x3_male/3),   % показываем только МУЖИКАМ
%     ok = test_simple(fun test_x3_female/3), % показываем только БАБАМ
%     ok = test_simple(fun test_x3_male_anti_it/3),
%         % показываем как будто мужикам, неопределившимся
%     ok = test_simple(fun test_x3_female_anti_it/3),
%         % показываем как будто бабам, неопределившимся
%     ok = test_simple_anti(fun test_x3_male_anti/3),
%         % показываем ТОЛЬКО мужикам, но не бабам
%     ok = test_simple_anti(fun test_x3_female_anti/3),
%         % показываем ТОЛЬКО бабам, но не мужикам
%     ok = test_simple(fun test_x4_child/3), % показываем детям
%     ok = test_simple(fun test_x4_adult/3), % показываем взрослым
%     ok = test_simple_anti(fun test_x4_child_anti/3), % не показываем детям
%     ok = test_simple_anti(fun test_x4_adult_anti/3), % не показываем взрослым
% 
%     ok.
% 
% %%%
% %%% Тестирует:
% %%%     + показ рекламы в указанное время
% %%%     + показ рекламы не чаще чем заказано
% %%%
% test_time_and_times()->
% 
%     %ok = test_simple(fun test_x5_now/3),
%         % показ рекламы в указанное время
%     %ok = test_simple_anti(fun test_x5_now_anti/3),
%         % не показ рекламы иное время
%     ok = test_simple_x6(),
% 
%     ok.
% 
% %%%
% %%% Основная функция юнит тестирования
% %%%
% test_dirty() -> 
% %     Параметры рекламной кампании для проверки:
% 
% %     -- показ рекламы не более чем было заказано
% %               (требует доработки модуля статистики)
% %     -- таргетирование по нескольким категориям видео.
% %     -- таргетирование по местоположению кастомера
% %     -- проверка места размещения ролика
% %           (преролл, мидролл, постролл) - выставить один из них.
% 
%     %ok = test_cats(),
%     %ok = test_users(),
%     ok = test_time_and_times(),
% 
%     test_truncate(),
% 
%     ok.
% 
% test()-> 
%     ok.
% 
% %%%
% %%% Нагрузочное тестирование
% %%%
% 
% test(speed)->
% 
%     ok.
% 

