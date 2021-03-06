%%% @file dao_acv_video.erl
%%%
%%%    Администрирование роликов.
%%%

-module(dao_acv_video).

-export([
    get_all_acv_videos/1,
    get_all_acv_video_stats/1,
    get_acv_videos/1,
    get_acv_video_stats/1,
    get_acv_video/1,
    get_acv_video_common/1,
    get_acv_video_show/1,
    get_acv_video_upload/1,
    get_acv_video_cats/1,
    get_acv_video_geos/1,
    get_acv_video_users/1,
    update_acv_video/1,
    delete_acv_video/1,

    start_acv_video/1,
    stop_acv_video/1,

    get_owner/1,
    is_owner/2,

    activate_acv_video/1,
    disactivate_acv_video/1,
    chstate_acv_video/1,

    mkbill/1,
    paybill/1,
    bill/1,

    full_delete_acv_video/1,
    delete_acv_video_shown_expired/0,
    stop_old_acv_video/0,
    delete_acv_video_shown_expired/1,
    test_acv_video/0,
    test_eunit_1/0,
    test_eunit_2/0,
    test/0,
    test/1
]).


-compile(export_all).



-include("common.hrl").

-type proplist() :: [{term(), term()}].
-type date() :: {integer(), integer(), integer()}.
-type time() :: {integer(), integer(), float()}.
-type datetime() :: {date(), time()}.

-type acv_video() :: {
    %%% Поля acv_video:
        Id              ::integer()|null,
        Name            ::string(),
        Datestart       ::datetime(),
        Datestop        ::datetime(),
        Url             ::string(),
        Ref             ::string(),
        Wish            ::integer(),
        Postroll        ::boolean()|null,
        Preroll         ::boolean()|null,
        Midroll         ::boolean()|null,
        Pauseroll       ::boolean()|null,
        User_male       ::boolean()|null,
        Age_from        ::integer()|null,
        Age_to          ::integer()|null,
        Time_from       ::integer()|null,
        Time_to         ::integer()|null,
        Duration        ::integer(),
        Link_title      ::string(),
        Alt_title       ::string(),
        % Active           ::integer(),
            % это поле может редактировать только модератор
        % Shown           ::integer(),
            % это поле покупатель менять не может
        Comment         ::boolean(),
        Rerun_hours     ::integer()|null,
        Rerun_minutes   ::integer()|null,
        Customer_id     ::integer()
}.

-spec get_all_acv_videos(any()) -> [proplist()].
-spec get_acv_videos(Customer_id::integer()) -> [proplist()].

-spec get_acv_video(Acv_video_id::integer()) -> [proplist()];
    ({Acv_video_id::integer(), _, _, _, _}) -> [proplist()].

-spec update_acv_video(Acv_video::acv_video()) -> [proplist()];
    ({acv_video(), [Geo_region_list::integer()],
                        [Cat_id_list::integer()]}) -> integer().

%%% @doc
%%% Возвращает список proplist относящийся к acv_video с заданным State.
%%% Где State --- это tuple с первым элементом равном Id.
%%%
get_acv_video(State) when erlang:is_tuple(State) ->
    get_acv_video(erlang:element(1, State));

%%% @doc
%%% Возвращает список proplist относящийся к acv_video с заданным id
%%%
get_acv_video(Id) ->
    Q1 = "select acv_video.*, customer.email, customer.login from acv_video join customer on acv_video.customer_id = customer.id where acv_video.id = $1;",
    Q2 = "select cat_id from acv_video2cat where acv_video_id = $1;",
    {ok, R1} = dao:simple(Q1, [Id]),
    R2 = dao:with_connection_fk(fun(Con) ->
        {ok, _, Vals} = dao:equery(Con, Q2, [Id]),
        [utils:to_list(X) || {X} <- Vals]
    end),
    case R2 of
        [] ->
            R3 = [];
        _ ->
            Q3 = list_to_binary(?FMT("select mark.name, mark.seo_alias from mark where id in (~s);", [string:join(R2,", ")])),
            {ok, R3} = mysql_dao:simple(Q3),
            done
    end,
    Q4 = "select geo_region.name_ru, geo_region.code "
            "from geo_region join acv_video2geo_region on geo_region.id = acv_video2geo_region.geo_region_id "
            "where acv_video2geo_region.acv_video_id = $1;",
    {ok, R4} = dao:simple(Q4, [Id]),
    {ok, R1, R3, R4}.

%%% @doc
%%% Возвращает список всех acv_video для всех покупателей
%%% 
get_all_acv_videos(_) ->
    Query =
        "select "
            " customer.login, "
            " customer.organization, "
            " acv_video.id, "
            " acv_video.name, "
            " acv_video.active, "
            " acv_video.stoped, "
            " acv_video.comment, "
            " acv_video.shown, "
            " acv_video.clicks, "
            " acv_video.datestart, "
            " acv_video.datestop "
        " from acv_video "
            " join customer on "
                " acv_video.customer_id = customer.id "
                " and acv_video.deleted = false;",
    dao:simple(Query).

%%% @doc
%%% Возвращает список всех acv_video для всех покупателей
%%% 
get_all_acv_video_stats({Fromdate, Todate}) ->
    Query = "select acv_video.id, acv_video.name, acv_video.datestart, acv_video.datestop, acv_video.shown, acv_video.clicks "
                "from acv_video where deleted = false and "
                    "((datestart <= $1 and datestop >= $1)  "
                    " or (datestart <= $2  and datestop >= $2)  "
                    " or (datestart >= $1 and datestop <= $2));",
    {ok, R1} = dao:simple(Query, [Fromdate, Todate]),
    {ok, collect_stats_by_acv(R1, Fromdate, Todate)}.

collect_stats_by_acv(Acvs, Fromdate, Todate) ->
    lists:map(fun(V) ->
        Idx = proplists:get_value("id", V),
        {Video_shows, Video_clicks} = dao_stat:get_acv_video_stat(Fromdate, Todate, Idx),
        [{"delta_shown", Video_shows}, {"delta_clicks", Video_clicks} | V]
    end, Acvs).   


%%% @doc
%%% Возвращает список всех acv_video для данного покупателя
%%% 
get_acv_videos(Customer_id) ->
    Query =
        "select "
            " acv_video.id, "
            " acv_video.name, "
            " acv_video.comment, "
            " acv_video.shown, "
            " acv_video.clicks, "
            " acv_video.active, "
            " acv_video.stoped, "
            " acv_video.datestart, "
            " acv_video.datestop "
        " from acv_video "
            " where customer_id = $1 "
                " and deleted = false;",
    dao:simple(Query, [(Customer_id)]).


%%% @doc
%%% Возвращает список всех acv_video для данного покупателя
%%%
get_acv_video_stats({Customer_id, {Fromdate, Todate}}) ->
    Query = "select acv_video.id, acv_video.name, acv_video.datestart, acv_video.datestop, acv_video.shown, acv_video.clicks "
                "from acv_video where customer_id = $3 and deleted = false and "
                    "((datestart <= $1 and datestop >= $1)  "
                    " or (datestart <= $2  and datestop >= $2)  "
                    " or (datestart >= $1 and datestop <= $2));",
    {ok, R1} = dao:simple(Query, [Fromdate, Todate, Customer_id]),
    {ok, collect_stats_by_acv(R1, Fromdate, Todate)}.

%%% @doc
%%% Возвращает данное acv_video
%%%

% get_acv_video(Acv_video_id) ->
%     Query =
%         "select "
%             " acv_video.id,    acv_video.name, acv_video.comment, "
%             " acv_video.url,   acv_video.ref, "
%             " acv_video.wish,  acv_video.link_title, "
%             " acv_video.shown, acv_video.duration, "
%             " acv_video.datestart, acv_video.datestop "
%         " from acv_video "
%             " where acv_video.id = $1 and deleted = false;",
%     dao:simple(Query, [(Acv_video_id)]).

% %%% @doc
% %%% Возвращает данное acv_video
% %%%
% get_acv_video_all(Acv_video_id) ->
%     case get_acv_video(Acv_video_id) of
%         {ok, R1} ->
%             case get_acv_video2cat(Acv_video_id) of
%                 {ok, R2} -> {ok, R1, [X || [{"cat_id", X}] <- R2]};
%                 E2 -> E2
%             end;
%         E1 -> E1
%     end.


%%% @doc
%%% Возвращает владельца.
%%%
get_owner(Acv_video_id) ->
    Query =
        "select "
            " acv_video.customer_id "
        " from acv_video "
            " where acv_video.id = $1",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Проверяет владельца.
%%%     Customer_id is_owner of Acv_video_id.
%%% Не применима для dao:dao_call.
%%%
is_owner(Customer_id, Acv_video_id) ->
    Query =
        "select "
            " acv_video.id "
        " from acv_video "
            " where acv_video.id = $1 "
                " and acv_video.customer_id = $2;",
    case dao:simple(Query, [Acv_video_id,Customer_id]) of
        {ok, []} -> false;
        {ok, _}  -> true
    end.

%%% @doc
%%% Возвращает общую информацию о ролике
%%% 
get_acv_video_common(Acv_video_id) ->
    Query =
        "select "
            "acv_video.id, acv_video.name, acv_video.comment, "
            "acv_video.datestart, acv_video.datestop "
        " from acv_video "
            " where acv_video.id = $1 and deleted = false;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Возвращает информацию о показах ролика
%%% 
get_acv_video_show(Acv_video_id) ->
    Query =
        "select "
            " acv_video.wish, acv_video.shown, "
            " acv_video.preroll, acv_video.postroll, "
            " acv_video.midroll, acv_video.pauseroll, "
            " acv_video.rerun_hours, acv_video.rerun_minutes, acv_video.ref "
        " from acv_video "
            " where acv_video.id = $1 and deleted = false;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Возвращает информацию о файле ролика
%%%
get_acv_video_upload(Acv_video_id) ->
    Query =
        "select "
            " acv_video.duration, acv_video.Link_title, "
            " acv_video.Alt_title, acv_video.Url, acv_video.Ref "
        " from acv_video "
            " where acv_video.id = $1 and deleted = false;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Возвращает информацию для таргетирования по категориям
%%%
get_acv_video_cats(Acv_video_id) ->
    Query =
        "select "
            " cat_id "
        " from acv_video2cat "
            " where acv_video_id = $1;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Возвращает информацию для таргетирования по регионам
%%%
get_acv_video_geos(Acv_video_id) ->
    Query =
        "select "
            " geo_region_id "
        " from acv_video2geo_region "
            " where acv_video_id = $1;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Возвращает информацию для таргетирования по пользователям
%%%
get_acv_video_users(Acv_video_id) ->
    Query =
        "select "
            " acv_video.age_from, acv_video.age_to, "
            " acv_video.time_from, acv_video.time_to, acv_video.user_male "
        " from acv_video "
            " where acv_video.id = $1;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Создает рекламу видео
%%%
update_acv_video({null, Name, Datestart, Datestop, Url, Ref, Wish,
    Postroll, Preroll, Midroll, Pauseroll, User_male,
        Age_from, Age_to, Time_from, Time_to,
            Duration, Link_title, Alt_title, Comment,
                Rerun_hours, Rerun_minutes, Customer_id}) ->
    Query =
        "insert into acv_video (name, datestart, datestop, url, ref, wish,"
            " postroll, preroll, midroll, pauseroll, "
                " user_male, age_from, age_to, time_from, time_to, "
                    "duration, link_title, alt_title, comment,"
                        "rerun_hours, rerun_minutes, customer_id) "
        "values ($1, $2, $3, $4, $5, $6, $7, $8, $9,$10, "
                    " $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, "
                    " $21, $22) "
            " returning acv_video.id;",
    case dao:simple_ret(Query, [Name, Datestart, Datestop, Url, Ref,
            (Wish), Postroll, Preroll, Midroll,
                Pauseroll, User_male,
                    Age_from, Age_to,
                    Time_from, Time_to,
                        Duration, Link_title,
                        Alt_title, Comment,
                            Rerun_hours,
                            Rerun_minutes,
                                Customer_id])  of
        {ok, 1, _, [{Id}]} ->
            dao_stat:create(Id),
            {ok, Id};
        Error -> Error
    end;

%%% @doc
%%% Изменяет рекламу видео
%%%
update_acv_video({Id, Name, Datestart, Datestop, Url, Ref, Wish,
    Postroll, Preroll, Midroll, Pauseroll, User_male,
        Age_from, Age_to, Time_from, Time_to,
            Duration, Link_title, Alt_title, Comment,
                Rerun_hours, Rerun_minutes, Customer_id}) ->
    Query =
        "update acv_video set name = $2, datestart = $3, datestop = $4, "
            " url = $5, ref = $6, wish = $7, postroll = $8, preroll = $9, "
            " midroll = $10, pauseroll = $11, user_male = $12, "
            " age_from = $13, age_to = $14, time_from = $15, time_to = $16, "
            " duration = $17, link_title = $18, alt_title = $19, comment = $20, "
            " rerun_hours =$21, rerun_minutes = $22, "
            " customer_id = $23 where id=$1;",
    dao:simple(Query, [(Id), Name,
        Datestart, Datestop, Url, Ref,
        (Wish), Postroll, Preroll, Midroll,
        Pauseroll, User_male,
        (Age_from), (Age_to),
        (Time_from), (Time_to),
        (Duration), Link_title,
        Alt_title, (Comment),
        (Rerun_hours), (Rerun_minutes),
        (Customer_id)]),
    Id;

%%% @doc
%%% ОБЕРТКА К СЛЕДУЮЩЕЙ ФУНКЦИИ.
%%% Создает рекламу видео и обвязки к ней
%%%
%
% update_acv_video({{null, Name, Datestart, Datestop, Url, Ref, Wish,
%     Postroll, Preroll, Midroll, Pauseroll, User_male,
%         Age_from, Age_to, Time_from, Time_to,
%             Duration, Link_title, Alt_title,
%                 Rerun_hours, Rerun_minutes, Customer_id},
%                     Geo_region_list, Cat_id_list}) ->
%     update_acv_video({{null, Name, Datestart, Datestop, Url, Ref, Wish,
%         Postroll, Preroll, Midroll, Pauseroll, User_male,
%         Age_from, Age_to, Time_from, Time_to,
%         Duration, Link_title, Alt_title, 0,
%         Rerun_hours, Rerun_minutes, Customer_id},
%         Geo_region_list, Cat_id_list});
%

%%% @doc
%%% Создает рекламу видео и обвязки к ней
%%%
update_acv_video({{null, Name, Datestart, Datestop, Url, Ref, Wish,
    Postroll, Preroll, Midroll, Pauseroll, User_male,
        Age_from, Age_to, Time_from, Time_to,
            Duration, Link_title, Alt_title, Comment,
                Rerun_hours, Rerun_minutes, Customer_id},
                    Geo_region_list, Cat_id_list}) ->

    Query_insert =
        "insert into acv_video (name, datestart, datestop, url, ref, wish, "
            " postroll, preroll, midroll, pauseroll, "
                " user_male, age_from, age_to, time_from, time_to, "
                    " duration, link_title, alt_title, comment, "
                        " rerun_hours, rerun_minutes, "
                        " customer_id) "
        "values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, "
                    " $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, "
                    " $21, $22) "
            " returning acv_video.id;",
    % ?D("~n~n~n@@@@@@@@@@@@@@@@@@@@#####################################~p~n", [Geo_region_list]),

    Pre_result = dao:with_transaction_fk(
        fun(Con) ->
            case dao:equery(Con, Query_insert,
                [Name, Datestart, Datestop, Url, Ref,
                    (Wish), Postroll, Preroll, Midroll,
                        Pauseroll, User_male,
                            (Age_from),
                            (Age_to),
                            (Time_from),
                            (Time_to),
                                (Duration), Link_title,
                                Alt_title, (Comment),
                                    (Rerun_hours),
                                    (Rerun_minutes),
                                        (Customer_id)])  of
                {ok, 1, _, [{Id}]} ->

    %?D("~n~n~n#####################################~p~n", [Geo_region_list]),

                    case length(Geo_region_list) of
                        0 ->    ok;
                        _ ->
                            Query_acv_video2geo_region =
                                "insert into acv_video2geo_region "
                                 "(acv_video_id, geo_region_id) values " ++
                                  make_brackets_string(Id, Geo_region_list),
                            {ok, _} = dao:equery(Con,
                                Query_acv_video2geo_region, [])
                    end,
                    case length(Cat_id_list) of
                        0 ->    ok;
                        _ ->
                            Query_acv_video2cat =
                                "insert into acv_video2cat "
                                    "(acv_video_id, cat_id) values " ++
                                        make_brackets_string(Id, Cat_id_list),
                            {ok, _} = dao:equery(Con,
                                Query_acv_video2cat, [])
                    end,
                    dao_stat:create(Con, Id),
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
            Duration, Link_title, Alt_title, Comment,
                Rerun_hours, Rerun_minutes, Customer_id},
                    Geo_region_list, Cat_id_list}) ->

    Query_update =
        "update acv_video set name = $2, datestart = $3, datestop = $4, "
            " url = $5, ref = $6, wish = $7, postroll = $8, preroll = $9, "
            " midroll = $10, pauseroll = $11, user_male = $12, "
            " age_from = $13, age_to = $14, time_from = $15, time_to = $16, "
            " duration = $17, link_title = $18, alt_title = $19, comment = $20,"
            " rerun_hours = $21, rerun_minutes = $22, "
            " customer_id = $23 where id=$1;",
    dao:with_transaction_fk(
        fun(Con) ->
            case dao:equery(Con, Query_update,
                [Id, Name, Datestart, Datestop, Url, Ref,
                (Wish), Postroll, Preroll, Midroll,
                    Pauseroll, User_male,
                    (Age_from),
                    (Age_to),
                        (Time_from),
                        (Time_to),
                            (Duration), Link_title,
                            Alt_title, (Comment),
                                (Rerun_hours),
                                (Rerun_minutes),
                                    (Customer_id)]) of
                {ok, _ } ->
                    case length(Geo_region_list) of
                        0 ->    ok;
                        _ ->
                            Query_video2region_d =
                                "delete from acv_video2geo_region "
                                    " where acv_video_id = $1", 
                            Query_acv_video2geo_region =
                                "insert into acv_video2geo_region "
                                 " (acv_video_id, geo_region_id) values " ++
                                  make_brackets_string(Id, Geo_region_list),
                            dao:equery(Con, Query_video2region_d, [Id]),
                            {ok, _} = dao:equery(Con,
                                Query_acv_video2geo_region, [])
                    end,
                    case length(Cat_id_list) of
                        0 ->    ok;
                        _ ->
                            Query_video2cat_d =
                                "delete from acv_video2cat "
                                    " where acv_video_id = $1", 
                            Query_acv_video2cat =
                                "insert into acv_video2cat "
                                    " (acv_video_id, cat_id) values " ++
                                        make_brackets_string(Id, Cat_id_list),
                            dao:equery(Con, Query_video2cat_d, [Id]),
                            {ok, _} = dao:equery(Con,
                                Query_acv_video2cat, [])
                    end,
                    {ok, Id};
                Error -> Error
            end
        end
    ).

%%% @doc
%%% "Удаляет" сущность рекламы
%%%
delete_acv_video(Acv_video_id) ->
    Query = "update acv_video set deleted = true where id=$1;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Остагнавливает кампанию. Ее остаитьможет в том числе и сам заказчик
%%%
stop_acv_video(Acv_video_id) ->
    Query = "update acv_video set stoped = true where id=$1;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Остагнавливает кампанию срок действия которой истек
%%%
stop_old_acv_video() ->
    Query = "update acv_video set acv_video.stoped = true "
        " where acv_video.datestop < NOW();",
    dao:simple(Query).

%%% @doc
%%% Запускает остановленную кампанию.
%%%
start_acv_video(Acv_video_id) ->
    Query = "update acv_video set stoped = false where id=$1;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Активирует рекламную компанию.
%%%
activate_acv_video(Acv_video_id) ->
    Query = "update acv_video set active = true where id=$1;",
    dao:simple(Query, [(Acv_video_id)]).

%%% @doc
%%% Деактивирует рекламную компанию.
%%%
disactivate_acv_video(Acv_video_id) ->
    Query = "update acv_video set active = false where id=$1;",
    dao:simple(Query, [(Acv_video_id)]).

mkbill(Acv_video_id) ->
    bill({Acv_video_id, false}).

paybill(Acv_video_id) ->
    bill({Acv_video_id, true}).

bill({Acv_video_id, Pay_status}) ->
    Query =
        " update acv_video set "
        "   pay_status = $2 "
        " where id = $1;",
    dao:simple(Query, [Acv_video_id, Pay_status]).

%%%
%%% @doc
%%% Изменяет состояние кампании:
%%% Актина = true
%%%     Неактивна = false
%%%     Непросмотрена = null
%%% Счет оплачен = true
%%%     Счет не оплачен  = false
%%%     Счет не выставлен  = null
%%% Sum
%%%


chstate_acv_video({Acv_video_id, Active, Sum}) ->
    Query =
        " update acv_video set "
        "   active = $2, "
        "   sum = $3 "
        " where id=$1;",
    dao:simple(Query, [Acv_video_id, Active, Sum]).



%%% @doc
%%% Удаляет устаревшие записи в acv_video_shown
%%%
delete_acv_video_shown_expired() ->
    Query = "delete from acv_video_shown where dateshow < NOW()",
    dao:simple(Query).

%%% @doc
%%% Удаляет устаревшие записи в acv_video_shown
%%%
delete_acv_video_shown_expired(Date) ->
    Query = "delete from acv_video_shown where dateshow < $1",
    dao:simple(Query, [Date]).

%%% @doc
%%% Полностью удаляет обвязки рекламы и ее сущность
%%%
full_delete_acv_video(Id) ->
    Query_video =
        "delete from acv_video where id = $1;",
    Query_video2geo_region =
        "delete from acv_video2geo_region where acv_video_id = $1;",
    Query_video2cat =
        "delete from acv_video2cat where acv_video_id = $1;",
    Query_stat =
        "drop table acv_video_stat_" ++ erlang:integer_to_list(Id) ++ ";",
    dao:with_transaction_fk(
        fun(Con) ->
            dao:equery(Con, Query_stat, []),
            dao:equery(Con, Query_video2geo_region, [(Id)]),
            dao:equery(Con, Query_video2cat, [(Id)]),
            dao:equery(Con, Query_video, [(Id)])
        end
    ).

%%% @doc
%%% Возвращает строку вида
%%%     (Id, Id_list[1]), ..., (Id, Id_list[n])
%%% 
make_brackets_string(Id, Id_list)->
    string:join([string:join(["(", convert:to_list(Id), ",",
        convert:to_list(X),")"], []) || X <- Id_list], ",").

%%% @doc
%%% Возвращает строку вида
%%%     (Id, Id_list[1]), ..., (Id, Id_list[n])
%%% НАИВНАЯ РЕАЛИЗАЦИЯ
%%% 
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
    %test_eunit_1(),
    %test_eunit_2(),

    ok.

test_acv_video()->
    dao_acv_video:update_acv_video({{null,
        "console -> test", {{2012,01,27},{0,0,0.1}}, {{2012,01,28},{0,0,0.1}},
            "http://ya.ru",
                "static/data/acv-video/common/130387262/adv10.mp4 ", 100,
                    true, true, true, true, true,
                        null, null, null, null,
                            20, "354b9bd8-c2aa-4a92-81ee-0fccc85a9273",
                                "354b9bd8-c2aa-4a92-81ee-0fccc85a9273", 0, 1},
        [13, 14], [674]}),
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
    Wish =          1,
    Postroll =      true,
    Preroll =       true,
    Midroll =       null,
    Pauseroll =     null,
    User_male =     null,
    Age_from  =     1,
    Age_to =        2,
    Time_from  =    10,
    Time_to =       11,
    Duration  =     1,
    Link_title =    "Link_title",
    Alt_title  =    "Alt_title",
    Comment =       "internal comment",
    Rerun_hours =   1,
    Rerun_minutes = 1,

    Customer_id =   1,

    ?MODULE:get_all_acv_videos([]),
    ?MODULE:get_acv_videos(Customer_id),
    {ok, Acv_video_id} =
        dao_acv_video:update_acv_video({null,
            Name, Datestart, Datestop, Url, Ref, Wish,
                Postroll, Preroll, Midroll, Pauseroll, User_male,
                    Age_from, Age_to, Time_from, Time_to,
                        Duration, Link_title, Alt_title, Comment,
                            Rerun_hours, Rerun_minutes,
                                Customer_id}),

    dao_acv_video:stop_acv_video(Acv_video_id),

    dao_acv_video:update_acv_video({Acv_video_id,
        Name_new, Datestart, Datestop, Url, Ref, Wish,
            Postroll, Preroll, Midroll, Pauseroll, User_male,
                Age_from, Age_to, Time_from, Time_to,
                    Duration, Link_title, Alt_title, Comment,
                        Rerun_hours, Rerun_minutes,
                            Customer_id}),
    ?assertEqual({ok,[[
            {"datestop",    Datestop},
            {"datestart",   Datestart},
            {"duration",    Duration},
            {"shown",       0},
            {"link_title",  Link_title},
            {"wish",        Wish},
            {"ref",         Ref},
            {"url",         Url},
            {"comment",     Comment},
            {"name",        Name_new},
            {"id",          Acv_video_id}]]},
        ?MODULE:get_acv_video(Acv_video_id)),
    ?MODULE:full_delete_acv_video(Acv_video_id),

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
    Wish =          1,
    Postroll =      true,
    Preroll =       true,
    Midroll =       true,
    Pauseroll =     true,
    User_male =     true,
    Age_from  =     1,
    Age_to =        2,
    Time_from  =    10,
    Time_to =       11,
    Duration  =     1,
    Link_title =    "Link_title",
    Alt_title  =    "Alt_title",
    Comment =       "internal comment",
    Rerun_hours =   1,
    Rerun_minutes = 1,
    Customer_id =   1,

    {ok, Acv_video_id} = ?MODULE:update_acv_video({{null,
        Name, Datestart, Datestop, Url, Ref, Wish,
            Postroll, Preroll, Midroll, Pauseroll, User_male,
                Age_from, Age_to, Time_from, Time_to,
                    Duration, Link_title, Alt_title, Comment,
                        Rerun_hours, Rerun_minutes,
                            Customer_id}, R_list, []}),

    dao_acv_video:stop_acv_video(Acv_video_id),

    ?MODULE:update_acv_video({{Acv_video_id,
        Name_new, Datestart, Datestop, Url, Ref, Wish,
            Postroll, Preroll, Midroll, Pauseroll, User_male,
                Age_from, Age_to, Time_from, Time_to,
                    Duration, Link_title, Alt_title, Comment,
                        Rerun_hours, Rerun_minutes,
                            Customer_id}, R_list_new, []}),

    dao_acv_video:full_delete_acv_video(Acv_video_id),
    lists:foreach(fun(R)->
        dao_geo_region:delete_geo_region(convert:to_integer(R)),
    ok end,R_list),

    lists:foreach(fun(R)->
        dao_geo_region:delete_geo_region(convert:to_integer(R)),
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
