%%% @file dao_stat.erl
%%%
%%%    Статистика.
%%%

-module(dao_stat).

-compile(export_all).

-export([
    create/1,
    create/2,
    delete/1,
    fetch_stat_by_time/0,
    fetch_stat_by_time/1,
    fetch_stat_by_id/0,
    fetch_stat_by_id/1,
    get_acv_video_stat_by_films/1,
    get_acv_video_stat_by_films/3,
    test/0,
    test/1
]).


-include("common.hrl").

% create sequence seq_acv_video_stat;
% create table acv_video_stat (
%    id int primary key default nextval('seq_acv_video_stat'),
%    ip varchar(15),
%    acv_video_id int references acv_video(id),
%    video_uid varchar(50),
%    datestart timestamp without time zone,
%    datestop timestamp without time zone,
%    clicked bool
%);
% CREATE INDEX datestart_date_idx on acv_video_stat(datestart);
% CREATE INDEX datestop_date_idx on acv_video_stat(datestop);
% CREATE INDEX video_uid_idx on acv_video_stat(video_uid);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-define(STAT_SKIP_SECOND, 3600).


-record(stat, {
    video_url,
    video_name, 
    peer, 
    server_node, 
    datestart,
    datestop,
    click,
    user_id,
    dbg_list
}).

%%%
%%% Создает таблицу статистики
%%% 
create(Acv_video_id) ->
    dao:with_transaction_fk(fun(Con) ->
        create(Con, Acv_video_id)
    end).

%%%
%%% Создает таблицу статистики
%%% 
create(Con, Acv_video_id) ->
    Q1 = ?FMT("create sequence seq_acv_video_stat_~p;", [Acv_video_id]),
    Q2 = ?FMT("create table acv_video_stat_~p ("
                "id int primary key default nextval('seq_acv_video_stat_~p'),"
                "parent_id int default ~p,"
                "peer varchar(15),"
                "video_url text,"
                "video_name text,"
                "node_name varchar(100),"
                "datestart timestamp without time zone,"
                "datestop timestamp without time zone,"
                "user_id varchar(100),"
                "tmp_dbg text,"
                "click timestamp without time zone);",
        [Acv_video_id, Acv_video_id, Acv_video_id]),
    Q3 = ?FMT("create index acv_video_stat_~p_datestart_date_idx "
        "on acv_video_stat_~p(datestart);", [Acv_video_id, Acv_video_id]),
    Q4 = ?FMT("create index acv_video_stat_~p_datestop_date_idx "
        "on acv_video_stat_~p(datestop);", [Acv_video_id, Acv_video_id]),
    Q5 = ?FMT("create index acv_video_stat_~p_video_uid_idx "
        "on acv_video_stat_~p(video_url);", [Acv_video_id, Acv_video_id]),
    pgsql:equery(Con, Q1),
    pgsql:equery(Con, Q2),
    pgsql:equery(Con, Q3),
    pgsql:equery(Con, Q4),
    pgsql:equery(Con, Q5).

%delete_vstat_tbl(Acv_video_id) ->
delete(Acv_video_id) ->
    Q = ?FMT("drop sequence seq_acv_video_stat_~p;", [Acv_video_id]),
    dao:simple(Q).

fetch_stat_by_time() ->
    fetch_stat_by_time(null).

fetch_stat_by_time(null) ->
    {_day, _} = erlang:localtime(),
    fetch_stat_by_time({_day, {0,0,0}});

fetch_stat_by_time(all) ->
    fetch_stat_by_time({{1970,01, 1}, {0,0,0}});

fetch_stat_by_time(From_id) ->
    case mysql:get_prepared(fetch_stat_by_time) of
        {error, _ } ->
            Q = <<"select * from AVStats where time > ? order by time desc;">>,
            mysql:prepare(fetch_stat_by_time, Q);
        _ -> ok
    end,
    {data,{mysql_result, Cols, Vals, _X31, _X32}}
        = mysql:execute(mysqlStat, fetch_stat_by_time, [From_id]),
    L = mysql_dao:make_proplist(Cols, Vals),
    ?D("===================~n", []),
    ?D("Stats stream: ~p~n", [L]),
    compose_stat(L),
    EL = ets:tab2list(stat_clt),
    ?D("===================~n", []),
    ?D("Composed stats: ~p~n", [EL]),
    To_db = collect(EL, []),
    ?D("===================~n", []),
    ?D("Stats to DB:~p~n", [To_db]),
    to_db(To_db).

fetch_stat_by_id() ->
    dao:with_transaction_fk(fun(Con) ->
        Cid = compute_stat_max_id(),
        Gid = get_max_id(Con),
        case Cid > Gid of
            true ->
                put_max_id(Con, Cid),
                fetch_stat_by_id(Cid);
            _ ->
                % а зачем?
                fetch_stat_by_id(Gid)
        end
    end).

fetch_stat_by_id(null) ->
    fetch_stat_by_id(0);

fetch_stat_by_id(From_id) ->
    case mysql:get_prepared(fetch_stat_by_id) of
        {error, _ } ->
            Q = <<"select * from AVStats where id > ? order by time desc;">>,
            mysql:prepare(fetch_stat_by_id, Q);
        _ -> ok
    end,
    {data,{mysql_result, Cols, Vals, _X31, _X32}}
        = mysql:execute(mysqlStat, fetch_stat_by_id, [From_id]),
    L = mysql_dao:make_proplist(Cols, Vals),
    ?D("===================~n", []),
    ?D("Stats stream: ~p~n", [L]),
    compose_stat(L),
    EL = ets:tab2list(stat_clt),
    ?D("===================~n", []),
    ?D("Composed stats: ~p~n", [EL]),
    To_db = collect(EL, []),
    ?D("===================~n", []),
    ?D("Stats to DB:~p~n", [To_db]),
    to_db(To_db).

compute_stat_max_id() ->
    case mysql:get_prepared(get_stat_max_id) of
        {error, _ } ->
            Q = <<"select max(id) as max_id from AVStats;">>,
            mysql:prepare(get_stat_max_id, Q);
        _ -> ok
    end,
    {data,{mysql_result, Cols, Vals, _X31, _X32}}
        = mysql:execute(mysqlStat, get_stat_max_id, []),
    [[{"max_id", Max_id}]] = mysql_dao:make_proplist(Cols, Vals),
    Max_id.

put_max_id(Con, Max_id) ->
    Query = "update var set av_stats_max_id = $1;",
    pgsql:equery(Con, Query, [Max_id]).

get_max_id(Con) ->
    Query = "select av_stats_max_id from var;",
    S = dao:pgret(pgsql:equery(Con, Query)),
    {ok,[[{"av_stats_max_id",Max_id}]]}  = S,
    Max_id.

compose_stat([R|T]) ->
    Acv_video_url = proplists:get_value("GUID", R), % Acv_video_url
    Usid = proplists:get_value ("user session id", R),
    Peer = proplists:get_value("IP address", R),
    Trans_server_node_name = proplists:get_value("Node name", R),
    Time = proplists:get_value("time", R),
    Action = proplists:get_value("action", R),
    Video_url = proplists:get_value("URL", R),
    User_id = proplists:get_value("User_id", R),

    SFish = #stat{video_url=Video_url, peer=Peer, server_node=Trans_server_node_name, datestart=null, datestop=null, click=null, user_id=User_id, dbg_list=[R]},
    Stat_list = case ets:lookup(stat_clt, {Acv_video_url, Usid}) of
        [] -> 
            case Action of
                <<"AC_SHOW">> ->    [SFish#stat{datestart=Time}];
                % пришел фулшоу, старта мы не знаем. варианты:
                % 1 была инфа о начале, но мы ее скинули из за задержки конца (если не запишем событие, то потеряем показ)
                % 2 была инфа о начале и клике, мы их внесли в базу из за задержки конца (если запишем это событие, то будет дубль записи)
                % 3 была инфа о клике, мы ее записали не дождавшись конца. (если запишем это событие, то будет дубль)
                % вывод - скипаем это событие, что бы не было дублей. возможна частичная потеря данных
                <<"AC_FULLSHOW">> -> [];
                % пришел клик, нет старта. вариант - инфа о начале потеряна из за дельты. ставим начало == клик
                <<"AC_CLICK">> ->   [SFish#stat{datestart=Time, click=Time}]
                                    %[{Video_url, Peer, Trans_server_node_name, Time, null, Time}];
            end;
        [{_, Av_list}] ->
            S=#stat{dbg_list=Dbgl} = lists:last(Av_list),
            case Action of
                % всегда создаем новый показ. если предыдущий был не закрыт - потом при записи отфильтруем его
                <<"AC_SHOW">> ->
                    lists:append(Av_list, [SFish#stat{datestart=Time}]);
                % апдейтим предыдущую запись - если конец был заполнен, то это ошибка с неизвестным источником
                <<"AC_FULLSHOW">> ->    
                    lists:append(lists:delete(S, Av_list), [S#stat{datestop=Time, dbg_list=[R|Dbgl]}]);
                % апдейтим предыдущую запись, если время клика будет больше времени конца просмотра, то это ошибка  с неизвестным источником
                <<"AC_CLICK">> ->
                    lists:append(lists:delete(S, Av_list), [S#stat{click=Time, dbg_list=[R|Dbgl]}])
            end
    end,

    if
        length(Stat_list) > 0 ->
            ?D("CS step: ~p~n", [{{Acv_video_url, Usid}, Stat_list}]),
            ets:insert(stat_clt, {{Acv_video_url, Usid}, Stat_list});
        true -> 
            ?D("CS step: none~n", []),
            done
    end,
    compose_stat(T);

compose_stat([]) ->
    done.

to_db([]) -> [];

to_db(To_db) ->
    Acv_video_urls = proplists:get_keys(To_db),
    ?D("KEYS: ~p~n", [Acv_video_urls]),
    Q = "select id, ref, shown from acv_video where ref in (" ++
        string:join(["'" ++ binary_to_list(X) ++ "'"
            || X <- lists:flatten(Acv_video_urls)], ",") ++ ");",
    ?D("QS1: ~p~n", [Q]),
    {ok, Advs} = dao:simple(Q),
    ?D("ADVS: ~p~n", [Advs]),
    to_db_acv_video_url(Advs, To_db).


to_db_acv_video_url([Adv|T], To_db) ->
    Key     = proplists:get_value("url", Adv),
    Id      = proplists:get_value("id", Adv),
    Shown   = proplists:get_value("shown", Adv),
    Clicks  = proplists:get_value("clicks", Adv),

    Values = lists:flatten(proplists:get_all_values(list_to_binary(Key), To_db)),

    try
        create(Id)
    catch
        A:B -> ?D("info: ~p:~p", [A, B])
    end,

    Q1 = ?FMT("insert into acv_video_stat_~p ("
            "peer,"         % varchar(15)
            "video_url, "   % varchar(50)
            "video_name,"   % text
            "node_name, "   % varchar(100)
            "datestart, "       % timestamp without time zone
            "datestop, "        % timestamp without time zone
            "user_id, "     % varchar(100)
            "tmp_dbg, "     % text
            "click"         % timestamp without time zone
            ") values ", [Id]),

    ?D("VALUES: ~p~n", [Values]),

    ADV_Values = format_adv(Values, []),
%    ?D("ADV  VALUES: ~p~n", [ADV_Values]),
    Q = Q1 ++ string:join(ADV_Values, ", ") ++ ";",
%    ?D("QQ:~p~n~n", [Q]),

    QUp = "update acv_video set shown=$1, clicks=$2 where id=$3;",

    ClickCounter = length([X || X <- Values, X#stat.click=/=null]),

    _RQ = dao:with_connection_fk(fun(Con) ->
        R1 = pgsql:equery(Con, QUp, [Shown + length(Values), Clicks + ClickCounter, Id]),
        R2 = pgsql:equery(Con, Q),
        {R1, R2}
    end),
%    ?D("RQ: ~p~n", [RQ]),
    to_db_acv_video_url(T, To_db);
to_db_acv_video_url([], _) ->
    done.

%%%
%%% TODO: переписать более эффективно.
%%%
format_adv([#stat{
            video_url=Video_url,
            video_name=Video_name,
            peer=Peer,
            server_node=Trans_server_node_name,
            datestart = {{Start_year, Start_month, Start_day},
                {Start_hour, Start_min, Start_sec}},
            datestop = {{Stop_year, Stop_month, Stop_day},
                {Stop_hour, Stop_min, Stop_sec}},
            click = Click,
                %%% {{Click_year, Click_month, Click_day},
                %%%    {Click_hour, Click_min, Click_sec}},
            user_id = User_id,
            dbg_list=Dbgl
        }|T], Ret) ->
    SL1 = [
        "'" ++ utils:to_list(Peer) ++ "'", 
        "'" ++ utils:to_list(Video_url) ++ "'",
        "'" ++ utils:to_list(Video_name) ++ "'",
        "'" ++ utils:to_list(Trans_server_node_name) ++ "'",
        ?FMT("'~p-~p-~p ~p:~p:~p'", [Start_year, Start_month, Start_day,
            Start_hour, Start_min, Start_sec]),
        ?FMT("'~p-~p-~p ~p:~p:~p'", [Stop_year, Stop_month, Stop_day,
            Stop_hour, Stop_min, Stop_sec]),
        "'" ++ utils:to_list(User_id) ++ "'",
        ?FMT("'~p'", [Dbgl])],
    case Click of
        {{Click_year, Click_month, Click_day},
            {Click_hour, Click_min, Click_sec}} ->
            SL2 = [?FMT("'~p-~p-~p ~p:~p:~p'",
                [Click_year, Click_month, Click_day,
                    Click_hour, Click_min, Click_sec])];
        _ ->
            SL2 = ["null"]
    end,
    AL = "(" ++ string:join(lists:append(SL1, SL2), ", ") ++ ")",
    format_adv(T, [AL|Ret]);

%%%
%%% TODO: переписать более эффективно.
%%%         lists:reverse --- очень дорогой.
%%%         ?FMT --- тоже не дешевый, надо пользоваться конкатенацией.
format_adv([], Ret) ->
    lists:reverse(Ret).


collect([{{Acv_video_url, Usid}, Stat_list}|T], DBRecords) ->
    {Last, To_db} = collect_acv_stats(Stat_list, [], []),
    if
        length(Last) > 0    ->
            ets:insert(stat_clt, {{Acv_video_url, Usid}, Last});
        true                ->
            ets:delete(stat_clt, {Acv_video_url, Usid})
    end,
    collect(T, [{Acv_video_url, To_db}|DBRecords]);
    % NewList = proplists:get_value("Acv_video_url", DBRecords), To_db)

collect([], DBRecords) ->
    DBRecords.

%%%
%%% Вычисляет количество секунд прошедших с момента Start
%%%
compute_seconds_from(Start) ->
    calendar:datetime_to_gregorian_seconds(erlang:localtime()) -
        calendar:datetime_to_gregorian_seconds(Start).
%%%
%%% только первый кусок сообщения, кешируем в ets,
%%%     если не прошло времени с прихода больше дельты
%%% 
collect_acv_stats(
            [S=#stat{datestart=Start, datestop=null, click=null}|[]],
            Last,
            To_db) ->
    case compute_seconds_from(Start) > ?STAT_SKIP_SECOND of
        true ->  % skip
            collect_acv_stats([], Last, To_db);
        false -> % wait
            collect_acv_stats([], [S|Last], To_db)
            % вообще-то last всегда == []
    end;

%%%
%%% скип. ошибка потока сознания от сервера
%%%
collect_acv_stats([#stat{datestop=null, click=null}|T], Last, To_db) ->
    collect_acv_stats(T, Last, To_db);

% клик не пустой. ставим стоп=клик, в базу.
collect_acv_stats([S=#stat{datestop=null, click=Click}|T], Last, To_db) ->
    collect_acv_stats(T, Last, [S#stat{datestop=Click}|To_db]);

% корректная запись, без клика.
collect_acv_stats([S=#stat{datestart=Start, datestop=Stop, click=null}|T],
        Last, To_db) when Start =< Stop ->
    collect_acv_stats(T, Last, [S|To_db]);

% корректная запись с кликом.
collect_acv_stats([S=#stat{datestart=Start, datestop=Stop, click=Click}|T],
        Last, To_db) when Start =< Click, Click =< Stop ->
    collect_acv_stats(T, Last, [S|To_db]);

% все остальные случаи - скип
collect_acv_stats([S|T], Last, To_db) ->
    ?D("skip stat: ~p~n", S),
     collect_acv_stats(T, Last, To_db);

collect_acv_stats([], Last, To_db) ->
    {lists:reverse(Last), lists:reverse(To_db)}.

mk_ets() ->
    utils:make_ets(stat_clt, [{write_concurrency,true}]).

%%%
%%% Возвращает {ok, proplist()}
%%%
get_acv_video_stat_by_films({From_datetime, To_datetime, Acv_Id}) ->
    {ok, get_acv_video_stat_by_films(From_datetime, To_datetime, Acv_Id)}.

%%%
%%% Возвращает proplist()
%%%
get_acv_video_stat_by_films(From_datetime, To_datetime, Acv_Id) ->
    Q = "select * from acv_video_stat_" ++ utils:to_list(Acv_Id) ++
        " where (datestart < $1 and datestop > $1) "
            " or (datestop < $2  and datestop > $2) "
            " or (datestart > $1 and datestop < $2);",
    {ok, Vals} = dao:simple(Q, [From_datetime, To_datetime]),
    All_videos_proplist =
        [{proplists:get_value("video_url", X), X} || X <- Vals],
    Video_urls = proplists:get_keys(All_videos_proplist),
    group_by_film(Video_urls, All_videos_proplist, []).

group_by_film([], _PL, Ret) ->
    Ret;

group_by_film([Video_url|Rest_video_urls], All_videos_proplist, Ret) ->
    Video_name = proplists:get_value("video_name",
        proplists:get_value(Video_url, All_videos_proplist)),
    Shown_list = proplists:get_all_values(Video_url, All_videos_proplist),
    Video_shows = length(Shown_list),
    Video_clicks = length([X || X <- Shown_list,
        proplists:get_value("click", X)=/=null]),
    group_by_film(Rest_video_urls, All_videos_proplist,
        [[
            {"video_name",  Video_name},
            {"video_url",   Video_url},
            {"shows",       Video_shows},
            {"clicks",      Video_clicks}]
        | Ret]
    ).

%{Acv_video_url, UserSessionId}, {Peer, NodeName, Start, Stop, Clicked}
%collect([R|T]) ->

clear_expired() ->
    Match = {{'_','_'},{'_', '_', '$1', '_', '_'}},
    PastDt = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) - 5*60),
    Guard = [{'<','$1',{const, PastDt}}],
    ets:select_delete(stat_clt, [{Match, Guard, ['$_']}]).

t1() ->
    Match = {{'_','_'}, {'_', '_', '$1', '_', '_'}},
    %DT = {{2011,2,2},{10,5,0}},
    PastDt = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) - 5*60),
    Guard = [{'<','$1',{const, PastDt}}],
%    ets:insert(stat_clt, {{1, "b"}, {{{2012,1,2},{10,5,0}}, kit, pechen, treska}}),
%    ?D("~p~n", [ets:lookup(stat_clt, {1, "a"})]),
    ets:select(stat_clt, [{Match, Guard, ['$_']}]).

%compose_stat(StartDatetime, StopDatetime) ->
%    "select * from AVStats where `time` > $1 and `time` < $2",
%    AdvList

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Юнит тестирование
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

test()->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Нагрузочное тестирование
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mk_year(_year, VideoList) ->
    lists:seq(0, 364),
    lists:foreach(fun({Y, VL}) ->
        spawn(fun() ->
            mk_day(Y, VL)
        end)
    end, [{calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(_year, 1, 1) + X), VideoList} || X <- lists:seq(0, 364)]).

mk_day(Date, VideoList) ->
    ?D("date: ~p~n", [Date]),
    %apply(mk_rec, [[calendar:gregorian_seconds_to_datetime(calendar:date_to_gregorian_days(Date) * 24*60*60 + X), VideoList] || X <- lists:seq(0, 23*60*60, 5)]).
    mk_rec([calendar:gregorian_seconds_to_datetime(calendar:date_to_gregorian_days(Date) * 24*60*60 + X) || X <- lists:seq(0, 23*60*60, 5)],
        VideoList, [], []).

mk_rec([], _VideoList, PL, QL) ->
    do_insert(PL, QL),
    done;

mk_rec([Start | T], VideoList, PL, QL) when length(QL) < 50 ->
    RandVideo = lists:nth(random:uniform(length(VideoList)), VideoList),
    Ip = ?FMT("~p.~p.~p.~p", [random:uniform(254), random:uniform(254), random:uniform(254), random:uniform(254)]),
    Stop = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Start) + 30 + random:uniform(30)),
    Clicked = random:uniform(2) =:= 1,
    mk_rec(T, VideoList, 
        lists:append(PL, [Ip, 1, RandVideo, Start, Stop, Clicked]), 
        lists:append(QL, ["(" ++ string:join(["$" ++ integer_to_list(X) || X <- lists:seq(length(PL) + 1, length(PL) + 6)], ", ") ++ ")"])
    );

mk_rec(StartList, VideoList, PL, QL) ->
    do_insert(PL, QL),
    mk_rec(StartList, VideoList, [], []).

do_insert(PL, QL) ->
    Q = "insert into acv_video_stat (ip, acv_video_id, video_uid, "
            " datestart, datestop, clicked) values " ++
        string:join(QL, ", ") ++ ";",
    %?D("QQQ: ~p~n~n", [Q]).
    dao:simple(Q, PL).
 
gen_vstat() ->
    mk_year(2011, [integer_to_list(X) || X <- lists:seq(0, 3000)]).


test1() ->
    D1 = {{2011, 1,1},{0,0,0}}, 
    D2 = {{2011, 5,31},{0,0,0}},
    Q = "select * from acv_video_stat where video_uid = '1748' "
        "and datestart > $1 and datestop < $2;",
    dao:simple(Q, [D1, D2]),
    done.

test(speed)->
    Times_1 = 1000,
    %%%
    %%% Лучше использовать mysql:get_prepared, с ним в 2 раза быстрее
    %%%     1349527 против 2840849
    %%%
    tests:print_speed("+",
        fun() -> % лучше
            case mysql:get_prepared('get_stat_max_id_get_prepared') of
                {error, _ } ->
                    Q = <<"select max(id) as max_id from AVStats;">>,
                    mysql:prepare('get_stat_max_id_get_prepared', Q);
                _ -> ok
            end,
            {data,{mysql_result, Cols, Vals, _X31, _X32}}
                = mysql:execute(mysqlStat, 'get_stat_max_id_get_prepared', []),
            [[{"max_id", Max_id}]] = mysql_dao:make_proplist(Cols, Vals)
        end, Times_1 ),

    tests:print_speed("-",
        fun() ->
            Q = <<"select max(id) as max_id from AVStats;">>,
            mysql:prepare('get_stat_max_id_prepare', Q),
            {data,{mysql_result, Cols, Vals, _X31, _X32}}
                = mysql:execute(mysqlStat, 'get_stat_max_id_prepare', []),
            [[{"max_id", Max_id}]] = mysql_dao:make_proplist(Cols, Vals)
        end, Times_1 ),

    ok.

