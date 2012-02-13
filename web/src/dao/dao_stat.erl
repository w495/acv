%%% @file dao_stat.erl
%%%
%%%    Статистика.
%%%

-module(dao_stat).

-compile(export_all).

-export([
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
%    start timestamp without time zone,
%    stop timestamp without time zone,
%    clicked bool
%);
% CREATE INDEX start_date_idx on acv_video_stat(start);
% CREATE INDEX stop_date_idx on acv_video_stat(stop);
% CREATE INDEX video_uid_idx on acv_video_stat(video_uid);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

-define(STAT_SKIP_SECOND, 3600).


-record(stat, {
    video_url, 
    peer, 
    server_node, 
    start, 
    stop,
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
    Q2 = ?FMT(  "create table acv_video_stat_~p ("
                    "id int primary key default nextval('seq_acv_video_stat_~p'),"
                    "peer varchar(15),"
                    "video_url text,"
                    "node_name varchar(100),"
                    "start timestamp without time zone,"
                    "stop timestamp without time zone,"
                    "user_id varchar(100),"
                    "tmp_dbg text,"
                    "click timestamp without time zone);", [Acv_video_id, Acv_video_id]),
    Q3 = ?FMT("create index acv_video_stat_~p_start_date_idx on acv_video_stat_~p(start);", [Acv_video_id, Acv_video_id]),
    Q4 = ?FMT("create index acv_video_stat_~p_stop_date_idx on acv_video_stat_~p(stop);", [Acv_video_id, Acv_video_id]),
    Q5 = ?FMT("create index acv_video_stat_~p_video_uid_idx on acv_video_stat_~p(video_url);", [Acv_video_id, Acv_video_id]),
    pgsql:equery(Con, Q1),
    pgsql:equery(Con, Q2),
    pgsql:equery(Con, Q3),
    pgsql:equery(Con, Q4),
    pgsql:equery(Con, Q5).

%delete_vstat_tbl(Acv_video_id) ->
delete(Acv_video_id) ->
    Q = ?FMT("drop sequence seq_acv_video_stat_~p;", [Acv_video_id]),
    dao:simple(Q).

fetch_stat() ->
    fetch_stat(null).

fetch_stat(null) ->
    {Day, _} = erlang:localtime(),
    fetch_stat({Day, {0,0,0}});

fetch_stat(From_date) ->
    Q = <<"select * from AVStats where time > ? order by time desc;">>,
    mysql:prepare(get_stats, Q),
    {data,{mysql_result, Cols, Vals, _X31, _X32}} = mysql:execute(mysqlStat, get_stats, [From_date]),

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


compose_stat([R|T]) ->
    Acv_video_url = proplists:get_value("GUID", R), % Acv_video_url
    Usid = proplists:get_value ("user session id", R),
    Peer = proplists:get_value("IP address", R),
    Trans_server_node_name = proplists:get_value("Node name", R),
    Time = proplists:get_value("time", R),
    Action = proplists:get_value("action", R),
    Video_url = proplists:get_value("URL", R),
    User_id = proplists:get_value("UserId", R),

    SFish = #stat{video_url=Video_url, peer=Peer, server_node=Trans_server_node_name, start=null, stop=null, click=null, user_id=User_id, dbg_list=[R]},
    StatList = case ets:lookup(stat_clt, {Acv_video_url, Usid}) of
        [] -> 
            case Action of
                <<"AC_SHOW">> ->    [SFish#stat{start=Time}];
                % пришел фулшоу, старта мы не знаем. варианты:
                % 1 была инфа о начале, но мы ее скинули из за задержки конца (если не запишем событие, то потеряем показ)
                % 2 была инфа о начале и клике, мы их внесли в базу из за задержки конца (если запишем это событие, то будет дубль записи)
                % 3 была инфа о клике, мы ее записали не дождавшись конца. (если запишем это событие, то будет дубль)
                % вывод - скипаем это событие, что бы не было дублей. возможна частичная потеря данных
                <<"AC_FULLSHOW">> -> [];
                % пришел клик, нет старта. вариант - инфа о начале потеряна из за дельты. ставим начало == клик
                <<"AC_CLICK">> ->   [SFish#stat{start=Time, click=Time}]
                                    %[{Video_url, Peer, Trans_server_node_name, Time, null, Time}];
            end;
        [{_, AVList}] ->
            S=#stat{dbg_list=DBGL} = lists:last(AVList),
            case Action of
                % всегда создаем новый показ. если предыдущий был не закрыт - потом при записи отфильтруем его
                <<"AC_SHOW">> ->
                    lists:append(AVList, [SFish#stat{start=Time}]);
                % апдейтим предыдущую запись - если конец был заполнен, то это ошибка с неизвестным источником
                <<"AC_FULLSHOW">> ->    
                    lists:append(lists:delete(S, AVList), [S#stat{stop=Time, dbg_list=[R|DBGL]}]);
                % апдейтим предыдущую запись, если время клика будет больше времени конца просмотра, то это ошибка  с неизвестным источником
                <<"AC_CLICK">> ->
                    lists:append(lists:delete(S, AVList), [S#stat{click=Time, dbg_list=[R|DBGL]}])
            end
    end,

    if
        length(StatList) > 0 -> 
            ?D("CS step: ~p~n", [{{Acv_video_url, Usid}, StatList}]),
            ets:insert(stat_clt, {{Acv_video_url, Usid}, StatList});
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
    Q = "select id, url, shown from acv_video where url in (" ++ 
        string:join(["'" ++ binary_to_list(X) ++ "'" || X <- lists:flatten(Acv_video_urls)], ",") ++ ");",
    ?D("QS1: ~p~n", [Q]),
    {ok, Advs} = dao:simple(Q),
    ?D("ADVS: ~p~n", [Advs]),
    to_db_acv_video_url(Advs, To_db).


to_db_acv_video_url([Adv|T], To_db) ->
    Key = proplists:get_value("url", Adv),
    Id = proplists:get_value("id", Adv),
    Shown = proplists:get_value("shown", Adv),

    Values = lists:flatten(proplists:get_all_values(list_to_binary(Key), To_db)),

    try
        create(Id)
    catch
        A:B -> ?D("info: ~p:~p", [A, B])
    end,

    Q1 = ?FMT("insert into acv_video_stat_~p ("
            "peer,"         % varchar(15)
            "video_url, "   % varchar(50)
            "node_name, "   % varchar(100)
            "start, "       % timestamp without time zone
            "stop, "        % timestamp without time zone
            "user_id, "     % varchar(100)
            "tmp_dbg, "     % text
            "click"         % timestamp without time zone
            ") values ", [Id]),

    ?D("VALUES: ~p~n", [Values]),

    ADV_Values = format_ADV(Values, []),
    ?D("ADV  VALUES: ~p~n", [ADV_Values]),
    Q = Q1 ++ string:join(ADV_Values, ", ") ++ ";",
    ?D("QQ:~p~n~n", [Q]),

    QUp = "update acv_video set shown=$1 where id=$2;",

    RQ = dao:with_connection_fk(fun(Con) ->
        R1 = pgsql:equery(Con, QUp, [Shown + length(Values), Id]),
        R2 = pgsql:equery(Con, Q),
        {R1, R2}
    end),
    ?D("RQ: ~p~n", [RQ]),
    to_db_acv_video_url(T, To_db);
to_db_acv_video_url([], _) ->
    done.


format_ADV([#stat{
            video_url=Video_url, peer=Peer, server_node=Trans_server_node_name,
            start = {{StartYear, StartMonth, StartDay}, {StartHour, StartMin, StartSec}}, 
            stop = {{StopYear, StopMonth, StopDay}, {StopHour, StopMin, StopSec}}, 
            click = Click, %{{ClickYear, ClickMonth, ClickDay},{ClickHour, ClickMin, ClickSec}}, 
            user_id = UserId,
            dbg_list=DBGL
        }|T], Ret) ->
    SL1 = [
        "'" ++ utils:to_list(Peer) ++ "'", 
        "'" ++ utils:to_list(Video_url) ++ "'",
        "'" ++ utils:to_list(Trans_server_node_name) ++ "'",
        ?FMT("'~p-~p-~p ~p:~p:~p'", [StartYear, StartMonth, StartDay, StartHour, StartMin, StartSec]),
        ?FMT("'~p-~p-~p ~p:~p:~p'", [StopYear, StopMonth, StopDay, StopHour, StopMin, StopSec]),
        "'" ++ utils:to_list(UserId) ++ "'", 
        ?FMT("'~p'", [DBGL])],
    case Click of
        {{ClickYear, ClickMonth, ClickDay},{ClickHour, ClickMin, ClickSec}} ->
            SL2 = [?FMT("'~p-~p-~p ~p:~p:~p'", [ClickYear, ClickMonth, ClickDay, ClickHour, ClickMin, ClickSec])];
        _ ->
            SL2 = ["null"]
    end,

    AL = "(" ++ string:join(lists:append(SL1, SL2), ", ") ++ ")",
    format_ADV(T, [AL|Ret]);
format_ADV([], Ret) ->
    lists:reverse(Ret).
    

collect([{{Acv_video_url, Usid}, StatList}|T], DBRecords) ->
    {Last, To_db} = collect_acv_stats(StatList, [], []),
    if
        length(Last) > 0    -> ets:insert(stat_clt, {{Acv_video_url, Usid}, Last});
        true                -> ets:delete(stat_clt, {Acv_video_url, Usid})
    end,

    collect(T, [{Acv_video_url, To_db}|DBRecords]);
    %NewList = proplists:get_value("Acv_video_url", DBRecords), To_db)
collect([], DBRecords) ->
    DBRecords.

% только первый кусок сообщения, кешируем в ets, если не прошло времени с прихода больше дельты
collect_acv_stats([S=#stat{start=Start, stop=null, click=null}|[]], Last, To_db) ->
    case calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(Start) > ?STAT_SKIP_SECOND of
        true -> % skip
            collect_acv_stats([], Last, To_db);
        false -> % wait
            collect_acv_stats([], [S|Last], To_db) % вообще-то last всегда == []
    end;

% скип. ошибка потока сознания от сервера
collect_acv_stats([#stat{stop=null, click=null}|T], Last, To_db) ->
    collect_acv_stats(T, Last, To_db);

% клик не пустой. ставим стоп=клик, в базу.
collect_acv_stats([S=#stat{stop=null, click=Click}|T], Last, To_db) ->
    collect_acv_stats(T, Last, [S#stat{stop=Click}|To_db]);

% корректная запись, без клика.
collect_acv_stats([S=#stat{start=Start, stop=Stop, click=null}|T], Last, To_db) when Start =< Stop ->
    collect_acv_stats(T, Last, [S|To_db]);

% корректная запись с кликом.
collect_acv_stats([S=#stat{start=Start, stop=Stop, click=Click}|T], Last, To_db) when Start =< Click, Click =< Stop ->
    collect_acv_stats(T, Last, [S|To_db]);

% все остальные случаи - скип
collect_acv_stats([S|T], Last, To_db) ->
    ?D("skip stat: ~p~n", S),
     collect_acv_stats(T, Last, To_db);

collect_acv_stats([], Last, To_db) ->
    {lists:reverse(Last), lists:reverse(To_db)}.


mk_ets() ->
    utils:make_ets(stat_clt, [{write_concurrency,true}]).


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

test()->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Нагрузочное тестирование
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mk_year(Year, VideoList) ->
    lists:seq(0, 364),
    lists:foreach(fun({Y, VL}) ->
        spawn(fun() ->
            mk_day(Y, VL)
        end)
    end, [{calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Year, 1, 1) + X), VideoList} || X <- lists:seq(0, 364)]).

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
    Q = "insert into acv_video_stat (ip, acv_video_id, video_uid, start, stop, clicked) values " ++
        string:join(QL, ", ") ++ ";",
    %?D("QQQ: ~p~n~n", [Q]).
    dao:simple(Q, PL).
 
gen_vstat() ->
    mk_year(2011, [integer_to_list(X) || X <- lists:seq(0, 3000)]).


test1() ->
    D1 = {{2011, 1,1},{0,0,0}}, 
    D2 = {{2011, 5,31},{0,0,0}},
    Q = "select * from acv_video_stat where video_uid = '1748' and start > $1 and stop < $2;",
    dao:simple(Q, [D1, D2]),
    done.

test(speed)->

    ok.

