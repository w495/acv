-module(web_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).


-include("common.hrl").

-define(WEB_SUP_TIMEOUT,    5000).
-define(WEB_SUP_DB_TIMEOUT, 1000).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),
    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),
    sets:fold(fun (Id, ok) ->
        supervisor:terminate_child(?MODULE, Id),
        supervisor:delete_child(?MODULE, Id),
        ok
    end, ok, Kill),
    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

addMysqlPoolConnection(_, 0, _) ->
    done;
addMysqlPoolConnection(Mysql, Count, Params={Host, Port, User, Pass, DB}) ->
    mysql:connect(Mysql, Host, Port, User, Pass, DB, true),
    addMysqlPoolConnection(Mysql, Count-1, Params).


%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Assist_srv = {
        assist_srv,
        {assist_srv, start_link, []},
        permanent,
        ?WEB_SUP_TIMEOUT,
        worker,
        [assist_srv]
    },
    %%% Логирование
    CLog = {
        clog,
        {clog, start_link, []},
        permanent,
        ?WEB_SUP_TIMEOUT,
        worker,
        [clog]
    },
    %%% События (отсылка почты)
    Evman_sup = {
        evman_sup,
        {evman_sup, start_link, []},
        permanent,
        ?WEB_SUP_TIMEOUT,
        supervisor,
        [evman_sup]
    },
    %%% Соединение с локальной базой данных
    PgConPool = {   
        pgConPoolFK,
        {pgConPool, start_link, [
            config:get(fk_db_user, "w-495"),
            config:get(fk_db_password, "eiir"),
            config:get(fk_db_name, "fk"),
            config:get(fk_db_host, "localhost")
        ]},
        permanent,
        ?WEB_SUP_DB_TIMEOUT,
        worker,
        [pgConPool]
    },
    %%% Соединение с внешней базой данных
    MysqlVK = { 
        mysqlVK,
        {mysql, start_link, [
            mySqlConPool, 
            config:get(vk_db_host, "localhost"), 
            config:get(vk_db_user, "root"),
            config:get(vk_db_password, "1111"),
            config:get(vk_db_name, "vk")
        ]},
        permanent,
        ?WEB_SUP_DB_TIMEOUT,
        worker,
        [mysql]
    },

    %%% Соединение с внешней базой данных статистики
    MysqlStat = {
        mysqlStat,
        {mysql, start_link, [
            mysqlStat, 
            config:get(stat_db_host, "localhost"), 
            config:get(stat_db_user, "root"),
            config:get(stat_db_password, "1111"), 
            config:get(stat_db_name, "vk")
        ]},
        permanent,
        ?WEB_SUP_DB_TIMEOUT,
        worker,
        [mysql]
    },


    %%% Шаблонизатор
    Xslt_processor = { 
        xslt_sup,
        {xslt_sup, start_link, []},
        permanent,
        ?WEB_SUP_TIMEOUT,
        supervisor,
        [xslt]
    },
    Processes = [
        CLog,            % Логирование
        Evman_sup,       % Сервер событий
        PgConPool,       % Связь с локальной базой
        Xslt_processor,  % Шаблонизатор
        MysqlVK,         % Связь с tvzavr vk
        MysqlStat,       % Связь с tvzavr AVstat
        Assist_srv       % Вспомогательный сервер
    ],
    {ok, {{one_for_one, 10, 10}, Processes}}.

