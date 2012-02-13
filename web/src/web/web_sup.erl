-module(web_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include("common.hrl").

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

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->

    Assist_srv = {
        assist_srv,
        {assist_srv, start_link, []},
        permanent, 5000, worker, [assist_srv]},

    CLog = {
        clog,
        {clog, start_link, []},
        permanent,
        1000,
        worker,
        [clog]
    },

    
    PgConPool = {   %%% соединение с локальной базой данных
        pgConPoolFK,
        {pgConPool, start_link, [
            config:get(fk_db_user, "w-495"),
            config:get(fk_db_password, "eiir"),
            config:get(fk_db_name, "fk"),
            config:get(fk_db_host, "localhost")
        ]},
        permanent,
        1000,
        worker,
        [pgConPool]
    },
    %%% pgConPool:start_link(config:get(fk_db_user, "w-495"),config:get(fk_db_password, "eiir"),config:get(fk_db_name, "fk"),config:get(fk_db_host, "localhost"))

    ?D("---------------------------------------~n", []),
    ?D("---------------------------------------~n", []),
    ?D("PgConPool = ~p~n", [PgConPool]),
    ?D("---------------------------------------~n", []),
    ?D("---------------------------------------~n", []),

    %%% mysql_con_pool:start_link(config:get(vk_db_user, "w-495"),config:get(vk_db_password, "1111"),config:get(vk_db_name, "vk"),config:get(fk_db_host, "localhost"))

    MySqlConPool = { %%% соединение с внешней базой данных
        mySqlConPoolFK,
        {mysql_con_pool, start_link, [
            mySqlConPool,
            config:get(vk_db_host, "localhost"),
            config:get(vk_db_user, "root"),
            config:get(vk_db_password, "1111"),
            config:get(vk_db_name, "vk"),
            10 % количество соединений
        ]},
        permanent,
        1000,
        worker,
        [mySqlConPool]
    },

    MySqlConPoolStat = { %%% соединение с внешней базой данных статистики
        mySqlConPoolStat,
        {mysql_con_pool, start_link, [
            mysqlStat,
            config:get(stat_db_host, "localhost"),
            config:get(stat_db_user, "root"),
            config:get(stat_db_password, "1111"),
            config:get(stat_db_name, "vk"),
            10 % количество соединений
        ]},
        permanent,
        1000,
        worker,
        [mySqlConPoolStat]
    },

%    Emysql = {
%    },

    mysql:start_link(mySqlConPool, config:get(vk_db_host, "localhost"), config:get(vk_db_user, "root"), 
        config:get(vk_db_password, "1111"), config:get(vk_db_name, "vk")),
    mysql:connect(mysqlStat, 
        config:get(stat_db_host, "localhost"), 
        undefined, 
        config:get(stat_db_user, "root"), 
        config:get(stat_db_password, "1111"), 
        config:get(stat_db_name, "vk"),
        true),
    dao_stat:mk_ets(),

    Xslt_processor = { %%% преодбразователь
        xslt_sup,
        {xslt_sup, start_link, []},
        permanent,
        1000,
        supervisor,
        [xslt]
    },

    Processes = [
        CLog,           % Логирование
        Assist_srv,      % Авторизация
        PgConPool,      % Связь с локальной базой
        Xslt_processor
%        ,MySqlConPool   % Связь с tvzavr vk
%        ,MySqlConPoolStat
    ],
    {ok, {{one_for_one, 10, 10}, Processes}}.

