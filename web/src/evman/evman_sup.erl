-module(evman_sup).

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


    Evman_acv_video = {
        evman_acv_video,
        {evman_acv_video, start_link,
            [
                [
                    {echo_handler, []},
                    {mail_customer, []},
                    {mail_sysmsg, []}
                ]
            ]
        },
        permanent,
        5000,
        worker,
        [evman_acv_video]
    },

    Evman_customer = {
        evman_customer,
        {
            evman_customer,
            start_link,
            [
                [
                    {echo_handler, []},
                    {mail_customer, []},
                    {mail_sysmsg, []}
                ]
            ]
        },
        permanent,
        5000,
        worker,
        [evman_customer]
    },

    Processes = [
        Evman_customer,         % События заказчика
        Evman_acv_video         % События видео
    ],
    {ok, {{one_for_one, 10, 10}, Processes}}.

