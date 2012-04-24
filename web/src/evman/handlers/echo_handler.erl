%%%
%%% @file evman_gen_handler.erl
%%%     Абстрактный обработчик событий.
%%%     Можно переобределить его методы с помощью.
%%%
%%%         -behaviour(gen_event).
%%%         -extends(evman_gen_handler).
%%%

-module(echo_handler).
-behaviour(gen_event).

%%%
%%% Используем наследование модулей,
%%% чтобы не тянуть кучу одинаковых методов.
%%% Если надо, мы всегда сможем из переопределить.
%%%
-extends(evman_gen_handler).
-export([
    handle_event/2
]).

-include("common.hrl").
-define(HANDLERNAME, ?MODULE).



%%% ---------------------------------------------------------------------
%%%
%%% Обработка событий создания кастомера
%%%
%%% ---------------------------------------------------------------------

handle_event(
    {evman_customer,
        {create,
            {data, Data}
        }
    }, State ) ->
    %%% when proplists:is_defined("id", Data)
    ?I("Customer ~p was created ~n",
        [proplists:get_value("id", Data)]),
    {ok, State};


%%% ---------------------------------------------------------------------
%%%
%%% Обработка событий удаления кастомера
%%%
%%% ---------------------------------------------------------------------

handle_event(
    {evman_customer,
        {delete,
            {data, Data}
        }
    }, State ) ->
    %%% when proplists:is_defined("id", Data)
    ?I("Customer ~p was deleted ~n",
        [proplists:get_value("id", Data)]),
    {ok, State};


%%% ---------------------------------------------------------------------
%%%
%%% Обработка событий изменения кастомера
%%%
%%% ---------------------------------------------------------------------

%%%
%%% Мы тут используем {insider, {data, Data}}
%%%     вместо  {insider, {data, Data}}
%%%     для ослабления связанности методов. по сути в Data
%%%     может юыть передано что угодно, и не обязательно,
%%%     что там будут все необходимые данные для обработки события.
%%%     Нам нужно, что бы там был точно "id"
%%%     Все остальные данные мы получим на основе этого id.
%%%
handle_event(
    {evman_customer,
        {change,
            {perm,
                {add,
                    {insider,
                        {data, Data}
                    }
                }
            }
        }
    }, State ) ->
    %%% when proplists:is_defined("id", Data)
    ?D("~n>>>> ~p ~p HAS EVENT ~n",
        [?HANDLERNAME, self()]),
    ?I("Customer ~p is insider ~n",
        [proplists:get_value("id", Data)]),
    {ok, State};

handle_event(
    {evman_customer,
        {change,
            {perm,
                {del,
                    {insider,
                        {data, Data}
                    }
                }
            }
        }
    }, State) ->
    %%% when proplists:is_defined("id", Data)
    ?I("Customer ~p is NOT insider ~n",
        [proplists:get_value("id", Data)]),
    {ok, State};


%%% ---------------------------------------------------------------------
%%%
%%% Обработка событий создания рекламной кампании
%%%
%%% ---------------------------------------------------------------------

handle_event(
    {evman_acv_video,
        {create,
            {data, Data}
        }
    }, State ) ->
    %%% when proplists:is_defined("id", Data)
    ?I("Acv_video ~p was created ~n",
        [proplists:get_value("id", Data)]),
    {ok, State};


%%% ---------------------------------------------------------------------
%%%
%%% Обработка событий удаления рекламной кампании
%%%
%%% ---------------------------------------------------------------------

handle_event(
    {evman_acv_video,
        {delete,
            {data, Data}
        }
    }, State ) ->
    %%% when proplists:is_defined("id", Data)
    ?I("Acv_video ~p was deleted ~n",
        [proplists:get_value("id", Data)]),
    {ok, State};

%%% ---------------------------------------------------------------------
%%%
%%% Обработка событий изменения рекламной кампании
%%%
%%% ---------------------------------------------------------------------

handle_event(
    {evman_acv_video,
        {change,
            {bill,
                {make,
                    {data, Data}
                }
            }
        }
    }, State ) ->
    %%% when proplists:is_defined("id", Data)
    ?I("Acv_video ~p made bill ~n",
        [proplists:get_value("id", Data)]),
    {ok, State};


handle_event(
    {evman_acv_video,
        {change,
            {bill,
                {pay,
                    {data, Data}
                }
            }
        }
    }, State ) ->
    ?I("Acv_video ~p paid bill ~n",
        [proplists:get_value("id", Data)]),
    {ok, State};

%%% ---------------------------------------------------------------------
%%%
%%% Отнаследованные методы
%%%
%%% ---------------------------------------------------------------------

%%%
%%% Для всего остального вызываем функции надмодуля.
%%%
handle_event(Some, State) ->
    ?BASE_MODULE:handle_event(Some, State).

