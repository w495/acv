%%%
%%% @file
%%%     Обработчик событий, который отправляет
%%%     почту пользователям с правами sysmsg
%%%     при возникновении различных событий
%%%

-module(mail_sysmsg).
-behaviour(gen_event).

%%%
%%% Используем наследование модулей,
%%% чтобы не тянуть кучу одинаковых методов.
%%% Если надо, мы всегда сможем из переопределить.
%%% 
-extends(evman_gen_handler).

-include("common.hrl").

-export([handle_event/2]).


%%%
%%%
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
    ?I("Customer ~p is insider ~n", [proplists:get_value("id", Data)]),
    Id = proplists:get_value("id", Data),
    {ok, [Customer], _} = dao_customer:get_customer(Id),
    Rmail = proplists:get_value("email", Customer),
    Rname = proplists:get_value("login", Customer),
    mail:add_insider_customer({Rmail, Rname, {data, Customer}}),
    {ok, State};


%%%
%%%
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
    Id = proplists:get_value("id", Data),
    {ok, [Customer], _} = dao_customer:get_customer(Id),
    ?D("Customer = ~p~n", [Customer]),
    Rmail = proplists:get_value("email", Customer),
    Rname = proplists:get_value("login", Customer),
    ?D("email = ~p~n", [Rmail]),
    ?D("login = ~p~n", [Rname]),

    mail:del_insider_customer({Rmail, Rname, {data, Customer}}),

    {ok, State};

%%%
%%% Для всего остального вызываем функции надмодуля.
%%% 
handle_event(Some, State) ->
    ?BASE_MODULE:handle_event(Some, State).
