%%%
%%% @file
%%%     Обработчик событий, который отправляет
%%%     почту пользователю (любому)
%%%     при возникновении различных событий
%%%

-module(mail_customer).
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
-define(MAILTYPE, customer).



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
    customer_facade(mail, create_customer, Data),
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
    customer_facade(mail, delete_customer, Data),
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
    customer_facade(mail, add_insider, Data),
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
    customer_facade(mail, del_insider, Data),
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
    acv_video_facade(mail, create_acv_video, Data),
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
    acv_video_facade(mail, delete_acv_video, Data),
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
    acv_video_facade(mail, mkbill, Data),
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
    acv_video_facade(mail, paybill, Data),
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


%%% ---------------------------------------------------------------------
%%%
%%% Внутренние методы
%%%
%%% ---------------------------------------------------------------------


%%%
%%% Фасад, для отправки почты при изменении кастомера
%%% 
customer_facade(Mod, Fun, Data) ->
    Id = proplists:get_value("id", Data),
    {ok, [Customer], _} = dao_customer:get_customer(Id),
    Rmail = proplists:get_value("email", Customer),
    Rname = proplists:get_value("login", Customer),
    Mod:Fun({?MAILTYPE, {Rmail, Rname}}, {data, Customer}).


%%%
%%% Фасад, для отправки почты при изменении кампании
%%% 
acv_video_facade(Mod, Fun, Data) ->
    Id = proplists:get_value("id", Data),
    {ok, [Acv_video], Geo_list, Cat_list} = dao_acv_video:get_acv_video(Id),
    Rmail = proplists:get_value("email", Acv_video),
    Rname = proplists:get_value("login", Acv_video),
    Mod:Fun({?MAILTYPE, {Rmail, Rname}}, {data, Acv_video}).
