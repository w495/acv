%%%
%%% @file
%%%     Обработчик событий, который отправляет
%%%     почту пользователю (любому)
%%%     при возникновении различных событий
%%%

-module(mail_customer).
-behaviour(gen_event).

-include("common.hrl").
-define(HANDLERNAME, ?MODULE).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {}).

init([]) ->
    {ok, #state{}}.

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
    {ok, Customer, _} = dao_customer:get_customer(Id),
    Rmail = proplists:get_value("email", Customer),
    Rname = proplists:get_value("login", Customer),
    mail:add_insider_customer({Rmail, Rname, {data, Customer}}),
    {ok, State};

handle_event({evman_customer, {change, {perm, {add, {insider, Insider}}}}}, State) ->
    {ok, State};

handle_event({evman_customer, {change, {perm, {add, Perm}}}}, State) ->
    %     ?D("~p ~p add perm from evman_customer~n",
    %         [?HANDLERNAME, self()]),
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
    {ok, Customer, _} = dao_customer:get_customer(Id),
    Rmail = proplists:get_value("email", Customer),
    Rname = proplists:get_value("login", Customer),

    mail:del_insider_customer({Rmail, Rname, {data, Customer}}),

    {ok, State};

handle_event({evman_customer, {change, {perm, {del, {insider, Insider}}}}}, State) ->

    {ok, State};

handle_event({evman_customer, {change, {perm, {del, Perm}} }}, State) ->
    %     ?D("~p ~p del perm from evman_customer~n",
    %         [?HANDLERNAME, self()]),
    {ok, State};

handle_event({evman_customer, {change, {perm, Perm} }}, State) ->
    %     ?D("~p ~p has changes in perms from evman_customer~n",
    %         [?HANDLERNAME, self()]),
    {ok, State};

handle_event({evman_customer, {change, Changes }}, State) ->
%     ?D("~p ~p has changes from evman_customer~n",
%         [?HANDLERNAME, self()]),
    {ok, State};

handle_event({evman_customer, _event}, State) ->
    %     ?D("~p ~p has unknown event from evman_customer~n",
    %         [?HANDLERNAME, self()]),
    {ok, State};

handle_event({_sender, _event}, State) ->
    %     ?D("~p ~p has unknown event from ~p, `~p' and state `~p' ~n",
    %         [?HANDLERNAME, self(), _sender, _event, State]),
    {ok, State};

handle_event(_event, State) ->
    %     ?D("~p ~p has unknown event `~p' and state `~p' ~n",
    %         [?HANDLERNAME, self(), _event, State]),
    {ok, State}.





handle_call(_request, State) ->
    %     ?D("~p ~p has unknown call `~p' and state `~p' ~n",
    %         [?HANDLERNAME, self(), _request, State]),
    {ok, ok, State}.

handle_info(_info, State) ->
    %     ?D("~p ~p has unknown info `~p' and state `~p' ~n",
    %         [?HANDLERNAME, self(), _info, State]),
    {ok, State}.

terminate(_reason, _state) ->
    %     ?D("~p ~p was terminate with reason `~p' and state `~p' ~n",
    %         [?HANDLERNAME, self(), _reason, _state]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

