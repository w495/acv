-module(evman_acv_video).
-behaviour(evman_gen_notifier).

-include("common.hrl").
-define(EVENTNAME, ?MODULE).

-export([
    start_link/0,
    start_link/1,
    add_handler/2,
    add_guarded_handler/2,
    rem_handler/1,
    rem_guarded_handler/1,
    get_handlers/0,
    info/1
]).


-export([
    create/1,
    delete/1,
    change/1,
    stop/1,
    start/1,
    activate/1,
    disactivate/1,
    change_bill/1,
    mkbill/1,
    paybill/1
]).


%%% -----------------------------------------------------------------------
%%% evman_gen_notifier API
%%% -----------------------------------------------------------------------

start_link() ->
    evman_gen_notifier:start_link(?EVENTNAME).

start_link(Args) ->
    evman_gen_notifier:start_link(?EVENTNAME, Args).

add_handler(ModuleName, Args) ->
    evman_gen_notifier:add_handler(?EVENTNAME, ModuleName, []).

add_guarded_handler(ModuleName, Args) ->
    evman_gen_notifier:add_guarded_handler(?EVENTNAME, ModuleName, Args).

rem_handler(ModuleName) ->
    evman_gen_notifier:rem_handler(?EVENTNAME, ModuleName).

rem_guarded_handler(ModuleName) ->
    evman_gen_notifier:rem_guarded_handler(?EVENTNAME, ModuleName).

get_handlers() ->
    evman_gen_notifier:get_handlers(?EVENTNAME).

info(Msg) ->
    evman_gen_notifier:info(?EVENTNAME, Msg).

%%% -----------------------------------------------------------------------
%%% API
%%% -----------------------------------------------------------------------


%%%
%%% @doc
%%%     Сообщает о создании состояния
%%%
create(Msg) ->
    info({create, Msg}).

%%%
%%% @doc
%%%     Сообщает об удалении аккаунта
%%%
delete(Msg) ->
    info({delete, Msg}).

%%%
%%% @doc
%%%     Сообщает об изменении состояния
%%%
change(Msg) ->
    info({change, Msg}).

%%%
%%% @doc
%%%     Сообщает об изменении состояния счета
%%%
change_bill(Msg) ->
    change({bill, Msg}).

%%%
%%% @doc
%%%     Сообщает о том, что счет выставлен 
%%%
mkbill(Msg) ->
    change_bill({make, Msg}).

%%%
%%% @doc
%%%     Сообщает о том, что счет оплачен
%%%
paybill(Msg) ->
    change_bill({pay, Msg}).


activate(Msg) ->
    change({activate, Msg}).


disactivate(Msg) ->
    change({disactivate, Msg}).

start(Msg) ->
    change({start, Msg}).

stop(Msg) ->
    change({stop, Msg}).

