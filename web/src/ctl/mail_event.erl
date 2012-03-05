%%% @file adv_manager.erl
%%%
%%%     Контроллер работы с почтой
%%%


-module(mail_event).
-behaviour(gen_event).

-include("../include/common.hrl").
-include("../include/web.hrl").


%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
     handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

init([_Args]) ->
    {ok, #state{}}.

handle_event({mail, Message}, State) ->
    io:format("eh ~p | Mail Message --> (~p)~n", [self(), Message]),
    {ok, State};

handle_event(Event, State) ->
    io:format("eh ~p | event : ~p~n", [self(), Event]),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) -> {ok, State}.
terminate(_Reason, _State) ->  ok.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.
