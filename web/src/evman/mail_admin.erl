-module(mail_admin).
-behaviour(gen_event).

-include("common.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
     handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_event({signup, Msg}, State) ->
    ?D("mail_admin ~p | signup event for admin : ~p~n", [self(), Msg]),
    {ok, State};

handle_event(Event, State) ->
    ?D("mail_admin ~p | unknown event : ~p~n", [self(), Event]),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
