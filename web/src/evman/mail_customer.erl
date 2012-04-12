-module(mail_customer).
-behaviour(gen_event).

-include("common.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
     handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_event({signup, Msg}, State) ->
    ?D("mail_customer ~p | signup event for customer : ~p~n", [self(), Msg]),
    {ok, State};

handle_event({chstate, Id}, State) ->
    {ok, [Acv_video], _, _} = dao_acv_video:get_acv_video(Id),

    Rmail = proplists:get_value("email", Acv_video),
    Rname = proplists:get_value("login", Acv_video),

    case {
        proplists:get_value("active", Acv_video),
        proplists:get_value("pay_status", Acv_video)
    } of
        {true, null} ->
            mail:mkbill({Rmail, Rname, {data, Acv_video}}),
            %%dao_acv_video:mkbill(Id),
            ok;
        _ ->
            ok
    end,

    ?D("mail_customer ~p | chstate : ~p~n", [self(), Acv_video]),
    {ok, State};


handle_event(Event, State) ->
    ?D("mail_customer ~p | unknown event : ~p~n", [self(), Event]),
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
