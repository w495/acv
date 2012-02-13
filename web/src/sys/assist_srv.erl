%%% @file assist_srv.erl
%%%
%%% Этому модулю никто не посылает запросов.
%%% Используется как таймер.
%%% События происходят по истечению
%%%     ASSIST_SRV_TIMEOUT
%%% и обрабатываются в handle_info(timeout, State)
%%%
%%% Более никакой функциональности сервер не несет
%%% Прежняя версия кода: assist_srv.erl.2
%%%

-module(assist_srv).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("common.hrl").
%% ----------------------------------------------------------------------------
%% Defines
%% ----------------------------------------------------------------------------

-define(ASSIST_SRV_TIMEOUT, 300000). %% 5*60*1000

%% ----------------------------------------------------------------------------
%% External exports
%% ----------------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{spawn_opt,[{min_heap_size,200000}]}]).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: []
%% --------------------------------------------------------------------
init([]) ->
    ?INFO(?FMT("ASSIST STARTING...~n~n~n", [])),
    process_flag(trap_exit, true),
    captcha = utils:make_ets(captcha, [{write_concurrency,true}]),
    captcha_time = utils:make_ets(captcha_time, [{write_concurrency,true}]),
    {ok, ?ASSIST_SRV_TIMEOUT}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call(Request, _From, State) ->
    flog:error(?FMT("~p:~p unexpected call: ~p~n", [?MODULE, ?LINE, Request])),
    {noreply, State, ?ASSIST_SRV_TIMEOUT}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
    flog:error(?FMT("~p:~p unexpected cast: ~p~n", [?MODULE, ?LINE, Msg])),
    {noreply, State, ?ASSIST_SRV_TIMEOUT}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) ->
    ?D("~n<handle_info 1>~n(timeout, State) = ~p ~n</handle_info>~n", [State]),
    web_session_DAO:removeExpired(),
    captcha:remove_expired(),
    dao_acv_video:delete_acv_video_shown_expired(),

    dao_stat:fetch_stat(),%%% Сбор статистики.
    {noreply, State, ?ASSIST_SRV_TIMEOUT};

handle_info(Info, State) ->
    flog:error(?FMT("~p:~p info at node ~p, info=~p~n", [?MODULE, ?LINE, node(), Info])),
    {noreply, State, ?ASSIST_SRV_TIMEOUT}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
%    ets:delete(msg),
    flog:info(?FMT("~p:~p terminated, reason: ~p~n", [?MODULE, ?LINE, Reason])),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Local
%% --------------------------------------------------------------------

