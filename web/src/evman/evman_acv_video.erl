-module(evman_acv_video).

-define(EVENTNAME, ?MODULE).

-include("common.hrl").

%% API
-export([start_link/0, start_link/1, add_handler/2, add_guarded_handler/2,
         get_handlers/0, info/1]).

-export([chstate/1]).

start_link() ->
    gen_event:start_link({local, ?EVENTNAME}).

start_link(Handler_Args) ->
    %%% Handler_Args = [{Handler, Args}, {Handler, Args}]
    ?D("~n~nHandler_Args = ~p", [Handler_Args]),
    Link = gen_event:start_link({local, ?EVENTNAME}),
    lists:foreach(fun({Handler, Args})->
        ?EVENTNAME:add_handler(Handler, [])
    end,Handler_Args),
    Link.

add_handler(ModuleName, Args) ->
    ?D("~n~nHandler, Args = ~p, ~p", [ModuleName, Args]),
    ok = gen_event:add_handler(?EVENTNAME, ModuleName, []).

add_guarded_handler(ModuleName, Args) ->
    {ok, Pid} = simple_hnd_guard:start(?EVENTNAME, ModuleName, Args),
    simple_hnd_guard:add(Pid).

get_handlers() ->
    gen_event:which_handlers(?EVENTNAME).

create(Msg) ->
    gen_event:notify(?EVENTNAME, {create, Msg}).

update(Msg) ->
    gen_event:notify(?EVENTNAME, {update, Msg}).

activate(Msg) ->
    gen_event:notify(?EVENTNAME, {activate, Msg}).

chstate(Msg) ->
    gen_event:notify(?EVENTNAME, {chstate, Msg}).

start(Msg) ->
    gen_event:notify(?EVENTNAME, {start, Msg}).

stop(Msg) ->
    gen_event:notify(?EVENTNAME, {stop, Msg}).

disactivate(Msg) ->
    gen_event:notify(?EVENTNAME, {disactivate, Msg}).

delete(Msg) ->
    gen_event:notify(?EVENTNAME, {delete, Msg}).


info(Msg) ->
    gen_event:notify(?EVENTNAME, Msg).

