-module(evman_customer).

-define(EVENTNAME, ?MODULE).

%% API
-export([
    start_link/0,
    start_link/1,
    add_handler/2,
    add_guarded_handler/2,
    get_handlers/0,
    info/1
]).

-export([
    signup/1,
    signdown/1
]).

start_link() ->
    gen_event:start_link({local, ?EVENTNAME}).

start_link(Handler_Args) ->
    %%% Handler_Args = [{Handler, Args}, {Handler, Args}]
    Link = gen_event:start_link({local, ?EVENTNAME}),
    lists:foreach(fun({Handler, Args})->
        ?EVENTNAME:add_handler(Handler, [])
    end,Handler_Args),
    Link.

add_handler(ModuleName, Args) ->
    ok = gen_event:add_handler(?EVENTNAME, ModuleName, []).

add_guarded_handler(ModuleName, Args) ->
    {ok, Pid} = simple_hnd_guard:start(?EVENTNAME, ModuleName, Args),
    simple_hnd_guard:add(Pid).

get_handlers() ->
    gen_event:which_handlers(?EVENTNAME).

signup(Msg) ->
    % Пользователь зарегестрировался
    gen_event:notify(?EVENTNAME, {signup, Msg}).

signdown(Msg) ->
    % Пользователя удалили
    gen_event:notify(?EVENTNAME, {signdown, Msg}).

info(Msg) ->
    gen_event:notify(?EVENTNAME, Msg).

