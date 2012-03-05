-module(mail_mng).

-define(REF, ?MODULE).

%% API
-export([start_link/0, add_handler/2, add_guarded_handler/2,
         get_handlers/0, info/1]).

start_link() ->
    gen_event:start_link({local, ?REF}).

add_handler(ModuleName, Args) ->
    ok = gen_event:add_handler(?REF, ModuleName, [Args]).

add_guarded_handler(ModuleName, Args) ->
    {ok, Pid} = simple_hnd_guard:start(?REF, ModuleName, Args),
    simple_hnd_guard:add(Pid).

get_handlers() ->
    gen_event:which_handlers(?REF).

info(Msg) ->
    gen_event:notify(?REF, Msg).
