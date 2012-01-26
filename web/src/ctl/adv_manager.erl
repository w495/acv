%%% @file adv_manager.erl
%%%
%%%     Контроллер раздачи рекламы
%%%

-module(adv_manager).

-export([
    get_adv/1,
    test/0,
    test/1
]).


-include("../include/common.hrl").
-include("../include/web.hrl").

get_adv(Req) ->
    Data = Req:parse_qs(),
    In = norm:extr(Data, [
        {"type", [string]},
        {"resourse", [string]},
        {"uid", [nullable, integer]}
    ]),
    XML = dao:dao_call(biz_adv_manager, get_adv, In),
    {?OUTPUT_HTML, [], [XML]}.

test()->
    ok.

test(speed)->
    ok.

