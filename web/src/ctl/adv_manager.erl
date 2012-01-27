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
        {"userid", [nullable, string]}
    ]),

    Peer = Req:get(peer),
    Result = biz_adv_manager:get_acv_mp4(In, Peer),

    Fake_result = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<block duration=\"120\" loadnext=\"600\">"
    "  <creative type=\"video\""
    "            click=\"http://click-here/\""
    "            link_title=\"Adv from get here\""
    "            url=\"http://get-here/video.flv\""
    "            start=\"0\""
    "            skip=\"no\""
    "            duration=\"10\"/>"
    "  <creative type=\"video\""
    "            click=\"http://click-here/\""
    "            url=\"http://get-here/video.flv\""
    "            start=\"10\""
    "            skip=\"partner\""
    "            duration=\"10\">"
    "    <event type=\"start\" action=\"get\" url=\"http://some.url/here\"/>"
    "  </creative>"
    "  <creative type=\"video\""
    "            partner=\"partner.name\""
    "            start=\"20\""
    "            skip=\"yes\""
    "            category_id=\"0\""
    "            duration=\"10\"/>"
    "  <creative type=\"paused\""
    "            click=\"http://click-here/\""
    "            media='img'"
    "            url=\"http://get-here/paused.jpg\"/>"
    "</block>",

    {?OUTPUT_HTML, [], [Result]}.

test()->
    ok.

test(speed)->
    ok.

