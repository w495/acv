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
        {"resourse", [string]}
        {"userid", [nullable, string]}
    ]),

    erlang:list_to_tuple(erlang:tuple_to_list(Tuple) ++ [proplists:get_value("userid", Data, null)])

    Peer = Req:get(peer),
    Result = biz_adv_manager:get_acv_mp4(In, Peer),


%     Fake_result_1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> "
%     "<block duration=\"120\" loadnext=\"600\"> "
%     "<creative  type=\"video\" click=\"http://ya.ru\"  link_title=\"354b9bd8-c2aa-4a92-81ee-0fccc85a9273\" url=\"http://192.168.2.187:8000/static/data/acv-video/common/130387262/adv10.mp4\" start=\"0\" skip=\"no\"  duration=\"20\"  /> "
%     "</block> ",
% 
% 
%     Fake_result_2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
%     "<block duration=\"120\" loadnext=\"600\">"
%     "  <creative type=\"video\""
%     "            click=\"http://click-here/\""
%     "            link_title=\"Adv from get here\""
%     "            url=\"http://get-here/video.flv\""
%     "            start=\"0\""
%     "            skip=\"no\""
%     "            duration=\"10\"/>"
%     "  <creative type=\"video\""
%     "            click=\"http://click-here/\""
%     "            url=\"http://get-here/video.flv\""
%     "            start=\"10\""
%     "            skip=\"partner\""
%     "            duration=\"10\">"
%     "    <event type=\"start\" action=\"get\" url=\"http://some.url/here\"/>"
%     "  </creative>"
%     "  <creative type=\"video\""
%     "            partner=\"partner.name\""
%     "            start=\"20\""
%     "            skip=\"yes\""
%     "            category_id=\"0\""
%     "            duration=\"10\"/>"
%     "  <creative type=\"paused\""
%     "            click=\"http://click-here/\""
%     "            media='img'"
%     "            url=\"http://get-here/paused.jpg\"/>"
%     "</block>",

    {?OUTPUT_HTML, [], [Result]}.

test()->
    ok.

test(speed)->
    ok.

