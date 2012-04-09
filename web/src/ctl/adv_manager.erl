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
    DefResXlm = "<block duration=\"600\" loadnext=\"300\">"
                    "<creative category_id=\"16\" skip=\"no\" duration=\"600\" type=\"video\" start=\"0\" partner=\"videonow|doubleclick3\"/>"
                    "</block>", 
    Result = try
        Data = Req:parse_qs(),
        Tuple = norm:extr(Data, [
            {"type", [string]},
            {"resourse", [string]}
        ]),

        In = erlang:list_to_tuple(erlang:tuple_to_list(Tuple) ++ [proplists:get_value("userid", Data, null)]),

        XCountryCode = Req:get_header_value("X-Country-Code"),
        XCity = Req:get_header_value("X-City"),
        Peer = Req:get(peer),
        io:format("get_adv ::::::::::::::~p, ~p~n", [XCountryCode, XCity]),
        case biz_adv_manager:get_acv_ext(In, {XCountryCode, XCity}) of
            [] -> ResTry = DefResXlm;
            Val -> ResTry = biz_adv_manager:make_acv_xml(Val)
        end,
        ResTry
    catch Err:Reason ->
        ?ERROR({?MODULE, ?LINE, catch_err, {Err, Reason}}),
        DefResXlm
    end,
    ?D("~n!!!------------------------------~n!!! Result = ~p ~n!!!------------------------------~n ", [Result]),

    {"application/xml", [], [Result]}.

test()->
    ok.

test(speed)->
    ok.

