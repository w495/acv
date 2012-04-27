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

%%% TODO:
%%%     Разнести часть логики в biz и dao
%%%


-include("common.hrl").
-include("web.hrl").

get_adv(Req) ->
    %%% TODO:
    %%%     Есть мысль от Саши Рыкова, чтобы не возвращался loadnext
    %%%     в случае postroll, или был равен -1
    %%%
    Loadnext = dao_sysvar:acv_video_loadnext(),
    DefResXlm =
        "<block "
            "duration=\"600\" "
            "loadnext=\"" ++ convert:to_list(Loadnext) ++
        "\">"
            "<creative "
                " category_id=\"16\" " % 161 для 694 - id жанра
                " skip=\"no\" "
                " duration=\"600\" "
                " type=\"video\" "
                " start=\"0\" "
                " partner=\"videonow|doubleclick3\" "
            "/>"
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
        case biz_adv_manager:get_acv_ext(In, {XCountryCode, XCity}) of
            [] -> ResTry = DefResXlm;
            Val -> ResTry = biz_adv_manager:make_acv_xml(Val)
        end,
        ResTry
    catch 
        Error ->
            ?ERROR(?FMT("~p~n", [{?MODULE, ?LINE, catch_err, Error}])),
            DefResXlm;
    
        Err:Reason ->
            ?ERROR(?FMT("~p~n", [{?MODULE, ?LINE, catch_err, {Err, Reason}}])),
            DefResXlm
    end,
    ?D("~n!!!------------------------------~n!!! Result = ~p ~n!!!------------------------------~n ", [Result]),

    {?OUTPUT_XML, [], [Result]}.

test()->
    ok.

test(speed)->
    ok.

