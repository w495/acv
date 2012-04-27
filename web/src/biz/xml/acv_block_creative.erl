%%%
%%% @file block_creative.erl
%%%
%%%     Конкретное описание блоков и креативов.
%%%     В частности описание креативов
%%%         от нашей рекламной системы
%%%

-module(acv_block_creative).

-export([
    fast_creative/2,
    creative/2,
    creative_url/2,
    creative_streamer/2,
    test/0,
    test/1
]).

-include("block_creative.hrl").


%%%
%%% @doc
%%%     Вощврашает описание креатива {t, a, c}.
%%%     Никакая динамическая подстановка
%%%         функции внутри не производится
%%%
fast_creative(Raw_creative, _) ->
    Vk_streamer =
        "vk_streamer",
        %config:get(vk_streamer, ?VK_STREAMER_DEFAULT),
    #creative{
        attributes = [
            {type, video},
            {click, proplists:get_value("url", Raw_creative)},
            {link_title, proplists:get_value("link_title", Raw_creative)},
            {url,
                Vk_streamer ++
                    string:strip(proplists:get_value("ref", Raw_creative))
            },
            {start, 0},
            {skip, no},
            {duration, proplists:get_value("duration", Raw_creative)}
        ]
    }.


%%%
%%% @doc
%%%     Вощврашает описание креатива {t, a, c}
%%%     На вход
%%%         Raw_creative --- proplists креативова такой,
%%%                             какой возвращается из нашей базы
%%%         Funs --- proplists функций преобразования входных параметров.
%%%
%%%             Funs = [
%%%                 % должна имя стримящего сервера
%%%                     {creative_streamer, fun/2 -> [] },
%%%                 % должна значение аттрибута url креатива
%%%                     {creative_url,  fun/2 -> #creative{} }
%%%             ]
%%%
%%%         Если функции не определены в Funs,
%%%         то используются одноименные функции текушего модуля.
%%%         Параметры перегружаемых функций имют такую жу структуру как вызвывющй
%%%
%%%             1) объект
%%%             2) функции Funs
%%%
creative(Raw_creative, Funs) ->
    Creative_url = proplists:get_value(creative_url, Funs, fun ?MODULE:creative_url/2),
    #creative{
        attributes = [
            {type, video},
            {click, proplists:get_value("url", Raw_creative)},
            {link_title, proplists:get_value("link_title", Raw_creative)},
            {url, Creative_url(Raw_creative, Funs)},
            {start, 0},
            {skip, no},
            {duration, proplists:get_value("duration", Raw_creative)}
        ]
    }.

%%%
%%% @doc
%%%     Вощврашает значение url креатива.
%%%     Оно конструируется на основе параметров Raw_creative
%%%         и того что возвращает fun creative_streamer/2
%%%     На вход
%%%         Raw_creative --- proplists креативова такой,
%%%                             какой возвращается из нашей базы
%%%         Funs --- proplists функций преобразования входных параметров.
%%%
%%%             Funs = [
%%%                 % должна имя стримящего сервера
%%%                     {creative_streamer, fun/2 -> [] },
%%%             ]
%%%
%%%         Если функции не определены в Funs,
%%%         то используются одноименные функции текушего модуля.
%%%         Параметры перегружаемых функций имют такую жу структуру как вызвывющй
%%%
%%%             1) объект
%%%             2) функции Funs
%%%
creative_url(Raw_creative, Funs) ->
    Creative_streamer = proplists:get_value(creative_streamer, Funs, fun ?MODULE:creative_streamer/2),
    Creative_streamer(Raw_creative, Funs) ++
        string:strip(proplists:get_value("ref", Raw_creative)).

%%%
%%% @doc
%%%     Возвращает значение имени стримящего сервера
%%%     Должно браться из конфига,
%%%
creative_streamer(_raw_creative, _funs)->
    %config:get(vk_streamer, ?VK_STREAMER_DEFAULT),
    "vk_streamer".


test() ->
    ok.

test(speed) ->
    ok.
