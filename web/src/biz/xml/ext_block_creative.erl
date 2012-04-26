%%%
%%% @file ext_block_creative.erl
%%%
%%%     Конкретное описание блоков и креативов.
%%%     В частности описание креативов
%%%         для сторонней рекламной системы
%%%

-module(ext_block_creative).

-export([
    creative/2,
    test/0,
    test/1
]).


-include("block_creative.hrl").

%%%
%%% @doc
%%%     Вощврашает статичное описание креатива {t, a, c}.
%%%     Никакая динамическая подстановка
%%%         функции внутри не производится
%%%
creative(_raw_creative, _funs) ->
    #creative{
        attributes = [
            {type, video},
            {category_id, 16},
            {duration, 600},
            {start, 0},
            {skip, no},
            {partner, "videonow|doubleclick3"}
        ]
    }.


test() ->
    ok.

test(speed) ->
    ok.
