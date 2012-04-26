%%%
%%% @file block_creative.erl
%%%
%%%     Абстрактное описание блоков и креативов.
%%%

-module(block_creative).

-export([
    block/1,
    block/2,
    creatives/1,
    creatives/2,
        % переопределяемая
    creative/1,
    creative/2,
        % переопределяемая
    block_duration/1,
    block_duration/2,
        % переопределяемая
    block_loadnext/1,
    block_loadnext/2,
        % переопределяемая
    test/0,
    test/1
]).

-include("block_creative.hrl").

block(Raw_creatives) ->
    block(Raw_creatives, []).

%%%
%%% @doc
%%%     Вощврашает описание блока в формате {t, a, c}
%%%     На вход
%%%         Raw_creatives --- список proplists креативов
%%%         Funs --- proplists функций преобразования входных параметров.
%%%
%%%             Funs = [
%%%                 % должна возвращать список креативов
%%%                     {creatives, fun/2 -> [] },
%%%                 % должна возвращать креатив
%%%                     {creative,  fun/2 -> #creative{} },
%%%                 % должна возвращать значение атрибутв
%%%                     {block_duration,  fun/2 -> integer() },
%%%                 % должна возвращать атрибута loadnext
%%%                     {block_loadnext,  fun/2 -> integer() }
%%%             ]
%%%
%%%         Если функции не определены в Funs,
%%%         то используются одноименные функции текушего модуля.
%%%         Параметры перегружаемых функций имют такую жу структуру как вызвывющй
%%%
%%%             1) объект
%%%             2) функции Funs
%%%
block(Raw_creatives, Funs) ->
    Mkcreatives  =
        proplists:get_value(
            creatives,
            Funs,
            fun ?MODULE:creatives/2
        ),
    Block_duration =
        proplists:get_value(
            block_duration,
            Funs,
            fun ?MODULE:block_duration/2
        ),
    Block_loadnext =
        proplists:get_value(
            block_loadnext,
            Funs,
            fun ?MODULE:block_loadnext/2
        ),

    Creatives = Mkcreatives(Raw_creatives, Funs),
    #block{
        attributes =
            [
                {duration, Block_duration(Creatives, Funs)},
                {loadnext, Block_loadnext(Creatives, Funs)}
            ],
        children = Creatives
    }.

%%%
%%% @doc
%%%     Обертка для creatives/2
%%%
creatives(Raw_creatives) ->
    creatives(Raw_creatives, []).

%%%
%%% @doc
%%%     Вощврашает список креативов [{t, a, c}]
%%%
%%%         Raw_creatives --- список proplists креативов
%%%         Funs --- proplists функций преобразования входных параметров.
%%%
%%%             Funs = [
%%%                     {creative,  fun/2 -> #creative{} }
%%%             ]
%%%
%%%         Если функции не определены в Funs,
%%%         то используются одноименные функции текушего модуля.
%%%         Параметры перегружаемых функций имют
%%%         такую жу структуру как вызвывющй
%%%
%%%             1) объект
%%%             2) функции Funs
%%%
creatives(Raw_creatives, Funs) ->
    Mkcreative  =
        proplists:get_value(
            creative,
            Funs,
            fun ?MODULE:creative/2
        ),
    [Mkcreative(Raw_creative, Funs) || Raw_creative <- Raw_creatives].

%%%
%%% @doc
%%%     Обертка для creative/2
%%%
creative(Raw_creative) ->
    creative(Raw_creative, []).

%%%
%%% @doc
%%%     Вощврашает креатив {t, a, c}
%%%
%%%         Raw_creatives --- список proplists креативов
%%%
creative(_Raw_creative, _funs) ->
    #creative{
        attributes =
            [
                {type, video},
                {click, 0},
                {link_title, 0},
                {url, 0},
                {skip, 0},
                {duration, 1}
            ]
    }.


%%%
%%% @doc
%%%     Обертка для block_duration/2
%%%
block_duration(Creatives) ->
    block_duration(Creatives, []).

%%%
%%% @doc
%%%     Возвращает значение атрибутв duration
%%%     В данном случае вычисляется
%%%         на основе суммы одноименных атрибутов
%%%         для креативов
%%%
block_duration(Creatives, _) ->
    xml_utils:compute_args_sum(duration, Creatives).

%%%
%%% @doc
%%%     Обертка для block_loadnext/2
%%%
block_loadnext(Creatives) ->
    block_loadnext(Creatives, []).

%%%
%%% @doc
%%%     Возвращает значение атрибутв duration
%%%     Должна браться из конфига, 
%%%
block_loadnext(_, _) ->
    10.


test() ->
    ok.

test(speed) ->
    ok.
