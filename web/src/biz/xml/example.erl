%%%
%%% @file example.erl
%%%
%%%     Пример использования библиотеки для удобной генерации xml
%%%         рекламных креативов.
%%%     В данном случае мы используем библиотеку xmerl.
%%%
%%%     Кроме того используется механним переопределения функций.
%%%     Это можно использовать для однотипной генерации xml
%%%         рекламных креатиавов нашей системы (acv)
%%%         и старой (ext - внешней).
%%%
%%%

-module(example).

-export([
    main/0
]).

main() ->

    %%%
    %%% Описывам креативы как список proplist.
    %%% В таком виде они возвращаются из базы данных.
    %%%
    Acv_creatives =
    [
        [
            {"url", "http://url.ru"},
            {"link_title", "l t"},
            {"duration", 32},
            {"ref", "ref"}
        ]
    ],

    %%%
    %%% Описывам перегруженные функции как proplist.
    %%% Вообще можно применить и обычный fun(X, Y) -> end.
    %%%
    Acv_funs_1 = [
        {creative, fun acv_block_creative:creative/2}
    ],

    %%%
    %%% Получаем обратно binary c xml
    %%%
    Acv_xml_1 = xml_utils:simple(block_creative:block(Acv_creatives, Acv_funs_1)),

    %%%
    %%% Описывам перегруженные функции как proplist.
    %%% Вообще можно применить и обычный fun(X, Y) -> end.
    %%%
    Acv_funs_2 = [
        {creative, fun acv_block_creative:fast_creative/2}
    ],

    %%%
    %%% Получаем обратно binary c xml
    %%%
    Acv_xml_2 = xml_utils:simple(block_creative:block(Acv_creatives, Acv_funs_2)),

    %%%
    %%% Описывам креативы как список proplist.
    %%% В даном случае блок с однми креативом
    %%%     от корого ничего не зависит.
    %%%
    Ecv_creatives = [[]],

    %%%
    %%% Описывам перегруженные функции как proplist.
    %%% Вообще можно применить и обычный fun(X, Y) -> end.
    %%%
    Ext_funs = [
        {creative, fun ext_block_creative:creative/2}
    ],

    %%%
    %%% Получаем обратно binary c xml
    %%%
    Ext_xml_1 = xml_utils:simple(block_creative:block(Ecv_creatives, Ext_funs)),
    print(Acv_xml_1, Acv_xml_2, Ext_xml_1),
    ok.

print(Acv_xml_1, Acv_xml_2, Ext_xml_1) ->
    io:format("~n"),

    io:format("Acv_creative slow~n"),
    io:format("-----------------------------------------~n"),
    io:format("~p~n", [Acv_xml_1]),
    io:format("-----------------------------------------~n"),

    io:format("Acv_creative fast~n"),
    io:format("-----------------------------------------~n"),
    io:format("~p~n", [Acv_xml_2]),
    io:format("-----------------------------------------~n"),

    io:format("Ext_creative~n"),
    io:format("-----------------------------------------~n"),
    io:format("~p~n", [Ext_xml_1]),
    io:format("-----------------------------------------~n"),

    ok.