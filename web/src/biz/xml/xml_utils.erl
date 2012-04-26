%%% 
%%% @file xml_utils.erl
%%% 
%%%     Утилиты генерации xml
%%%     В своей реализации используют стандартную библиотеку xmerl
%%%     Мы не используем структры xmerl умышленно,
%%%     так как без них получается удобнее
%%%
%%%     Работаем с тройками
%%%
%%%         {
%%%             %'имя тега',
%%%                 tag_name
%%%             %'proplist c аргументами',
%%%                 [{name, value}| ... ]
%%%             %'list c вложенными элементами'
%%%                 [{t,a,c}| ...]
%%%         }
%%%
%%%     Или с со структурами вида
%%%
%%%         #tag_name{
%%%             attributes = [{name, value}| ... ]
%%%             children = [{t,a,c}| ...]
%%%         }
%%%

-module(xml_utils).

-export([
    simple/1,
    compute_args_sum/2,
    compute_args_sum/3,
    test/0,
    test/1
]).

-define(XML_PROLOG, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").

%%% @doc
%%%     Генерирует XML на основании кортежа вида
%%%     {
%%%         %'имя тега',
%%%             tag_name
%%%         %'proplist c аргументами',
%%%             [{name, value}| ... ]
%%%         %'list c вложенными элементами'
%%%             [{t,a,c}| ...]
%%%     }
%%%
simple(Initial_block) ->
    Xml = xmerl:export_simple([Initial_block], xmerl_xml,
                              [{prolog, ?XML_PROLOG}]),
    unicode:characters_to_binary(Xml).

%%% @doc
%%%     Вычислет сумму аргументов Name
%%%         для объектов заланного формата.
%%% 
compute_args_sum(Name, Objects) ->
    compute_args_sum(Name, Objects, 0).
compute_args_sum(Name, Objects, Init) ->
    lists:foldl(
        fun (Object, C) ->
            get_value(Name, Object) + C
        end,
        Init,
        Objects
    ).

get_value(Name, {_, Args, _} = _object) ->
    proplists:get_value(Name, Args, 0);
get_value(_name, _object) ->
    0.




test() ->
    ok.

test(speed) ->
    ok.
