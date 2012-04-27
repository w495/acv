%%% 
%%% @file block_creative.hrl
%%%
%%% Элемент xml предасталяет из себя
%%% 
%%%     В данном случае мы используем библиотеку xmerl.
%%%     Структуру самомго xml можно описать структурами erlang.
%%%         Работаем с тройками
%%%             {
%%%                 %'имя тега',
%%%                     tag_name
%%%                 %'proplist c аргументами',
%%%                     [{name, value}| ... ]
%%%                 %'list c вложенными элементами'
%%%                     [{t,a,c}| ...]
%%%             }
%%%         Или с со структурами вида
%%%             #tag_name{
%%%                 attributes = [{name, value}| ... ]
%%%                 children = [{t,a,c}| ...]
%%%             }
%%%
%%%     Формальное определение элемента xml:
%%%
%%%             xmlel() ->
%%%                     {
%%%                         tag_name()
%%%                         [attribute()],
%%%                         [xmlel()]
%%%                     }
%%%                 |   #tag_name {
%%%                         attributes = [attribute()],
%%%                         children = [xmlel()]
%%%                     }
%%%                 | iolist()
%%%
%%%             tag_name()  ->
%%%                 name()
%%%
%%%             attribute() ->
%%%                 {name(), value()}
%%%
%%%             name() ->
%%%                 atom() | string() | binary()
%%%
%%%             value() ->
%%%                 atom() | string() | number() | binary()

%%%

-record(block,
    {
        attributes  = [], % proplist()
        children    = []  % [xmlel]
    }
).

-record(creative,
    {
        attributes  = [], % proplist()
        children    = []  % [xmlel]
    }
).
