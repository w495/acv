%%% @file dao_sysvar.erl
%%%
%%%    Работа с системными переменными.
%%%

-module(dao_sysvar).

-export([
    acv_video_loadnext/0
]).

-export([
    get_sysvars/1,
    get/2,
    get_value/1,
    get_sysvar/1,
    update_sysvar/1,
    test/0,
    test/1
]).

-include("common.hrl").

%%% -------------------------------------------------------------------------
%%% ФУНКЦИИ ВОЗВРАТА КОНКРЕТНЫХ ПЕРЕМЕННЫХ,
%%%
%%%     Возможно надо вынести в biz
%%% -------------------------------------------------------------------------

acv_video_loadnext() ->
    dao_sysvar:get(acv_video_loadnext, ?DEFAULT_ACV_VIDEO_LOADNEXT).

%%% -------------------------------------------------------------------------
%%% API ФУНКЦИИ
%%% -------------------------------------------------------------------------

%%% @doc
%%%     Возвращает список всех переменных удовлетворяющим разрешениям Perm.
%%%
get_sysvars({perm, Perm}) ->
    Query =
        " select "
            " sysvar.id, "
            " sysvar_type.name as type, "
            " sysvar.name, "
            " sysvar.value, "
            " sysvar.description "
        " from "
            " sysvar "
        " join "
            " sysvar_type "
        " on "
            " sysvar_type.id = sysvar.type_id "
        " join "
            " permission "
        " on "
            " permission.id = sysvar.perm_id "
            " and permission.name = $1"
        " ; ",
    dao:simple(Query, [convert:to_list(Perm)]);

%%% @doc
%%%     Возвращает список всех переменных
%%%
get_sysvars(_) ->
    Query =
        " select "
            " sysvar.id, "
            " sysvar_type.name as type, "
            " permission.name  as permission, "
            " sysvar.name, "
            " sysvar.value, "
            " sysvar.description "
        " from "
            " sysvar "
        " join "
            " sysvar_type "
        " on "
            " sysvar_type.id = sysvar.type_id "
        " join "
            " permission "
        " on "
            " permission.id = sysvar.perm_id "
        " ; ",
    dao:simple(Query).

%%% @doc
%%%     Возвращает значение пременной.
%%%     Переменная берется по Uniq --- это имя или id
%%%     Если переменной с таким именем нет,
%%%     возвращается ошибка.
%%%
get_value(Uniq) ->
    case get_sysvar(Uniq) of
        {ok, [Var]} ->
            Value = proplists:get_value("value", Var),
            Type = proplists:get_value("type", Var),
            {ok, to_type(Type, Value)};
        {ok, []} ->
            {error, 'no such sysvar'};
        Error ->
            Error
    end.

%%% @doc
%%%     Возвращает значение пременной.
%%%     Переменная берется по Uniq --- это имя или id
%%%     Если переменной с таким именем нет,
%%%     возвращается значение Default.
%%%
get(Uniq, Default) ->
    case get_value(Uniq) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

%%% @doc
%%%     Возвращает пременную.
%%%
get_sysvar(Name) when erlang:is_atom(Name) ->
    get_sysvar(convert:to_list(Name));
get_sysvar(Id) when erlang:is_integer(Id) ->
    Query =
        " select "
            " sysvar.id, "
            " sysvar_type.name as type, "
            " permission.name  as permission, "
            " sysvar.name, "
            " sysvar.value, "
            " sysvar.description "
        " from "
            " sysvar "
        " join "
            " sysvar_type "
        " on "
            " sysvar_type.id = sysvar.type_id "
        " join "
            " permission "
        " on "
            " permission.id = sysvar.perm_id "
        " where "
            " sysvar.id = $1; ",
    dao:simple(Query, [Id]);
get_sysvar(Name) when erlang:is_list(Name) ->
    Query =
        " select "
            " sysvar.id, "
            " sysvar_type.name as type, "
            " permission.name  as permission, "
            " sysvar.name, "
            " sysvar.value, "
            " sysvar.description "
        " from "
            " sysvar "
        " join "
            " sysvar_type "
        " on "
            " sysvar_type.id = sysvar.type_id "
        " join "
            " permission "
        " on "
            " permission.id = sysvar.perm_id "
        " where "
            " sysvar.name = $1; ",
    dao:simple(Query, [Name]).

%%% @doc
%%%     Обновляет значение пременной.
%%%     Переменная берется по Uniq --- это имя или id
%%%     Если тип значение не совпадает с типом переменной,
%%%     возвращается ошибка.
%%%
update_sysvar({Uniq, Value}) ->
    case check_type({Uniq, Value})  of
        true ->
            update_sysvar_unsafe({Uniq, Value});
        false ->
            {error, 'wrong value type'}
    end.

%%% -------------------------------------------------------------------------
%%% ВНУТРЕННИЕ ФУНКЦИИ
%%% 
%%%     Для переменных введены проверки и приведения типов.
%%%     Это сделано в целях безопасности работы, и на случай,
%%%     чтобы при настройке системы, случайно не были введены
%%%     недопустимые значения.
%%%
%%%         В случае проблем с производительностью,
%%%         лучше просто использовать
%%%
%%%             % to_type(_, _) -> true.
%%%             % check_type({_, _}) -> true.
%%%
%%%         Но в этом случае провекрка правильности значения
%%%         Должна осуществляться на более высоком уровне.
%%%
%%% -------------------------------------------------------------------------

%%% @doc
%%%     Приводит значение переменной к указанному типу.
%%%
to_type("integer", Value) ->
    convert:to_integer(Value);
to_type("float", Value) ->
    convert:to_float(Value);
to_type("string", Value)  ->
    Value;
to_type(Type, Value) when erlang:is_list(Type) ->
    Value.

%%% @doc
%%%     Проверяет соответвует ли новое значение типу переменной 
%%%     переменная берется по Uniq --- это имя или id
%%%
%%%         В случае проблем с производительностью,
%%%         лучше просто использовать
%%%
%%%             % check_type({_, _}) -> true.
%%%
check_type({Uniq, Value}) ->
    case get_sysvar(Uniq) of
        {ok, [Var]} ->
            check_type(proplists:get_value("type", Var), Value);
        _ ->
            false
    end.

%%% @doc
%%%     Проверяет соответвует ли новое значение типу переменной Var
%%%
check_type("string", Value) when erlang:is_atom(Value) ->
    true;
check_type("string", Value) when erlang:is_list(Value)  ->
    true;
check_type("integer", Value) when erlang:is_integer(Value) ->
    true;
check_type("float", Value) when erlang:is_float(Value) ->
    true;
check_type(Type, _value) when erlang:is_list(Type) ->
    false.

%%% @doc
%%%     Обновляет функцию без учета типа значения
%%%
update_sysvar_unsafe({Name, Value})
        when erlang:is_atom(Name) ->
    update_sysvar_unsafe({convert:to_list(Name), Value});
update_sysvar_unsafe({Id, Value})
        when erlang:is_integer(Id) ->
    Query =
        " update "
            " sysvar "
        " set "
            " value = $2 "
        " where "
            " id = $1;",
    dao:simple(Query, [Id, convert:to_list(Value)]);
update_sysvar_unsafe({Name, Value})
        when erlang:is_list(Name) ->
    Query =
        " update "
            " sysvar "
        " set "
            " value = $2 "
        " where "
            " name = $1;",
    dao:simple(Query, [Name, convert:to_list(Value)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Юнит тестирование
%%%

test()->
    ok.


%%%
%%% Нагрузочное тестирование
%%%
test(speed)->
    Times_1 = 1000000,
    tests:print_speed("test_matching_1",
        fun() ->
            test_matching_1("string"),
            test_matching_1("integer"),
            test_matching_1("float"),
            test_matching_1("void")
        end, Times_1 ),
    tests:print_speed("test_guard_1",
        fun() ->
            test_guard_1("string"),
            test_guard_1("integer"),
            test_guard_1("float"),
            test_guard_1("void")
        end, Times_1 ),
    tests:print_speed("test_guard_2",
        fun() ->
            test_guard_2("string"),
            test_guard_2("integer"),
            test_guard_2("float"),
            test_guard_2("void")
        end, Times_1 ),
    tests:print_speed("test_case_1",
        fun() ->
            test_case_1("string"),
            test_case_1("integer"),
            test_case_1("float"),
            test_case_1("void")
        end, Times_1 ),
    ok.

test_matching_1("string") -> true;
test_matching_1("integer") -> true;
test_matching_1("float") -> true;
test_matching_1(_) -> false.

test_guard_1(Type)  when Type =:= "string" -> true;
test_guard_1(Type)  when Type =:= "integer" -> true;
test_guard_1(Type)  when Type =:= "float" -> true;
test_guard_1(_) ->  false.

test_guard_2(Type) when  erlang:is_list(Type) -> true;
test_guard_2(Type) -> false.

test_case_1(Type) ->
    case Type of
        "string" -> true;
        "integer" -> true;
        "float" -> true;
        _ -> false
    end.
