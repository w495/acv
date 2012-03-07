-module(mysql_dao).

-include("common.hrl").

-compile(export_all).


to_type(undefined, _Type) ->
    null;
to_type(V, 'VAR_STRING') ->
    binary_to_list(V);
to_type({date, V}, 'DATE') ->
    V;
to_type({datetime, V}, 'TIMESTAMP') ->
    V;
to_type(V, _Type) ->
    V.

name_columns([{_Table, Name, _P3, Type}|Ct], [V|Vt], Ret) ->
    name_columns(Ct, Vt, [{binary_to_list(Name), to_type(V, Type)}|Ret]);
name_columns([], [], Ret) ->
    Ret;
name_columns([], V, Ret) ->
    ?DEBUG(?FMT("unexpected values: ~p~n", [V])),
    Ret;
name_columns(C, [], Ret) ->
    ?DEBUG(?FMT("unexpected columns: ~p~n", [C])),
    Ret.

make_proplist(Columns, List) ->
    make_proplist(Columns, List, []).

make_proplist(Columns, [V|T], Ret) ->
    make_proplist(Columns, T, [name_columns(Columns, V, [])|Ret]);

make_proplist(_C, [], Ret) ->
    Ret.

dao_call(Module, Function, Param, JsonRetName) ->
    case Module:Function(Param) of
        {ok, Vals} -> db2json:encode(Vals, JsonRetName);
        _ -> []
    end.


msret(Vals) ->
    {ok, Vals}.


simple(Query) ->
    mysql:prepare(mySqlConPool, simple_2, Query),
    {data,{mysql_result, Cols, Vals, _, _}} =
        mysql:execute(mySqlConPool, simple_2, []),
    msret(mysql_dao:make_proplist(Cols, Vals, [])).

simple(Query, Params) ->
    mysql:prepare(mySqlConPool, simple_2, Query),
    {data,{mysql_result, Cols, Vals, _, _}} =
        mysql:execute(mySqlConPool, simple_2,
            [convert:to_binary(P) || P <- Params]),
    msret(mysql_dao:make_proplist(Cols, Vals, [])).


test()-> ok.

test(speed)-> ok.
