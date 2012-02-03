-module(dao).

-include("common.hrl").

-include_lib("epgsql/include/pgsql.hrl").

-compile(export_all).


%%%
%%% Спецификации
%%% 



-spec simple(Query::string())
    -> {State::atom(), list(tuple())}.   %%% {ok, []}

-spec simple(Query::string(), Params::list())
    -> {State::atom(), list(tuple())}.   %%% {ok, []}

-spec simple_ret(Query::string(), Params::list()) -> tuple().


make_error_json({unexpected, E}) ->
    ?DEBUG_INFO("Unexpected error: ~p~n", [E]),
    {struct, [{<<"ERROR">>,
        {struct,
            [{<<"type">>, <<"unexpected">>},
            {<<"info">>, <<"unexpected">>}]}}]};
make_error_json({Etype, Einfo}) ->
    io:format("Queue error: ~p~n", [Einfo]),
    {struct,
        [{<<"ERROR">>,
            {struct, [{<<"type">>, Etype},
            {<<"info">>, list_to_binary(Einfo)}]}}]}.

dao_call(Module, Function, undefined, JsonRetName) ->
    case Module:Function() of
        {ok, Vals} -> Res = db2json:encode(Vals, JsonRetName);
        ok -> Res = {struct, [{<<"result">>, ok}]};
        {retVal, ok} -> Res = {struct, [{<<"result">>, ok}]};
        {error, E} -> Res = make_error_json(E)
    end,
    Res;

dao_call(Module, Function, Param, JsonRetName) ->
    case Module:Function(Param) of
        {ok, Vals} -> Res = db2json:encode(Vals, JsonRetName);
        ok -> Res = {struct, [{<<"result">>, ok}]};
        {retVal, ok} -> Res = {struct, [{<<"result">>, ok}]};
        {error, E} -> Res = make_error_json(E)
    end,
    Res.

%%%
%%% Возвращает единственный вариант
%%%
dao_value(Module, Function, Param) ->
    case Module:Function(Param) of
        {ok, Vals} ->
            Pre_res = db2json:encode(Vals),
            Res = mochijson2:encode({struct, [{<<"value">>, Pre_res }]});
        _ ->
            Res = dao_call(Module, Function, Param, value)
    end,
    Res.

dao_call(Module, Function, Param) ->
    case Module:Function(Param) of
        {ok, Vals} -> Res = db2json:encode(Vals);
        ok -> Res = {struct, [{<<"result">>, ok}]};
        {retVal, ok} -> Res = {struct, [{<<"result">>, ok}]};
        {error, E} -> io:format("$~p~n", [E]), Res = make_error_json(E)
    end,
    Res.

pg2rs({ok, _, Vals}, Record_name) ->
    [list_to_tuple([Record_name | tuple_to_list(X)]) || X <- Vals];

pg2rs(Vals, Record_name) ->
    [list_to_tuple([Record_name | tuple_to_list(X)]) || X <- Vals].

strip_rs(Vals) when is_list(Vals)->
    strip_rs(Vals, []);

strip_rs(Val) when is_tuple(Val) ->
    [_|T]=tuple_to_list(Val),
    T.

strip_rs([Vh|Vt], Ret) ->
    [_|T]=tuple_to_list(Vh),
    strip_rs(Vt, [list_to_tuple(T)|Ret]);
strip_rs([], Ret) ->
    Ret.

collect_where_params(Params) ->
    collect_where_params([], [], Params).

collect_where_params(Where, Params, [{Val}|T]) when is_list(Val) ->
    collect_where_params([Val|Where], Params, T);
collect_where_params(Where, Params, [{Key, Val}|T]) ->
    collect_where_params(Where, Params, [{Key, "=", Val}|T]);
%    collect_where_params([lists:append([Key, " = $", utils:to_list(length(Params) + 1)])|Where], [Val | Params], T);
collect_where_params(Where, Params, [{Key, Action, Val}|T]) ->
    collect_where_params([lists:append([Key, " ", Action, " $", utils:to_list(length(Params) + 1)])|Where], [Val | Params], T);
collect_where_params(Where, Params, []) when length(Where) > 0->
    {?FMT(" WHERE ~s", [string:join(lists:reverse(Where), " and ")]), lists:reverse(Params)};
collect_where_params(_, _, []) ->
    {";", []}.


to_type(null, _Type) ->
    null;
to_type(V, int4) ->
    utils:to_int(V);
to_type(V, varchar) ->
    binary_to_list(V);
to_type(V, text) ->
    binary_to_list(V);
to_type(V, Type) ->
    V.


name_columns([{column, Name, Type, _P3, _P4, _P5}|Ct], [V|Vt], Ret) ->
    name_columns(Ct, Vt, [{binary_to_list(Name), to_type(V, Type)}|Ret]);
name_columns([], [], Ret) ->
    Ret;
name_columns([], V, Ret) ->
    ?DEBUG(?FMT("unexpected values: ~p~n", [V])),
    Ret;
name_columns(C, [], Ret) ->
    ?DEBUG(?FMT("unexpected columns: ~p~n", [C])),
    Ret.

make_proplist(Columns, [V|T], Ret) ->
    make_proplist(Columns, T, [name_columns(Columns, tuple_to_list(V), [])|Ret]);
make_proplist(_C, [], Ret) ->
    Ret.

pgret({return, Value}) ->
    {ok, Value};

%%%
%%% select sq & eq
%%%
pgret({ok, Columns, Vals}) ->
    {ok, make_proplist(Columns, Vals, [])};

%%%
%%% update sq & eq
%%%
pgret({ok, _Count}) ->
    ok;
%%%
%%% insert sq & eq
%%%
pgret({ok, _Count, _Columns, _Rows}) ->
    ok;

%%%
%%% ошибка сиквела - неожиданный возврат в функции дает ошибку ожидаемого возврата.
%%%
pgret({pgcp_error, E}) ->
    pgreterr(E);

pgret({error, E}) ->
    pgreterr(E);

pgret(V) ->
    {retVal, V}.

pgreterr({error, E}) ->
    pgreterr(E);
pgreterr({badmatch, E}) ->
    pgreterr(E);
pgreterr(#error{code=Error_code_bin, message=Msg}) ->
    case Error_code_bin of
        <<"23502">> ->
            try
                {ok, RE} = re:compile("\"(.+)\""),
                {match, [_, C | _]} = re:run(Msg, RE, [{capture, all, list}]),
                {error, {not_null, C}}
            catch
                E:R ->
                    io:format("pgret ERROR: ~p - ~p~n", [E, R]),
                    {error, {unknown, Msg}}
            end;
        <<"23505">> ->
            try
                {ok, RE} = re:compile("\".*_([^_]?.+)_key\""),
                {match, [_, C | _]} = re:run(Msg, RE, [{capture, all, list}]),
                {error, {not_unique, C}}
            catch
                E:R ->
                    io:format("pgret ERROR: ~p - ~p~n", [E, R]),
                    {error, {unknown, binary_to_list(Msg)}}
            end;
        _ ->
            {error, {unknown, binary_to_list(Msg)}}
    end;
pgreterr(E) ->
    {error, {unexpected, E}}.


%%
%% 
%%
with_connection_fk(Function) ->
    pgConPool:withConnection(Function, config:get(fk_db_name, "fk")).

%%
%%
%%
with_transaction_fk(Function) ->
    pgConPool:withTransaction(Function, config:get(fk_db_name, "fk")).

%%% @doc
%%% Выполняет простой запрос и возвращает его результат 
%%%
simple(Query) ->
%    ?DEBUG_INFO("~p:simple/1~nQuery:  ~s~n", [?MODULE, Query]),
    Result = dao:pgret(dao:with_connection_fk(
            fun(Con) -> pgsql:equery(Con, Query)
        end)),
%    ?DEBUG_INFO("~p:simple/1~n-> ~p~n", [?MODULE, Result]),
    Result.

%%% @doc
%%% Выполняет простой запрос и возвращает его результат
%%%
simple(Query, Params) ->
%    ?DEBUG_INFO("~p:simple/2~nQuery: ~s~nParams: ~p~n",
%        [?MODULE, Query, Params]),
    Result = dao:pgret(dao:with_connection_fk(
            fun(Con) -> pgsql:equery(Con, Query, Params)
        end)),
%    ?DEBUG_INFO("~p:simple/2~n-> ~p~n",
%        [?MODULE, Result]),
    Result.

%%% @doc
%%% Выполняет простой запрос и возвращает результат побочного действия
%%%
simple_ret(Query, Params) ->
%    ?DEBUG_INFO("~p:simple_ret/2~nQuery: ~s~nParams: ~p~n",
%        [?MODULE, Query, Params]),
    Pre_result = dao:with_connection_fk(
            fun(Con) -> pgsql:equery(Con, Query, Params)
        end),
    case dao:pgret(Pre_result) of
        ok ->
            Result = Pre_result,
%            ?DEBUG_INFO("~p:simple_ret -> ~p~n",
%                [?MODULE, Result]),
            ok;
        Error -> Result = {error, Error}
    end,
    Result.
