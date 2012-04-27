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



make_error_warn(E) ->
    make_error_json(<<"WARN">>, E).

make_error_json(E) ->
    make_error_json(<<"Error">>, E).

make_error_json(Type, {unexpected, E}) ->
    ?I("Unexpected error: ~p~n", [E]),
    {struct, [{Type,
        {struct,
            [{<<"type">>, <<"unexpected">>},
            {<<"info">>, <<"unexpected">>}]}}]};

make_error_json(Type, {Etype, Einfo})
        when erlang:is_list(Etype) and erlang:is_list(Einfo) ->
    io:format("Queue error: ~p~n", [Einfo]),
    {struct,
        [{Type,
            {struct, [{<<"type">>, erlang:list_to_binary(Etype)},
            {<<"info">>, erlang:list_to_binary(Einfo)}]}}]};

make_error_json(Type, {Etype, Einfo})
        when erlang:is_binary(Etype) and erlang:is_list(Einfo) ->
    io:format("Queue error: ~p~n", [Einfo]),
    {struct,
        [{Type,
            {struct, [{<<"type">>, Etype},
            {<<"info">>, erlang:list_to_binary(Einfo)}]}}]};

make_error_json(Type, {Etype, Einfo})
        when erlang:is_atom(Etype) and erlang:is_list(Einfo) ->
    io:format("Queue error: ~p~n", [Einfo]),
    {struct,
        [{Type,
            {struct, [{<<"type">>, erlang:list_to_binary(erlang:atom_to_list(Etype))},
            {<<"info">>, erlang:list_to_binary(Einfo)}]}}]}.

%%% ------------------------------------------------------------------------

% dao_value(Module, Function, Param) ->
%     case Module:Function(Param) of
%         {ok, Vals} ->
%             Pre_res = db2json:encode(Vals),
%             Res = mochijson2:encode({struct, [{<<"value">>, Pre_res }]});
%         _ ->
%             Res = dao_call(Module, Function, Param, value)
%     end,
%     Res.

%%%
%%% Возвращает единственный вариант
%%%
dao_value(Module, Function, Param) ->
    Value = dao:dao_call(Module, Function, Param, undefined),
    mochijson2:encode({struct, [{<<"value">>, Value}]}).

%%%
%%% Возвращает несколько результатов
%%%
dao_values(Module, Function, Param) ->
    mochijson2:encode(
        dao:dao_call(Module, Function, Param, values)
    ).

%%% ------------------------------------------------------------------------

dao_call(Module, Function, Param) ->
   dao_call_worker(Module:Function(Param), undefined, undefined).

dao_call(Module, Function, undefined, Json_oname) ->
    dao_call_worker(Module:Function(), Json_oname, undefined);

dao_call(Module, Function, Param, Json_oname) ->
    dao_call_worker(Module:Function(Param), Json_oname, undefined).

dao_call(Module, Function, undefined, Json_oname, Callback) ->
    dao_call_worker(Module:Function(), Json_oname, Callback);

dao_call(Module, Function, Param, Json_oname, Callback) ->
    dao_call_worker(Module:Function(Param), Json_oname, Callback).

%%% ------------------------------------------------------------------------

dao_call_worker(Function_result, Json_oname) ->
    dao_call_worker(Function_result, Json_oname, undefined).

%%%
%%% Функция с пустым Callback 
%%%
dao_call_worker(Function_result, Json_oname, undefined) ->
    dao_call_worker(Function_result, Json_oname, fun(X) -> ok end);

%%%
%%% Функция с Callback
%%%
dao_call_worker(Function_result, Json_oname, Callback)  when erlang:is_function(Callback, 1) ->
    case
        case Function_result of
            {ok, Vals} ->
                {ok, db2json:encode(Vals, Json_oname)};
            ok ->
                {ok, {struct, [{<<"result">>, ok}]}};
            {retVal, ok} ->
                {ok, {struct, [{<<"result">>, ok}]}};
            {error, E} ->
                {error, make_error_json(E)};
            {warn, E} ->
                {warn, make_error_warn(E)}
        end
    of
        {ok, Encoded} ->
            Callback(Function_result),
            Encoded;
        {_, Encoded} ->
            Encoded
    end.

%%% ------------------------------------------------------------------------




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

to_type(V, numeric) ->
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
            fun(Con) -> equery(Con, Query)
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
            fun(Con) -> equery(Con, Query, Params)
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
            fun(Con) -> equery(Con, Query, Params)
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

%%% @doc
%%%     Обертка для стандвартной функции pgsql
%%%     Выполныет заданный запрос
%%%     Если параметры функции являются proplist
%%%     то происходит их распарсивание и подстановка согласно
%%%     с буквенными именами в теле запроса
%%%     Сделано это для того, чтобы в случае, если мы будем менять,
%%%     наши запросы, не приходилось высчитывать порядок следования.
%%%     Последнее очень не удобно для запросов с более чем 5 параметров.
%%%

equery(Con, Query, Params) ->
    case utils:is_proplist(Params) of
        true ->
            {NewQuery, Values} = equery_pl(Query, Params),
            pgsql:equery(Con, NewQuery, Values);
        _ ->
            pgsql:equery(Con, Query, Params)
    end.

equery(Con, Query) ->
    pgsql:equery(Con, Query).

%%% @doc
%%%     Функция преобразования запроса
%%%     с буквенными именами к числовым.
%%%     Запрос:
%%%         dao:equery_pl(
%%%            "select id from customer where id = $id and uid = $uid",
%%%            [{"id", 10}, {"uid", 15}]
%%%         ).
%%%    Вернет:
%%%         "select id from customer where id = $1 and uid = $2"
%%%
equery_pl(Query, Proplist) ->
    equery_pl(Query, Proplist, 0, []).

%%% @doc
%%%     Функция преобразования запроса, основная рабочая часть.
%%%     Параметры разбиваем на {k, v}
%%%         Преобразованное имя ключа $k заменится
%%%             на его порядковый номер в списке.
%%%         Значение (v) отправится в список значений,
%%%             который далее будет использован для стандартного запроса
%%%             тут важно соблюсти порядок следования значений
%%%                 (Values ++[Value]) a не [Value|Values] !!!
%%%     Чтобы не перекомпилевать одинаковые ключи
%%%         для поиска по регекспу они храняться в ets
%%%             dao_regexp создается в assist_srv
%%%
equery_pl(Query, [], _, Values) -> {Query, Values};

equery_pl(Query, [{Name, Value}|Rest], Cnt, Values) ->
    Newcnt = Cnt + 1,
    Re = "[$]" ++ convert:to_list(Name),
    case ets:lookup(dao_regexp, Re) of
        [{_, {Re, Cre}}]  ->
            true;
        _ ->
            {ok, Cre} = re:compile(Re),
            true = ets:insert(dao_regexp, {Re, Cre}),
            false
    end,
    Newquery =
        re:replace(
            Query,
            Cre,
            "$" ++ convert:to_list(Newcnt),
            [{return,list}]
        ),
    equery_pl(Newquery, Rest, Newcnt, lists:append(Values, [Value]));

equery_pl(Query, List, _, _) when erlang:is_list(List) ->
    {Query, List}.







