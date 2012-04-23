-module(db2json).
-export([
    encode/1,
    encode/2
]).

-include("common.hrl").


%encode_list([R| Rest]) ->


encode([]) ->
    {struct, name_fields([])};

encode([R]) ->
    {struct, name_fields(R)}.

encode(Arg, undefined) ->
    encode(Arg);

encode(Rows, Json_oname) when is_list(Rows)->
    Res = [{struct, name_fields(R)} || R <- Rows],
    mount_json(Res, Json_oname);

encode([R], Json_oname) ->
    mount_json({struct, name_fields(R)}, Json_oname);

encode(Row, Json_oname) ->
    mount_json(to_binary(Row), Json_oname).


mount_json([_H|_T]=Res, Json_oname) ->
    {struct, [{Json_oname, Res}]};

mount_json([], Json_oname) ->
    {struct, [{Json_oname, <<"">>}]};

mount_json(Res, Json_oname) ->
    {struct, [{Json_oname, Res}]}.

name_fields([{Name, Val}|T]) ->
    [{Name, encode_value(Val)}|name_fields(T)];

name_fields([])->
    [].

encode_value(V) when is_tuple(V) ->
    R = string:join([[encode_value(Vi)] || Vi <- tuple_to_list(V)], " "),
    to_binary(R);
encode_value(V) ->
    to_binary(V).

to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V), V =:= null ->
    to_binary("");
to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
to_binary(V) when is_float(V) ->
    list_to_binary(float_to_list(V));
to_binary(V) when is_binary(V) ->
    V.
%    list_to_binary(lists:flatten([erlang:integer_to_list(Val, 16) || Val <- binary_to_list(V)])).
