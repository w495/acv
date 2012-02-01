-module(captcha).

-export([new/0, check/2, removeExpired/0]).

%%% --------------------------------------------------------------------------
%%% API
%%% --------------------------------------------------------------------------
new() ->
    FileName = lists:flatmap(fun(Item) -> integer_to_list(Item) end, tuple_to_list(now())),

    Code = generate_rand(5),

    File = io_lib:format("/tmp/~s.png",[FileName]),

    Cmd = io_lib:format("convert -background 'none' -fill '#222222' -size 175 -gravity Center -wave 5x100 -swirl 50 -font DejaVu-Serif-Book -pointsize 28 label:~s -draw 'Bezier 10,40 50,35 100,35 150,35 200,50 250,35 300,35' ~s", [Code, File]),
    os:cmd(Cmd),

    CodeUp = string:to_upper(Code),
    MD5 = erlang:md5(CodeUp),

    Now = gs_us(),
    true = ets:insert(captcha, {MD5, {CodeUp, Now}}),
    true = ets:insert(captcha_time, {Now, MD5}),

%    io:format("Captcha: ~p~n", [Code]),

    {ok, BinPng} = file:read_file(File),
    file:delete(File),

    CodeHex = mochihex:to_hex(MD5),
    {CodeHex, BinPng}.

check(CodeHex, Code) when CodeHex =:= undefined; Code =:= undefined ->
    false;
check(CodeHex, Code) ->
    UCode = string:to_upper(Code),
    MD5 = mochihex:to_bin(CodeHex),
    case ets:lookup(captcha, MD5) of
        [{_, {Val, Now}}] when Val == UCode ->
            ets:delete(captcha, MD5),
            ets:delete(captcha_time, Now),
            true;
        _ -> false
    end.

removeExpired() ->
    {Gs,_} = gs_us(),
    DeadTime =
      Gs - round(config:get(web_session_expire_timeout, 180000)/1000),
    Match = {{'$1','_'},'_'},
    Guard = [{'<','$1',{const,DeadTime}}],
    L = ets:select(captcha_time, [{Match, Guard, ['$_']}]),
    DelFun = fun({IdTime,IdCaptcha}) ->
                   ets:delete(captcha, IdCaptcha),
                   ets:delete(captcha_time, IdTime)
    end,
    lists:foreach(DelFun, L).

%%% --------------------------------------------------------------------------
%%% Local
%%% --------------------------------------------------------------------------
generate_rand(PassLen) ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    L = "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz23456789",
    [lists:nth(X,L) || X <- lists:map(fun(_)->random:uniform(length(L)) end, lists:seq(1, PassLen))].


% {gregorian_seconds, microseconds}
% используется как уникальный ключ в captcha_time
% элемент gregorian_seconds используется для определения устаревших captcha
gs_us() ->
    {_,_,Mi} = Now = now(),
    DT = calendar:now_to_datetime(Now),
    {calendar:datetime_to_gregorian_seconds(DT), Mi}.

