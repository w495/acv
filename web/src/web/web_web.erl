-module(web_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

-include_lib("xmerl/include/xmerl.hrl").

-include("common.hrl").
-include("web.hrl").

-compile(export_all).

%% External API

start(Initial_options) ->
    {Doc_root, Options} = get_option(docroot, Initial_options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, Doc_root)
           end,
    %%%
    %%%  mochiweb стартует тут
    %%%
    mochiweb_http:start([{loop, Loop} | Options]).

stop() ->
    ?D("~p: ~p got stop signal~n", [erlang:localtime(), ?MODULE]),
    supervisor:terminate_child(web_sup, ?MODULE),
    supervisor:delete_child(web_sup, ?MODULE).

serve_static_inner(P, T, Req, ExtraHeaders) ->
    try
        {Tmp, <<".js">>} = split_binary(list_to_binary(T), length(T) - 3),
        Fname = binary_to_list(Tmp) ++ ".gz.js",
        true = lists:member("gzip", string:tokens(Req:get_header_value("Accept-Encoding"), ", ")),
        {ok, _} = file:read_file_info(P ++ "/" ++ Fname),
        Req:serve_file(Fname, P, [{"content-encoding", "gzip"} | ExtraHeaders])
    catch
        _:_ ->
            Req:serve_file(T, P, ExtraHeaders)
    end.

serve_static(P, T, Req) ->
    serve_static_inner(P, T, Req, []).

serve_static(P, T, Req, ExtraHeaders) ->
    serve_static_inner(P, T, Req, ExtraHeaders).

start_controller(Module, Action, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) ~p:~p ~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Module, Action])),
    Exports = Module:module_info(exports),
    % % CALL BEFORE
    _NReq = bacRunner(Exports, call_before, Module, Req),
    % % CALL CONTROLLER
    Result = Module:Action(Req),
    % % CALL AFTER
    {_, NResult} = bacRunner(Exports, call_after, Module, {Req, Result}),
    Req:ok(NResult).

start_controller(Module, Action, Req, Param) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) ~p:~p ~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Module, Action])),
    Exports = Module:module_info(exports),
    % % CALL BEFORE
    _NReq = bacRunner(Exports, call_before, Module, Req),
    % % CALL CONTROLLER
    Result = Module:Action(Req, Param),
    % % CALL AFTER
    {_, NResult} = bacRunner(Exports, call_after, Module, {Req, Result}),
    Req:ok(NResult).


bacRunner([{M, _}|T], Method, Module, Param) ->
    case M =:= Method of
        true -> Module:Method(Param);
        false -> bacRunner(T, Method, Module, Param)
    end;
bacRunner([], _Method, _Module, Param) ->
    Param.


save_start_controller(Module, Action, Req) ->
    try
        start_controller(Module, Action, Req)
    catch throw:Val ->
        processControllerException(throw, Val, Req)
    end.

save_start_controller(Module, Action, Req, T) ->
    try
        start_controller(Module, Action, Req, T)
    catch throw:Val ->
        processControllerException(throw, Val, Req)
    end.

%% ========================================================================
%% ПРОБЛЕМЫ И ОШИБКИ
%% ========================================================================

processControllerException(throw, nothing_to_be_done, _Req) ->
    [];
    
processControllerException(throw, {serve_static, Filename, Path, ExtraHeaders}, Req)->
    serve_static(Path, Filename, Req, ExtraHeaders);
    
processControllerException(throw, {js_redirect, Url, _Cookie}, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) JS Redirect to ~p~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Url])),
    V = {struct, [{<<"REDIRECT">>, list_to_binary(Url)}]},
    DataOut = mochijson2:encode(V),
    Req:ok({?OUTPUT_JSON, [], [DataOut]});

%%
%% Перенаправление без Cookie, как для аутентификации.
%%
processControllerException(throw, {redirect, Url, []}, Req) ->
    flog:debug(?FMT("~p:~p 302 ~p REQUEST (~p) Redirect to ~p~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Url])),
    Req:respond({302, [{"Location", Url}, {"Content-Type", ?OUTPUT_HTML}], ""});

%%
%% Перенаправление с новыми Cookie, как для слепых.
%%
processControllerException(throw, {redirect, Url, Cookie}, Req) ->
    flog:debug(?FMT("~p:~p 302 ~p REQUEST (~p) Redirect to ~p~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Url])),
    Req:respond({302, [{"Location", Url}, {"Content-Type", ?OUTPUT_HTML}] ++ Cookie,""});
    
processControllerException(throw, not_found, Req) ->
    flog:info(?FMT("~p:~p 404 ~p REQUEST (~p) Not Found~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    Req:not_found();

processControllerException(throw, auth_required, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) AUTH REQUIRED~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    V = {struct, [{<<"REDIRECT">>, <<"/">>}]},
    DataOut = mochijson2:encode(V),
    Req:ok({?OUTPUT_JSON, [], [DataOut]});

%%
%% application/json 
%%
processControllerException(throw, {auth_ajax, State} , Req) ->
    {Iserr, Val} = State,
    case Iserr of
        ok      ->
            V = {struct, [{<<"mess">>, null }, {"val", list_to_binary(Val)}]};
        error   ->
            case Val of
                {M, _N} ->
                    Emess = list_to_binary(M);
                [_H | _] ->
                    Emess = list_to_binary(Val);
                _ ->
                    Emess = <<"unknown">>
            end,
            V = {struct, [{<<"mess">>, Emess }, {"val", Emess}]}
    end,
    Req:ok({?OUTPUT_JSON, [], [mochijson2:encode(V)]});


processControllerException(throw, auth_required_dialog, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) AUTH REQUIRED~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    V = {struct, [{<<"ERROR">>, <<"auth_required">>}]},
    DataOut = mochijson2:encode(V),
    Req:ok({?OUTPUT_JSON, [], [DataOut]});
processControllerException(throw, {cookize, Ctype, Cookie, Body}, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) COOKIZE~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    Req:respond({200, [{"Content-Type", Ctype}, Cookie], Body});
processControllerException(Type, Exc, Req) ->
    flog:error(?FMT("~p:~p Catch unknown exception (~p) on ~p request ~p ~n",[?MODULE, ?LINE, Exc, Req:get(method), Req:get(path)])),
    Type(Exc).


%% ========================================================================
%% СТАТИЧЕСКОЕ ОТОБРАЖЕНИЕ АДРЕСОВ
%% ========================================================================

serve_request(menu, Req) ->
    case authorization:auth_if(Req, "admin") of
        false ->
    	    io:format("COMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM~n"),
            serve_static(?RIA_HOME, ?RIA_MENU_COMMON_PATH, Req);
        R ->
    	    io:format("ADMIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIN, ~p~n",[R]),
    	    
            serve_static(?RIA_HOME, ?RIA_MENU_ADMIN_PATH, Req)
    end;

serve_request(?RIA_MENU_URL_, Req) ->
    serve_request(menu, Req);


serve_request(?RIA_MENU_URL, Req) ->
    serve_request(menu, Req);


%%% RIA DEBUG
serve_request("/" ++ ?QOOXDOO_BUILD ++ "/index.html", Req) ->
    serve_request("INDEX", Req);

serve_request("/" ++ ?QOOXDOO_BUILD ++ "/" ++ T, Req) ->
    serve_static(?RIA_HOME, T, Req);

%%% RIA PRODUCTION
serve_request("/resource/" ++ T, Req) ->
    serve_static(?RIA_HOME ++ "/resource/", T, Req);

serve_request(?RIA_DEPS_URL ++ T, Req) ->
    serve_static(?RIA_DEPS_PATH, T, Req);

serve_request(?RIA_BIN_URL ++ T , Req) ->
    serve_static(?RIA_BIN_PATH, T, Req);

%%% JS
serve_request(?STATIC_JS_URL ++ "ria.js", Req) ->
    serve_static(?RIA_BIN_PATH, "zqr.js", Req);

serve_request(?STATIC_JS_URL ++ T, Req) ->
    serve_static(?STATIC_JS_PATH, T, Req);

%%% IMAGES
serve_request(?STATIC_IMAGES_URL ++ T, Req) ->
    serve_static(?STATIC_IMAGES_PATH, T, Req);

%%% FAVICON
serve_request(?STATIC_FAVICON_URL, Req) ->
    serve_static(?STATIC_FAVICON_PATH, [], Req);

%%% CSS
serve_request(?STATIC_CSS_URL ++ T, Req) ->
    serve_static(?STATIC_CSS_PATH, T, Req);

%%% DATA
serve_request(?STATIC_DATA_URL ++ T, Req) ->
    serve_static(?STATIC_DATA_PATH, T, Req);

%%% MEDIA
serve_request(?STATIC_MEDIA_URL ++ T, Req) ->
    serve_static(?STATIC_MEDIA_PATH, T, Req);





%% ========================================================================
%% КОНТРОЛЛЕРЫ  
%% ========================================================================
    
serve_request(Path, Req) ->
   
    case Path of
        _   ->
                try web_url:dmap(Path) of
                    {Module, Action} ->
                        save_start_controller(Module, Action, Req);
                    {Module, Action, Param} ->
                        ?D("Param = ~p", [Param]),
                        save_start_controller(Module, Action, Req, Param)
                catch
                    _:_ ->
                    ?I("~p:~p 404 ~p REQUEST (~p) ERROR! Controller NOT FOUND~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)]),
                    Req:not_found()
                end
    end.

loop(Req, _DocRoot) ->
    serve_request(Req:get(path), Req).

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

iterKill(0, _) ->
    ok;
iterKill(X, F) ->
    F(),
    iterKill(X-1, F).

test(Fun, X) ->
    Start = utils:utime(),
    iterKill(X, Fun),
    Stop = utils:utime(),
    io:format("::: ~p~n", [Stop - Start]).

% ---------------------------------------------------------------------------

killSlash(Path) ->
    killSlash1(Path).

killSlash1("") ->
    "";

killSlash1(Path) ->
    io:format("~p~n", [Path]),
    lists:sublist(Path, 1, length(Path) - 1).

killSlash2(Path) ->
    Options = [{capture, all, list}],
	case re:run(Path, "(.*)/", Options) of
		{match, [_, Param]}->
			Param;
		nomatch ->
			"" % Пустая строка
	end.
