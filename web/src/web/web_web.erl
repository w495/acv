-module(web_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).
-include_lib("xmerl/include/xmerl.hrl").
-include("../include/common.hrl").

-compile(export_all).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    io:format("~p: ~p got stop signal~n", [erlang:localtime(), ?MODULE]),
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
            %% io:format("*************** ~p ~p~n", [T, P]),
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
    Req:ok({"application/json", [], [DataOut]});

%%
%% Перенаправление без Cookie, как для аутентификации.
%%
processControllerException(throw, {redirect, Url, []}, Req) ->
    flog:debug(?FMT("~p:~p 302 ~p REQUEST (~p) Redirect to ~p~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Url])),
    Req:respond({302, [{"Location", Url}, {"Content-Type", "text/html; charset=UTF-8"}], ""});

%%
%% Перенаправление с новыми Cookie, как для слепых.
%%
processControllerException(throw, {redirect, Url, Cookie}, Req) ->
    flog:debug(?FMT("~p:~p 302 ~p REQUEST (~p) Redirect to ~p~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Url])),
    Req:respond({302, [{"Location", Url}, {"Content-Type", "text/html; charset=UTF-8"}] ++ Cookie,""});
    
processControllerException(throw, not_found, Req) ->
    flog:info(?FMT("~p:~p 404 ~p REQUEST (~p) Not Found~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    Req:not_found();

processControllerException(throw, auth_required, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) AUTH REQUIRED~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    %V = {struct, [{<<"REDIRECT">>, <<"/login.html">>}]},
    V = {struct, [{<<"REDIRECT">>, <<"/login">>}]},
    DataOut = mochijson2:encode(V),
    Req:ok({"application/json", [], [DataOut]});

%%
%% Перенаправление без аутентификации.
%%
processControllerException(throw, {auth_required_front, RetPath} , Req) ->
    flog:debug(?FMT("~p:~p 302 ~p REQUEST (~p) AUTH REQUIRED~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    processControllerException(throw, {redirect, "/Users/Login" ++ RetPath, []}, Req);

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
                    ErrMess = list_to_binary(M);
                [_H | _] ->
                    ErrMess = list_to_binary(Val);
                _ ->
                    ErrMess = <<"unknown">> 
            end,
            V = {struct, [{<<"mess">>, ErrMess }, {"val", ErrMess}]}
    end,
    DataOut = mochijson2:encode(V),
    Req:ok({"application/json", [], [DataOut]});

%%
%% application/json
%%
processControllerException(throw, {banners_ajax, State} , Req) ->
    Req:ok({"application/json", [], [mochijson2:encode(State)]});

processControllerException(throw, auth_required_dialog, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) AUTH REQUIRED~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    V = {struct, [{<<"ERROR">>, <<"auth_required">>}]},
    DataOut = mochijson2:encode(V),
    Req:ok({"application/json", [], [DataOut]});
    
processControllerException(Type, Exc, Req) ->
    flog:error(?FMT("~p:~p Catch unknown exception (~p) on ~p request ~p ~n",[?MODULE, ?LINE, Exc, Req:get(method), Req:get(path)])),
    Type(Exc).


%% ========================================================================
%% СТАТИЧЕСКОЕ ОТОБРАЖЕНИЕ АДРЕСОВ
%% ========================================================================

serve_request("/"++?QOOXDOOBUILD++"/index.html", Req) ->
    % % io:format("goto INDEX"),
    serve_request("INDEX", Req); % перенаправление в динамический мэппинг для проверки авторизации
    
serve_request("/" ++ ?QOOXDOOBUILD ++ "/" ++ T, Req) ->
    serve_static(?JSHOME, T, Req);
    
serve_request("/deps/qooxdoo/" ++ T, Req) ->
    serve_static("deps/qooxdoo/", T, Req);
    
serve_request("/resource/" ++ T, Req) ->
    serve_static(?JSHOME ++ "/resource/", T, Req);

serve_request("/favicon.ico", Req) ->
    serve_static("static/site-media/favicon.ico", "", Req);
    
serve_request("/static/" ++ T, Req) ->
    % % io:format("TTT: ~p~n", [T]),
    serve_static("static", T, Req);

%% ========================================================================
%% КОНТРОЛЛЕРЫ  
%% ========================================================================
    
serve_request(Path, Req) ->
   
    case Path of
        _   ->
                try simple_map_controllers(Path) of
                    {Module, Action} ->
                        save_start_controller(Module, Action, Req)
                catch
                    _:_ ->
                    flog:info(?FMT("~p:~p 404 ~p REQUEST (~p) ERROR! Controller NOT FOUND~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
                    Req:not_found()
                end
    end.

%% ========================================================================
%% Простой map контроллеров (испрользуется в админке)
%%      Оставлен по историческим причинам.
%% ========================================================================

simple_map_controllers(Path) ->
    case Path of
    % advertising company video
        "/get-adv-coms-vid" -> {inside, get_adv_coms_vid};
        "/get-adv-com-vid" -> {inside, get_adv_com_vid};
        "/update-adv-com/upload-video" -> {web_file, upload_adv_com_video};
        "/update-adv-com-vid" -> {inside, update_adv_com_vid};

    % advertising company
        "/get-banner-places" -> {inside, get_banner_places};
        "/get-adv-coms" -> {inside, get_adv_coms};
        "/get-adv-com" -> {inside, get_adv_com};
        "/update-adv-com/upload-image" -> {web_file, upload_adv_com_image};
        "/update-adv-com" -> {inside, update_adv_com};

    % customer-groups *
        "/get-customer-groups" ->       {inside, get_customer_groups};
        "/get-customer-group-info" ->   {inside, get_customer_group_info};
        "/update-customer-group" ->     {inside, update_customer_group};
        "/delete-customer-group" ->     {inside, delete_customer_group};

    % customers *
        "/get-customers" ->         {inside, get_customers};
        "/get-experts" ->           {inside, get_experts};

        "/get-customer-info" ->     {inside, get_customer_info};
        "/update-customer" ->       {inside, update_customer};
        "/update-customer/upload-image" ->       {web_file, upload_customer_image};

        "/delete-customer" ->       {inside, delete_customer};
        "/get-permissions" ->       {inside, get_permissions};

    % do_*
        "/do_login" ->          {authorization, do_login};
        "/do_logout" ->         {authorization, do_logout};
        "/do_change_pass" ->    {authorization, do_change_pass};
        "/login" ->             {authorization, login};
    %%
    %% Внешние страницы, до захода пользователя в систему
    %%
        "/" ->                  {outside, index};
        "/index" ->             {outside, index};
        "/about" ->             {outside, about};

        %"INDEX" ->
        %    {index, index};

    %%
    %% Прочее
    %%
        _ ->
            throw(not_found)
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
