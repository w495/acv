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
    V = {struct, [{<<"REDIRECT">>, <<"/login">>}]},
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


processControllerException(throw, auth_required_dialog, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) AUTH REQUIRED~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    V = {struct, [{<<"ERROR">>, <<"auth_required">>}]},
    DataOut = mochijson2:encode(V),
    Req:ok({?OUTPUT_JSON, [], [DataOut]});
processControllerException(throw, {cookize, ContentType, Cookie, Body}, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) COOKIZE~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    Req:respond({200, [{"Content-Type", ContentType}, Cookie], Body});
processControllerException(Type, Exc, Req) ->
    flog:error(?FMT("~p:~p Catch unknown exception (~p) on ~p request ~p ~n",[?MODULE, ?LINE, Exc, Req:get(method), Req:get(path)])),
    Type(Exc).


%% ========================================================================
%% СТАТИЧЕСКОЕ ОТОБРАЖЕНИЕ АДРЕСОВ
%% ========================================================================

serve_request(?RIA_MENU_URL, Req) ->
    case authorization:auth_if(Req, "admin") of
        false ->
            serve_static(?RIA_HOME, ?RIA_MENU_COMMON_PATH, Req);
        _ ->
            serve_static(?RIA_HOME, ?RIA_MENU_ADMIN_PATH, Req)
    end;

serve_request("/" ++ ?QOOXDOO_BUILD ++ "/index.html", Req) ->
    serve_request("INDEX", Req); % перенаправление в динамический мэппинг для проверки авторизации

serve_request("/" ++ ?QOOXDOO_BUILD ++ "/" ++ T, Req) ->
    serve_static(?RIA_HOME, T, Req);

serve_request("/deps/qooxdoo/" ++ T, Req) ->
    serve_static("deps/qooxdoo/", T, Req);

serve_request("/resource/" ++ T, Req) ->
    serve_static(?RIA_HOME ++ "/resource/", T, Req);

serve_request(?STATIC_FAVICON_URL, Req) ->
    serve_static(?STATIC_FAVICON_PATH, [], Req);

serve_request(?STATIC_DATA_URL ++ T, Req) ->
    serve_static(?STATIC_DATA_PATH, T, Req);

serve_request(?STATIC_MEDIA_URL ++ T, Req) ->
    serve_static(?STATIC_MEDIA_PATH, T, Req);

serve_request(?STATIC_CSS_URL ++ T, Req) ->
    serve_static(?STATIC_CSS_PATH, T, Req);

serve_request(?STATIC_JS_URL ++ T, Req) ->
    serve_static(?STATIC_JS_PATH, T, Req);

serve_request(?STATIC_IMAGES_URL ++ T, Req) ->
    serve_static(?STATIC_IMAGES_PATH, T, Req);


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
                    ?INFO(?FMT("~p:~p 404 ~p REQUEST (~p) ERROR! Controller NOT FOUND~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
                    Req:not_found()
                end
    end.

%% ========================================================================
%% Простой map контроллеров
%% ========================================================================

simple_map_controllers(Path) ->
    case Path of
    % adv request
        "/adv" ->
            {adv_manager, get_adv};

    % advertising company video
        %"/get-adv-coms-vid" -> {inside, get_adv_coms_vid};

            "/get_acv_video_by_id" -> {inside, getAcvVideoById};


        %%% Вся статисттика всех покупателей
            "/get-all-acv-video-stats" -> {inside, get_all_acv_video_stats};

        %%% Вся статисттика всех покупателей
            "/get-acv-video-stats" -> {inside, get_acv_video_stats};

        %%% Cтатистика конкретной рекламной компании
           "/get-acv-video-stat"      ->
               {inside, get_acv_video_stat};

        %%% Cтатистика конкретной рекламной компании (по фильмам)
            "/get-acv-video-stat/by-films" ->
                {inside, get_acv_video_stat_by_films};

        %%% Cтатистика конкретной рекламной компании (по фильмам)
            "/get-acv-video-stat/by-film" ->
                {inside, get_acv_video_stat_by_film};

        %%% Все рекламные компании всех покупателей
            "/get-all-acv-videos"        ->  {inside, get_all_acv_videos};


        "/get-all-geo-regions"       ->  {inside, get_all_geo_regions};
        "/get-all-acv-banners"       ->  {inside, get_all_acv_banners};

        "/get-acv-videos"   -> {inside, get_acv_videos};

        "/get-acv-video/common"   -> {inside, get_acv_video_common};
        "/get-acv-video/upload"   -> {inside, get_acv_video_upload};
        "/get-acv-video/show"     -> {inside, get_acv_video_show};

        "/get-acv-video/users-targeting"
            -> {inside, get_acv_video_users_targeting};
        "/get-acv-video/region-targeting"
            -> {inside, get_acv_video_region_targeting};
        "/get-acv-video/category-targeting"
            -> {inside, get_acv_video_category_targeting};


        "/disactivate-acv-video"
            -> {inside, disactivate_acv_video};

        "/activate-acv-video"
            -> {inside, activate_acv_video};


        "/start-acv-video"
            -> {inside, start_acv_video};

        "/stop-acv-video"
            -> {inside, stop_acv_video};



        "/delete-acv-video"
            -> {inside, delete_acv_video};

        "/full-delete-acv-video"
            -> {inside, full_delete_acv_video};


        "/get-acv-banners"  -> {inside, get_acv_banners};

        "/update-acv-video"                 -> {inside, update_acv_video};
        "/update-acv-video/uload-video"     -> {web_file, upload_acv_video};


        "/get-all-cats"  -> {inside, get_all_cats};
        "/get-adv-com-vid" -> {inside, get_adv_com_vid};
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

        "/get-customer-info" ->
            {inside, get_customer_info};

        "/update-customer" ->
            {inside, update_customer};

        "/update-customer/upload-image" ->
                {web_file, upload_customer_image};

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

        "/signin" ->            {outside, signin};
        "/signin/post" ->       {outside, signin_post};

        "/signup" ->            {outside, signup};
        "/signup/post" ->       {outside, signup_post};
        "/captcha.png" ->           {outside, captcha};
    %%
    %% Основа админки
    %%

        "INDEX" ->
            {index, index};

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
