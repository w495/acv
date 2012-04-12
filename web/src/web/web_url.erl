-module(web_url).

-export([dmap/1]).

dmap(Path) ->
    case Path of
    % adv request
        "/adv" ->
            {adv_manager, get_adv};

    % Конфигарация системы

        "/get-config" -> {inside, get_config};
        "/update-config" -> {inside, update_config};

    % advertising company video

        "/get-acv-video" -> {inside, get_acv_video};

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

        %%% Регионы

            "/get-all-geo-regions" ->  {inside, get_all_geo_regions};
            "/get-contries" ->  {inside, get_contries};
            "/get-contries-sng" ->  {inside, get_contries_sng};

            "/get-cities" ->  {inside, get_cities};


        "/get-acv-videos"   -> {inside, get_acv_videos};

        "/get-acv-video/common"   -> {inside, get_acv_video_common};
        "/get-acv-video/upload"   -> {inside, get_acv_video_upload};
        "/get-acv-video/show"     -> {inside, get_acv_video_show};

        "/get-acv-video/users-targeting"
            -> {inside, get_acv_video_users};
        "/get-acv-video/region-targeting"
            -> {inside, get_acv_video_geos};
        "/get-acv-video/category-targeting"
            -> {inside, get_acv_video_cats};

        "/chstate-acv-video"
            -> {inside, chstate_acv_video};

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


    % customer-groups *
        "/get-customer-groups" ->       {inside, get_customer_groups};
        "/get-customer-group-info" ->   {inside, get_customer_group_info};
        "/update-customer-group" ->     {inside, update_customer_group};
        "/delete-customer-group" ->     {inside, delete_customer_group};

    % customers *
        "/get-customers" ->         		{inside, get_customers};
        "/get-experts" ->           		{inside, get_experts};
        "/get-customer-info" ->     		{inside, get_customer_info};
        "/update-customer" ->       		{inside, update_customer};
        "/update-customer/upload-image" ->  {web_file, upload_customer_image};
		
	% view профиля ( от имени пользователя )
        "/update-customer-profile" ->       {user_ctl, update_customer_profile};
        "/get-customer-profile" ->       	{user_ctl, get_customer_profile};

        "/delete-customer" ->       {inside, delete_customer};
        "/get-permissions" ->       {inside, get_permissions};

    % do_*
        "/do_login" ->          {authorization, do_login};
        % логаут для qooxdoo ( возвращает JSON )
		"/do_logout" ->         {authorization, do_logout};
		% логаут для человеков ( редиректит на главную страницу )
        "/logout" ->            {authorization, do_logout_with_redirect};
        "/do_change_pass" ->    {authorization, do_change_pass};
        "/login" ->             {authorization, login};
    %%
    %% Внешние страницы, до захода пользователя в систему
    %%
        "/" ->                  {outside, index};
        "/index" ->             {outside, index};
        "/about" ->             {outside, about};
        "/docs" -> 				{outside, docs};
        % Страница не требуется
		%"/docs/video" -> 		{outside, docs_video};
        "/docs/audience" ->     {outside, docs_audience};
        "/docs/content" ->     	{outside, docs_content};

        "/signin" ->            {outside, signin};
        "/signin/post" ->       {outside, signin_post};

        "/pref" ->              {outside, pref};
        "/pers" ->              {outside, pers};
        "/pay" ->               {outside, pay};

        "/surl" ->              {outside, surl};
        "/furl" ->              {outside, furl};
        "/curl" ->              {outside, curl};

        "/signup" ->            {outside, signup};
        "/signup/post" ->       {outside, signup_post};
        "/captcha.png" ->       {outside, captcha};

    %%
    %% Тестирование почты
    %%

        "/mail/signup/" ++ Args    -> {mail, signup,    [Args]};
        "/mail/signdown/" ++ Args  -> {mail, signdown,  [Args]};

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

