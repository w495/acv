


-define(RIA_DEPS_URL,  "/deps/qooxdoo/").
-define(RIA_DEPS_PATH, "deps/qooxdoo/").

-ifdef(debug).
    -define(QOOXDOO_BUILD, "source").
-else.
    -define(QOOXDOO_BUILD, "build").
-endif.

-define(RIA_HOME, "js/" ++ ?QOOXDOO_BUILD).

-define(RIA_BIN_URL, "/script/").
-define(RIA_BIN_PATH, ?RIA_HOME ++ "/script/").



-define(RIA_MENU_URL_,
    "/" ++ ?QOOXDOO_BUILD ++ "/resource/zqr/descr/menu.json").

-define(RIA_MENU_URL, "/resource/zqr/descr/menu.json").

-define(RIA_MENU_ADMIN_PATH,  "resource/zqr/descr/menu.admin.json").
-define(RIA_MENU_COMMON_PATH, "resource/zqr/descr/menu.common.json").






-define(STATIC_DATA_URL,        "/static/data/").
-define(STATIC_DATA_PATH,       "static/data/").


-define(STATIC_MEDIA_URL,        "/sm/").
-define(STATIC_MEDIA_PATH,       "static/site-media/").

-define(STATIC_CSS_URL,        "/c/").
-define(STATIC_CSS_PATH,       "static/site-media/css/").

-define(STATIC_JS_URL,        "/j/").
-define(STATIC_JS_PATH,       "static/site-media/js/").

-define(STATIC_IMAGES_URL,        "/i/").
-define(STATIC_IMAGES_PATH,       "static/site-media/images/").


-define(STATIC_FAVICON_URL,        "/favicon.ico").
-define(STATIC_FAVICON_PATH,       "static/site-media/favicon.ico").


%%% ---------------------------------------------------------------------
%%% MIME Вывода
%%% ---------------------------------------------------------------------

-define(OUTPUT_JSON,        "application/json").
-define(OUTPUT_HTML,        "text/html;charset=UTF-8").
-define(OUTPUT_TEXT,        "text/plain;charset=UTF-8").
