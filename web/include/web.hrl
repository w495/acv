

-ifdef(debug).
    -define(QOOXDOO_BUILD, "source").
-else.
    -define(QOOXDOO_BUILD, "build").
-endif.

-define(RIA_HOME, "js/" ++ ?QOOXDOO_BUILD).


-define(RIA_MENU_URL,
    "/" ++ ?QOOXDOO_BUILD ++ "/resource/bsk/descr/menu.json").

-define(RIA_MENU_ADMIN_PATH,  "resource/bsk/descr/menu.admin.json").
-define(RIA_MENU_COMMON_PATH, "resource/bsk/descr/menu.common.json").


-define(STATIC_DATA_URL,        "/data/").
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


-define(OUTPUT_JSON,        "application/json").
-define(OUTPUT_HTML,        "text/html; charset=UTF-8").

-define(OUTPUT_TEXT,        "text/plain; charset=UTF-8").
