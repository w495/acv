-define(SESSION_TABLE_NAME, list_to_atom(atom_to_list(node()) ++ "_session")).

-record(web_session, {
    uid,
    login,
    customer_id,
    permissions=[],
    time,
    password_hash
}).


-define(AUTHCOOKIE, config:get(cookiename, "MCHS")).
-define(EXPCOOKIE, config:get(expcookie, 18000)).


-define(F_COOKIEOPTIONS, [{max_age, ?EXPCOOKIE}, {path, "/"}]).

-define(CATCHA_COOKIE, "captcha_codehex").