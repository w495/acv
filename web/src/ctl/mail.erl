%%% @file mail.erl
%%%
%%%     Контроллеры, если пользователи пока не заходили
%%%

-module(mail).

-define(UPDATER_ID, 1).

-export([
    chstate/1,
    mail/4,
    test_mail/0,
    test/0,
    test/1
]).


-import(mochiweb_cookies, [cookie/2]).

-include("web_session.hrl").
-include("common.hrl").
-include("web.hrl").

mmh_utf8(In)->
    "=?UTF-8?B?"++ base64:encode_to_string(In) ++ "?=".

mmh_person(Name, Mail)->
    erlang:list_to_binary(
        mmh_utf8(Name) ++ "<" ++ Mail ++ ">"
    ).

sys_mail_name() ->
    config:get(sys_mail_username, "Система рекламы tvzavr.ru").

sys_mail_username() ->
    config:get(sys_mail_username, "nikitin.i@tvzavr.ru").

sys_mail_relay() ->
    config:get(sys_mail_relay, "active-video.ru").

sys_mail_password() ->
    config:get(sys_mail_password, "maiqu6Ce6aht").

sys_mail_options() ->
    [
        {relay,    sys_mail_relay()},
        {username, sys_mail_username()},
        {password, sys_mail_password()}
    ].


mail(Rmail, Rname, Rsubject, Rbody) ->
    Email = {<<"text">>, <<"plain">>,
            [
                {<<"From">>, mmh_person(sys_mail_name(), sys_mail_username())},
                {<<"To">>,   mmh_person(Rname, Rmail)},
                {<<"Subject">>,
                    erlang:list_to_binary(mmh_utf8(Rsubject))
                }
            ], [], Rbody},
    gen_smtp_client:send(
        {
            sys_mail_username(),
            [Rmail],
            mimemail:encode(Email)
        },  sys_mail_options()
    ).



chstate(Param) ->
    Xslh_path = "xsl/mail/outside/chstateh.xsl",
    Xslb_path = "xsl/mail/outside/chstateb.xsl",

    Metah = [
            {"current-path",        "sd"}
    ],

    Metab = [
            {"current-path",        "sd"}
    ],

    Xmlh  = xml:encode_data(
        [
            {"meta",    Metah}
        ]
    ),

    Xmlb  = xml:encode_data(
        [
            {"meta",    Metab}
        ]
    ),

    Outtyh = xslt:apply(Xslh_path, Xmlh),
    Rbody = xslt:apply(Xslb_path, Xmlb),



    ok.


test_mail() ->

    Rmail = "w@w-495.ru",
    Rname = "Получатель",
    Rsubject = "Тема письма",

    Xslb_path = "xsl/mail/outside/test_mailb.xsl",
    Xmlb  = xml:encode_data([{"meta",[{"current-path","sd"}]}]),
    Rbody = xslt:apply(Xslb_path, Xmlb),

    mail(Rmail, Rname, Rsubject, Rbody).


test()->
    % erlang 13 ->
    %   http:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []).
    % erlang 15 ->
    %   httpc:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []).
    %?HTTPC:request(get, {"http://127.0.0.1:8000", [{"connection", "close"}]}, [], []),

    ok.

test(speed) ->

    ok.