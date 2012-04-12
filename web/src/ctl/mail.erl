%%% @file mail.erl
%%%
%%%     Контроллеры, если пользователи пока не заходили
%%%

-module(mail).

-define(UPDATER_ID, 1).

-export([
    mkbill/1,
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



mail(Rmail, Rname, Rsubject, Rbody) ->
    Email = {<<"text">>, <<"plain">>,
            [
                {<<"From">>, mmh_person(?SYS_MAIL_NAME, ?SYS_MAIL_USERNAME)},
                {<<"To">>,   mmh_person(Rname, Rmail)},
                {<<"Subject">>,
                    erlang:list_to_binary(mmh_utf8(Rsubject))
                }
            ], [], Rbody},
    gen_smtp_client:send(
        {
            ?SYS_MAIL_USERNAME,
            [Rmail],
            mimemail:encode(Email)
        },  ?SYS_MAIL_OPTIONS
    ).


paybill({Rmail, Rname, {data, Acv_video}}) ->
    Rsubject = "Ваш счет оплачен",
    Xslb_path = "xsl/mail/outside/paybill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",
                [
                    {"sys-dns",     ?SYS_DNS},
                    {"usermail",    Rmail},
                    {"username",    Rname}
                ]
            },
            {"video", Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail(Rmail, Rname, Rsubject, Rbody).

mkbill({Rmail, Rname, {data, Acv_video}}) ->
    Rsubject = "Выставлен счет",
    Xslb_path = "xsl/mail/outside/mkbill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",
                [
                    {"sys-dns",     ?SYS_DNS},
                    {"usermail",    Rmail},
                    {"username",    Rname}
                ]
            },
            {"video", Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail(Rmail, Rname, Rsubject, Rbody).


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