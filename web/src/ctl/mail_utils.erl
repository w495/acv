%%% @file mail_utils.erl
%%%
%%%     Вспомогательные контроллеры, отправки почты
%%%

-module(mail_utils).

-export([
    mail/3,
    mail/4,
    test_mail/0,
    test/0,
    test/1
]).

-include("common.hrl").
-include("web.hrl").

%%%
%%% @doc
%%%    Формирует строку для заголовка почты в формате utf8
%%%
mmh_utf8(In)->
    "=?UTF-8?B?"++ base64:encode_to_string(In) ++ "?=".

%%%
%%% @doc
%%%     Формирует значение заголовка
%%%     отправителя \ получателя почты в формате utf8
%%%     Если имя и адрес не указаны,
%%%     используются значения для системы.
%%%     Т.е если пользователь в какой-то момень отказался без почты,
%%%     то система отпраляет почту сама себе.
%%%
mmh_person(null, null)->
    mmh_person(?SYS_MAIL_NAME, ?SYS_MAIL_USERNAME);

mmh_person(null, Mail)->
    mmh_person(?SYS_MAIL_NAME, Mail);

mmh_person(Name, null)->
    mmh_person(Name, ?SYS_MAIL_USERNAME);

mmh_person(Name, Mail)->
    ?D("Name = ~p, Mail = ~p~n", [Name, Mail]),
    erlang:list_to_binary(
        mmh_utf8(Name) ++ "<" ++ Mail ++ ">"
    ).

%%%
%%% @doc
%%%    Обертка mail/4
%%%
mail({Rmail, Rname}, Rsubject, Rbody) ->
    mail(Rmail, Rname, Rsubject, Rbody);

%%%
%%% @doc
%%%    Отправляет почту нескольким получателям
%%%
mail([], Rsubject, Rbody) -> ok;
mail([{Rmail, Rname} | Rest ], Rsubject, Rbody) ->
    mail(Rmail, Rname, Rsubject, Rbody),
    mail(Rest, Rsubject, Rbody).

%%%
%%% @doc
%%%    Отправляет почту одному получателю
%%%
mail(null, Rname, Rsubject, Rbody) ->
    mail(?SYS_MAIL_USERNAME, Rname, Rsubject, Rbody);

mail(Rmail, Rname, Rsubject, Rbody) ->
    ?D("~n mail goes to send (mail = ~p, name = ~p, subject = ~p)~n",
        [Rmail, Rname, base64:encode_to_string(Rsubject)]),
    Email = {<<"text">>, <<"plain">>,
            [
                {<<"From">>, mmh_person(?SYS_MAIL_NAME, ?SYS_MAIL_USERNAME)},
                {<<"To">>,   mmh_person(Rname, Rmail)},
                {<<"Subject">>,
                    erlang:list_to_binary(mmh_utf8(Rsubject))
                }
            ], [], Rbody},
    Ans = gen_smtp_client:send(
        {
            ?SYS_MAIL_USERNAME,
            [Rmail],
            mimemail:encode(Email)
        },  ?SYS_MAIL_OPTIONS
    ),
    ?I("~n mail was send (mail = ~p, name = ~p, subject = ~p)~n",
        [Rmail, Rname, base64:encode_to_string(Rsubject)]),
    Ans.


test_mail() ->

    Rmail = "countff@gmail.com",
    Rname = "Получатель",
    Rsubject = "Тема письма",
%    Xslb_path = "xsl/mail/outside/test_mailb.xsl",
    Xslb_path = "xsl/mail/outside/mkbill.xsl",

%    Xmlb  = xml:encode_data([{"meta",[{"current-path","sd"}]}]),
    Xmlb  = xml:encode_data(
        [
            {"meta",
                [
                    {"sys-dns",     ?SYS_DNS},
                    {"usermail",    Rmail},
                    {"username",    Rname}
                ]
            },
            {"video", "JJJJJJJ"}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),

    mail(Rmail, Rname, Rsubject, Rbody).


test()->
    ok.

test(speed) ->
    ok.