%%% @file mail.erl
%%%
%%%     Контроллеры, отправки почты
%%%

-module(mail).

-export([
    add_insider_customer/1,
    del_insider_customer/1,
    mkbill_customer/1,
    paybill_customer/1,

    % -----------------------
    add_insider/1,
    del_insider/1,
    mkbill/1,
    paybill/1,
    % -----------------------

    test/0,
    test/1
]).


-include("common.hrl").

add_insider({Rmail, Rname, {data, Customer}}) ->
    add_insider_customer({Rmail, Rname, {data, Customer}}).

del_insider({Rmail, Rname, {data, Customer}}) ->
    del_insider_customer({Rmail, Rname, {data, Customer}}).

add_insider_customer({Rmail, Rname, {data, Customer}}) ->
    Rsubject = "Ваш аккаунт одобрен модератором",
    Xslb_path = "xsl/mail/customer/mkbill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",
                [
                    {"sys-dns",     ?SYS_DNS},
                    {"usermail",    Rmail},
                    {"username",    Rname}
                ]
            },
            {"сustomer", Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmail, Rname, Rsubject, Rbody).

del_insider_customer({Rmail, Rname, {data, Customer}}) ->
    Rsubject = "Ваш аккаунт удален модератором",
    Xslb_path = "xsl/mail/customer/mkbill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",
                [
                    {"sys-dns",     ?SYS_DNS},
                    {"usermail",    Rmail},
                    {"username",    Rname}
                ]
            },
            {"сustomer", Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmail, Rname, Rsubject, Rbody).


mkbill({Rmail, Rname, {data, Acv_video}}) ->
    mkbill_customer({Rmail, Rname, {data, Acv_video}}).

%%%
%%% @doc
%%%    Отправляет почту пользователю, если счет выставлен
%%%
mkbill_customer({Rmail, Rname, {data, Acv_video}}) ->
    Rsubject = "Выставлен счет",
    Xslb_path = "xsl/mail/customer/mkbill.xsl",
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
    mail_utils:mail(Rmail, Rname, Rsubject, Rbody).

paybill({Rmail, Rname, {data, Acv_video}}) ->
    paybill_customer({Rmail, Rname, {data, Acv_video}}).

%%%
%%% @doc
%%%    Отправляет почту пользователю, если счет оплачен
%%%
paybill_customer({Rmail, Rname, {data, Acv_video}}) ->
    Rsubject = "Ваш счет оплачен",
    Xslb_path = "xsl/mail/customer/paybill.xsl",
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
    mail_utils:mail(Rmail, Rname, Rsubject, Rbody).




test()->
    ok.

test(speed) ->
    ok.