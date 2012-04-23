%%% @file mail.erl
%%%
%%%     Контроллеры, отправки почты
%%%

-module(mail).

-export([
    % -----------------------
    add_insider/2,
    del_insider/2,
    mkbill/2,
    paybill/2,
    % -----------------------

    test/0,
    test/1
]).


-include("common.hrl").


meta([{Rmail, Rname} = Rmn|_]) ->
    [
        {"sys-dns",     ?SYS_DNS},
        {"usermail",    Rmail},
        {"username",    Rname}
    ];

meta([Rmn]) ->
    [
        {"sys-dns",     ?SYS_DNS}
    ].

%%% -----------------------------------------------------------------------
%%%
%%%     dosome(Req, Param)
%%%
%%%         Req  ::=
%%%                     {Type, Rmn}
%%%         Type ::=
%%%                     customer    ;
%%%                     sysmsg
%%%         Rmn  :: =
%%%                     [{Mail, Name}|_] ;
%%%                     {Mail, Name}
%%%         Param :: =
%%%                     {data, Data}
%%% -----------------------------------------------------------------------


create_customer({sysmsg, Rmn}, {data, Customer}) ->
    Rsubject = "Появился новый пользователь",
    Xslb_path = "xsl/mail/customer/create-customer.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

create_customer({customer, Rmn}, {data, Customer}) ->
    Rsubject = "Спасибо за регистрацию",
    Xslb_path = "xsl/mail/customer/create-customer.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody).

%%% --------------------------------------------------------------------------

delete_customer({sysmsg, Rmn}, {data, Customer}) ->
    Rsubject = "Ваш аккаунт был удален",
    Xslb_path = "xsl/mail/customer/delete-customer.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

delete_customer({customer, Rmn}, {data, Customer}) ->
    Rsubject = "Ваш аккаунт был удален",
    Xslb_path = "xsl/mail/customer/delete-customer.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody).

%%% --------------------------------------------------------------------------

add_insider({sysmsg, Rmn}, {data, Customer}) ->
    Rsubject = "Ваш аккаунт одобрен модератором",
    Xslb_path = "xsl/mail/customer/add-insider.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

add_insider({customer, Rmn}, {data, Customer}) ->
    Rsubject = "Ваш аккаунт одобрен модератором",
    Xslb_path = "xsl/mail/customer/add-insider.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody).

%%% --------------------------------------------------------------------------

del_insider({sysmsg, Rmn}, {data, Customer}) ->
    Rsubject = "Ваш аккаунт удален модератором",
    Xslb_path = "xsl/mail/customer/del-insider.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

del_insider({customer, Rmn}, {data, Customer}) ->
    Rsubject = "Ваш аккаунт удален модератором",
    Xslb_path = "xsl/mail/customer/del-insider.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody).

%%% --------------------------------------------------------------------------

%%%
%%% @doc
%%%    Отправляет почту
%%%    получателям системных сообщений,
%%%    если счет выставлен
%%%
mkbill({sysmsg, Rmn}, {data, Acv_video}) ->
    Rsubject = "Выставлен счет",
    Xslb_path = "xsl/mail/customer/mkbill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",    meta([Rmn])},
            {"video",   Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

%%%
%%% @doc
%%%    Отправляет почту пользователю,
%%%    если счет выставлен
%%%
mkbill({customer, Rmn}, {data, Acv_video}) ->
    Rsubject = "Выставлен счет",
    Xslb_path = "xsl/mail/customer/mkbill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",    meta([Rmn])},
            {"video",   Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody).

%%% --------------------------------------------------------------------------

%%%
%%% @doc
%%%    Отправляет почту
%%%    получателям системных сообщений,
%%%    если счет оплачен
%%%
paybill({sysmsg, Rmn}, {data, Acv_video}) ->
    Rsubject = "Оплачен счет на видео ",
    Xslb_path = "xsl/mail/customer/paybill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",    meta([Rmn])},
            {"video",   Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

%%%
%%% @doc
%%%    Отправляет почту пользователю,
%%%    если счет оплачен
%%%
paybill({customer, Rmn}, {data, Acv_video}) ->
    Rsubject = "Ваш счет оплачен",
    Xslb_path = "xsl/mail/customer/paybill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",    meta([Rmn])},
            {"video",   Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody).

%%% --------------------------------------------------------------------------


test()->
    ok.

test(speed) ->
    ok.