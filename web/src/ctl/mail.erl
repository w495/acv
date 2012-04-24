%%% @file mail.erl
%%%
%%%     Контроллеры, отправки почты
%%%

-module(mail).

-export([
    % -----------------------
    create_customer/2,
    add_insider/2,
    del_insider/2,
    delete_customer/2,
    % -----------------------
    create_acv_video/2,
    mkbill/2,
    paybill/2,
    delete_acv_video/2,
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
    Rsubject = "Появился новый пользователь №" ++
        convert:to_list(proplists:get_value("id", Customer)),
    Xslb_path = "xsl/mail/sysmsg/customer/create.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    ?D("Customer = ~p", [Customer]),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

create_customer({customer, Rmn}, {data, Customer}) ->
    Rsubject = "Спасибо за регистрацию",
    Xslb_path = "xsl/mail/customer/customer/create.xsl",
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
    Rsubject = "Был удален пользователь №" ++ 
        convert:to_list(proplists:get_value("id", Customer)),
    Xslb_path = "xsl/mail/sysmsg/customer/delete.xsl",
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
    Xslb_path = "xsl/mail/customer/customer/delete.xsl",
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
    Rsubject = "Был одобрен пользователь №" ++
        convert:to_list(proplists:get_value("id", Customer)),
    Xslb_path = "xsl/mail/sysmsg/customer/add-insider.xsl",
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
    Xslb_path = "xsl/mail/customer/customer/add-insider.xsl",
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
    Rsubject = "Был отклонен пользователь №" ++ 
        convert:to_list(proplists:get_value("id", Customer)),
    Xslb_path = "xsl/mail/sysmsg/customer/del-insider.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

del_insider({customer, Rmn}, {data, Customer}) ->
    Rsubject = "Ваш аккаунт отклонен модератором",
    Xslb_path = "xsl/mail/customer/customer/del-insider.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"сustomer",    Customer}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody).


%%% --------------------------------------------------------------------------
%%% --------------------------------------------------------------------------
%%% --------------------------------------------------------------------------

create_acv_video({sysmsg, Rmn}, {data, Acv_video}) ->
    Rsubject = "Была создана новая кампания №" ++
        convert:to_list(proplists:get_value("id", Acv_video)),

    ?D("~nAcv_video = ~p~n", [Acv_video]),

    Xslb_path = "xsl/mail/sysmsg/acv-video/create.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"acv-video",       Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

create_acv_video({customer, Rmn}, {data, Acv_video}) ->
    Rsubject = "Вы создали кампанию", 
    Xslb_path = "xsl/mail/customer/acv-video/create.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"acv-video",       Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody).

%%% --------------------------------------------------------------------------

delete_acv_video({sysmsg, Rmn}, {data, Acv_video}) ->
    Rsubject = "Была удалена кампания №" ++
        convert:to_list(proplists:get_value("id", Acv_video)),
    Xslb_path = "xsl/mail/sysmsg/acv-video/delete.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"acv-video",       Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody);

delete_acv_video({customer, Rmn}, {data, Acv_video}) ->
    Rsubject = "Ваша кампания была удалена",
    Xslb_path = "xsl/mail/customer/acv-video/delete.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",        meta([Rmn])},
            {"acv-video",       Acv_video}
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
    Rsubject = "Выставлен счет для кампании №" ++
        convert:to_list(proplists:get_value("id", Acv_video)),
    Xslb_path = "xsl/mail/sysmsg/acv-video/mkbill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",    meta([Rmn])},
            {"acv-video",   Acv_video}
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
    Xslb_path = "xsl/mail/customer/acv-video/mkbill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",    meta([Rmn])},
            {"acv-video",   Acv_video}
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
    Xslb_path = "xsl/mail/sysmsg/acv-video/paybill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",    meta([Rmn])},
            {"acv-video",   Acv_video}
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
    Xslb_path = "xsl/mail/customer/acv-video/paybill.xsl",
    Xmlb  = xml:encode_data(
        [
            {"meta",    meta([Rmn])},
            {"acv-video",   Acv_video}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),
    mail_utils:mail(Rmn, Rsubject, Rbody).

%%% --------------------------------------------------------------------------


test()->
    ok.

test(speed) ->
    ok.