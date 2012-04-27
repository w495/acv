/***********************************************************************
 *
 * \file Описание схемы базы данных
 *
***********************************************************************/


-------------------------------------------------------------------------------
-- ЛЮДИ 
-------------------------------------------------------------------------------

/**
 * Рекламодатель
**/
create sequence seq_customer_id;
create table customer(
    id int primary key default nextval('seq_customer_id'),
    email varchar(1024) unique,
    login varchar(1024) not null unique,

    /*
        Картинка для пользователя
        Возможно в последствии, нужно будет загружать сан паспотр
    */
    pic_url text,
    city            varchar(1024),
    organization    varchar(1024),
    position        varchar(1024),
    firstname       varchar(1024) not null,
    lastname        varchar(1024) not null,
    patronimic      varchar(1024) not null,

    /*
        условие удаления
    */
    deleted bool default false,

    birthday date,
    telephone_number varchar(30),
    password_hash char(32) not null
);


/*
    Кошелек
*/
create sequence seq_purse_id;
create table purse(
    id int primary key default nextval('seq_purse_id')

);

/*
    Счет
*/
create sequence seq_bill_id;
create table bill(
    id int primary key default nextval('seq_bill_id')

);

/**
 * Группа рекламодателей
**/
create sequence seq_customer_group_id;
create table customer_group (
    id int primary key default nextval('seq_customer_group_id'),
    name varchar(1024),
    description varchar(1024),

    /*
        условие исключтельности
            false   --- обычная группа
            true    --- системная группа
                должна оставаться как минимум одна системная группа,
                которую нельзя удалить ни при каком условии
        На данный момнет поле не используется,
        но может оказаться полезным.
    */
    issystem bool default false,

    deleted bool default false
);

/**
 * Типы прав
**/
create sequence seq_permission_type;
create table permission_type (
    id      int primary key default nextval('seq_permission_type'),
    name    varchar(1024) unique
);

/**
 * Типы cущностей прав
**/
create sequence seq_permission_entity_type;
create table permission_entity_type (
    id      int primary key default nextval('seq_permission_entity_type'),
    name    varchar(1024) unique
);


/**
 * Права
**/
create sequence seq_permission;
create table permission (
    id              int primary key default nextval('seq_permission'),
    perm_type_id    int references permission_type(id),
    entity_type_id  int references permission_entity_type(id),
    entity_id       int,
    name            varchar(1024),
    description     varchar(1024),
    type            int
);

-------------------------------------------------------------------------------
-- НАСТРОЙКИ
-------------------------------------------------------------------------------

create sequence seq_sysvar_type_id;
create table sysvar_type(
    id int primary key default nextval('seq_sysvar_type_id'),
    name varchar(128) not null unique
);


/**
 * Таблица системных переменных
**/
create sequence seq_sysvar_id;
create table sysvar(
    id int primary key default nextval('seq_sysvar_id'),
    name varchar(128) not null unique,
    value varchar(1024) not null,
    description varchar(1024) default null,
    perm_id int references permission(id) default null,
    type_id int references sysvar_type(id) default null
);


-------------------------------------------------------------------------------
-- РЕКЛАМА
-------------------------------------------------------------------------------

/**
 * Размещение баннеров
**/
create sequence seq_banner_place;
create table banner_place (
    id int primary key default nextval('seq_banner_place'),
    name varchar(30) unique,
    alias varchar(100)
);

/**
 * Рекламная компания для баннеров
**/
create sequence seq_acv_banner;
create table acv_banner (
    id int primary key default nextval('seq_acv_banner'),
    name varchar(100),
    datestart timestamp without time zone,
    datestop timestamp without time zone,
    url varchar(200),
    ref varchar(1024),
    banner_place_id int references banner_place(id),
    customer_id int references customer(id)


);

/**
 * Рекламная компания для видео
**/
create sequence seq_acv_video;
create table acv_video (
    id          int primary key default nextval('seq_acv_video'),
    name        varchar(100),

    datestart   timestamp without time zone,
    datestop    timestamp without time zone,
    url         varchar(200),
    ref         varchar(1024),
    /*
        желаемое колическтво показов
    */
    wish        int,
    postroll    bool default true,
    preroll     bool default true,
    midroll     bool default true,
    pauseroll   bool default true,
    user_male   bool default null,
    age_from    int default null,
    age_to      int default null,

    /*
        часы
    */
    time_from   int default null,

    /*
        часы
    */
    time_to     int default null,

    /*
        продолжительность в секундах
    */
    duration    int default 0,
    /*
        заголовок ссылки
    */
    link_title  varchar(200) default null,  
    alt_title   varchar(200) default null,  

    /*
        разрешенный повторный показ
    */
    rerun_hours     int default null,

    /*
        разрешенный повторный показ
    */
    rerun_minutes   int default null,

    /*
        коментарий
            пока в нем нет необходимости, но рано или поздно его попросят
    */
    comment         varchar(200) default null,

    ------------------------------------------------------------------------
    -- ПОЛЯ НЕ ДЛЯ ВСЕХ
    ------------------------------------------------------------------------

    /*
        количество показов
    */
    shown   int default 0,

    /*
        количество кликов
    */
    clicks int default 0,

    /*
        условие предмодерации
            null    --- кампания не просмотрена
            false   --- кампания отклонена
            true    --- кампания разрешена
    */
    active  bool default null,

    /*
        условие удаления
    */
    deleted bool default false,

    /*
        условие остановки
    */
    stoped  bool default false,

    /*
        статус оплаты
            null    --- счет еще не выставлен
            false   --- счет выставлен, но не оплачен
            true    --- счет выставлен и оплачен
    */
    pay_status bool default null,

    /*
        сумма оплаты
            Если не оплачен, то null
    */
    sum    int default null,

    /*
        --> customer
    */
    customer_id int references customer(id)


);

/**
 * Район, наприер СНГ
**/
create sequence seq_geo_area_id;
create table geo_area (
    id int primary key default nextval('seq_geo_area_id'),
    name_ru character varying(100),
    name_en character varying(100)
);


/**
 * Регион
**/
create table geo_region (
    id integer not null primary key,
    country_id integer,
    name_ru character varying(100),
    name_en character varying(100),
    code character varying(2),
    geo_area_id int references geo_area(id)
);


/**
 * Что какому пользователю показали и когда
**/
create sequence seq_acv_video_shown;
create table acv_video_shown (
    id int primary key default nextval('seq_acv_video_shown'),
    dateshow timestamp without time zone,
    user_id varchar(37), -- 5615d6c0-79c3-4a90-bf34-9d23ae78c14a
    acv_video_id int references acv_video(id)
);

-------------------------------------------------------------------------------
-- СВЯЗКИ МНОГИЕ КО МНОГИМ
-------------------------------------------------------------------------------

/**
 * Многие ко многим для прав и групп
**/
create table permission2group (
    perm_id int references permission(id) not null,
    group_id int references customer_group(id) not null
);

/**
 * Многие ко многим для пользователей и групп
**/
create table customer2group (
    customer_id int references customer(id) not null,
    group_id int references customer_group(id) not null
);

/**
 * Многие ко многим для acv_video x geo_region
**/
--create sequence seq_acv_video_2_geo_region;
create table acv_video2geo_region (
    -- id int primary key default nextval('seq_acv_video_2_geo_region'),
    acv_video_id int references acv_video(id),
    geo_region_id int references geo_region(id)
);

/**
 * Многие ко многим для acv_video x cat
 *  cat_id берется из основной базы данных
**/
--create sequence seq_acv_video_2_cat;
create table acv_video2cat (
    -- id int primary key default nextval('seq_acv_video_2_cat'),
    acv_video_id int references acv_video(id),
    cat_id int
);




