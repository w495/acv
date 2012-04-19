/***********************************************************************
 *
 * \file Описание схемы базы данных
 *
***********************************************************************/

/**
 * Таблица конфигурации с одной сущностью.
 * Мы не дедаем table -> (name, value);
 *  Т.к. value не понятно какого типа.
**/

create table config(
    id int primary key default 0,
    acv_video_loadnext int /* load next */
);


/* **************************************************************************

    Изначально была идея хранить все в одной таблице,
    настроек и config и var, но проще иметь 2 разных таблицы

    create sequence seq_config_id;
    create table config(
        id int primary key default nextval('seq_config_id'),
        isvar bool primary key default true,
        -- type varchar(32) not null,
        --     -- int, float, char, varchar, и пр
        name varchar(128) not null unique,
        value varchar(1024) not null
    );

************************************************************************** */

/**
 * Таблица переменных значений с одной сущностью.
 * Мы не дедаем table -> (name, value);
 *  Т.к. value не понятно какого типа
**/

create table var(
    id bool primary key default true,
    av_stats_max_id numeric(20)
);


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
    pic_url text, /* заглавная картинка для новости */
    city varchar(1024),
    organization varchar(1024),
    position varchar(1024),
    firstname varchar(1024) not null,
    lastname varchar(1024) not null,
    patronimic varchar(1024) not null,
    deleted bool default false,
    issystem bool default false,
    birthday date,
    password_hash char(32) not null,
    telephone_number character varying(30);
);

/**
 * Группа рекламодателей
**/
create sequence seq_customer_group_id;
create table customer_group (
    id int primary key default nextval('seq_customer_group_id'),
    name varchar(1024),
    issystem bool default false,
    description varchar(1024),
    deleted bool default false
);

/**
 * Типы прав
**/
create sequence seq_permission_type;
create table permission_type (
    id int primary key default nextval('seq_permission_type'),
    issystem bool default false,
    name varchar(1024) unique
);

/**
 * Права
**/
create sequence seq_permission;
create table permission (
    id int primary key default nextval('seq_permission'),
    perm_type_id int references permission_type(id),
    entity_id int,
    name varchar(1024),
    description varchar(1024),
    type int
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
    ref varchar(1000),
    banner_place_id int references banner_place(id),
    customer_id int references customer(id)
        /* зачем пользователю знать про чужие баннеры */

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
    ref         varchar(1000),
    wish        int,   /* желаемое колическтво показов */
    postroll    bool default true,
    preroll     bool default true,
    midroll     bool default true,
    pauseroll   bool default true,
    user_male   bool default null,
    age_from    int default null,
    age_to      int default null,
    time_from   int default null,           /* часы */
    time_to     int default null,           /* часы */
    duration    int default 0,              /* продолжительность в секундах */
    link_title  varchar(200) default null,  /* заголовок ссылки */
    alt_title   varchar(200) default null,  /*  */

    rerun_hours     int default null,          /* разрешенный повторный показ */
    rerun_minutes   int default null,         /* разрешенный повторный показ */

    comment         varchar(200) default null,
        /* пока в нем нет необходимости, но рано или поздно его попросят */


    /* ПОЛЯ НЕ ДЛЯ ВСЕХ */

    shown   int default 0,          /* количество показов */
    clicks int default 0,            /* количество кликов */
    active  bool default null,      /* условие предмодерации */
    deleted bool default false,     /* удаление */
    stoped  bool default false,     /* остановка */

    pay_status bool default null,     /* статус оплаты */
    sum    int default null,          /* сумма оплаты  */

    customer_id int references customer(id)
        /* зачем пользователю знать про чужие баннеры */

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




