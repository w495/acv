create sequence seq_banner_place;
create table banner_place (
    id int primary key default nextval('seq_banner_place'),
    name varchar(30) unique,
    alias varchar(100)
);


create sequence seq_adv_com;
create table adv_com (
    id int primary key default nextval('seq_adv_com'),
    name varchar(100),
    datestart timestamp without time zone,
    datestop timestamp without time zone,
    banner_place_id int references banner_place(id),
    url varchar(200),
    ref varchar(1000)
);

create sequence seq_adv_com_vid;
create table adv_com_vid (
    id int primary key default nextval('seq_adv_com'),
    name varchar(100),
    datestart timestamp without time zone,
    datestop timestamp without time zone,
    url varchar(200),
    ref varchar(1000)
);



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
    birthday date,
    password_hash char(32) not null
);

create sequence seq_customer_group_id;
create table customer_group (
    id int primary key default nextval('seq_customer_group_id'),
    name varchar(1024),
    description varchar(1024),
    deleted bool default false
);

create sequence seq_permission_type;
create table permission_type (
    id int primary key default nextval('seq_permission_type'),
    name varchar(1024) unique
);

create sequence seq_permission;
create table permission (
    id int primary key default nextval('seq_permission'),
    perm_type_id int references permission_type(id),
    entity_id int,
    name varchar(1024),
    description varchar(1024),
    type int
);

create table permission2group (
    perm_id int references permission(id) not null,
    group_id int references customer_group(id) not null
);

create table customer2group (
    customer_id int references customer(id) not null,
    group_id int references customer_group(id) not null
);


