--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

--
-- Name: seq_acv_banner; Type: SEQUENCE; Schema: public; Owner: w-495
--

CREATE SEQUENCE seq_acv_banner
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.seq_acv_banner OWNER TO "w-495";

--
-- Name: seq_acv_banner; Type: SEQUENCE SET; Schema: public; Owner: w-495
--

SELECT pg_catalog.setval('seq_acv_banner', 59, true);


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: acv_banner; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE acv_banner (
    id integer DEFAULT nextval('seq_acv_banner'::regclass) NOT NULL,
    customer_id integer,
    name character varying(100),
    datestart timestamp without time zone,
    datestop timestamp without time zone,
    banner_place_id integer,
    url character varying(200),
    ref character varying(1000)
);


ALTER TABLE public.acv_banner OWNER TO "w-495";

--
-- Name: seq_acv_video; Type: SEQUENCE; Schema: public; Owner: w-495
--

CREATE SEQUENCE seq_acv_video
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.seq_acv_video OWNER TO "w-495";

--
-- Name: seq_acv_video; Type: SEQUENCE SET; Schema: public; Owner: w-495
--

SELECT pg_catalog.setval('seq_acv_video', 39, true);


--
-- Name: acv_video; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE acv_video (
    id integer DEFAULT nextval('seq_acv_video'::regclass) NOT NULL,
    customer_id integer,
    name character varying(100),
    datestart timestamp without time zone,
    datestop timestamp without time zone,
    url character varying(200),
    ref character varying(1000),
    wish integer,
    postroll boolean DEFAULT true,
    preroll boolean DEFAULT true,
    midroll boolean DEFAULT true,
    pauseroll boolean DEFAULT true,
    user_male boolean,
    age_from integer,
    age_to integer
);


ALTER TABLE public.acv_video OWNER TO "w-495";

--
-- Name: acv_video2cat; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE acv_video2cat (
    acv_video_id integer,
    cat_id integer
);


ALTER TABLE public.acv_video2cat OWNER TO "w-495";

--
-- Name: acv_video2geo_region; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE acv_video2geo_region (
    acv_video_id integer,
    geo_region_id integer
);


ALTER TABLE public.acv_video2geo_region OWNER TO "w-495";

--
-- Name: seq_banner_place; Type: SEQUENCE; Schema: public; Owner: w-495
--

CREATE SEQUENCE seq_banner_place
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.seq_banner_place OWNER TO "w-495";

--
-- Name: seq_banner_place; Type: SEQUENCE SET; Schema: public; Owner: w-495
--

SELECT pg_catalog.setval('seq_banner_place', 2, true);


--
-- Name: banner_place; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE banner_place (
    id integer DEFAULT nextval('seq_banner_place'::regclass) NOT NULL,
    name character varying(30),
    alias character varying(100)
);


ALTER TABLE public.banner_place OWNER TO "w-495";

--
-- Name: seq_customer_id; Type: SEQUENCE; Schema: public; Owner: w-495
--

CREATE SEQUENCE seq_customer_id
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.seq_customer_id OWNER TO "w-495";

--
-- Name: seq_customer_id; Type: SEQUENCE SET; Schema: public; Owner: w-495
--

SELECT pg_catalog.setval('seq_customer_id', 7, true);


--
-- Name: customer; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE customer (
    id integer DEFAULT nextval('seq_customer_id'::regclass) NOT NULL,
    email character varying(1024),
    login character varying(1024) NOT NULL,
    pic_url text,
    city character varying(1024),
    organization character varying(1024),
    "position" character varying(1024),
    firstname character varying(1024) NOT NULL,
    lastname character varying(1024) NOT NULL,
    patronimic character varying(1024) NOT NULL,
    deleted boolean DEFAULT false,
    birthday date,
    password_hash character(32) NOT NULL
);


ALTER TABLE public.customer OWNER TO "w-495";

--
-- Name: customer2group; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE customer2group (
    customer_id integer NOT NULL,
    group_id integer NOT NULL
);


ALTER TABLE public.customer2group OWNER TO "w-495";

--
-- Name: seq_customer_group_id; Type: SEQUENCE; Schema: public; Owner: w-495
--

CREATE SEQUENCE seq_customer_group_id
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.seq_customer_group_id OWNER TO "w-495";

--
-- Name: seq_customer_group_id; Type: SEQUENCE SET; Schema: public; Owner: w-495
--

SELECT pg_catalog.setval('seq_customer_group_id', 2, true);


--
-- Name: customer_group; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE customer_group (
    id integer DEFAULT nextval('seq_customer_group_id'::regclass) NOT NULL,
    name character varying(1024),
    description character varying(1024),
    deleted boolean DEFAULT false
);


ALTER TABLE public.customer_group OWNER TO "w-495";

--
-- Name: seq_geo_region; Type: SEQUENCE; Schema: public; Owner: w-495
--

CREATE SEQUENCE seq_geo_region
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.seq_geo_region OWNER TO "w-495";

--
-- Name: seq_geo_region; Type: SEQUENCE SET; Schema: public; Owner: w-495
--

SELECT pg_catalog.setval('seq_geo_region', 1, true);


--
-- Name: geo_region; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE geo_region (
    id integer DEFAULT nextval('seq_geo_region'::regclass) NOT NULL,
    alias character varying(30),
    name character varying(100)
);


ALTER TABLE public.geo_region OWNER TO "w-495";

--
-- Name: seq_permission; Type: SEQUENCE; Schema: public; Owner: w-495
--

CREATE SEQUENCE seq_permission
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.seq_permission OWNER TO "w-495";

--
-- Name: seq_permission; Type: SEQUENCE SET; Schema: public; Owner: w-495
--

SELECT pg_catalog.setval('seq_permission', 1, true);


--
-- Name: permission; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE permission (
    id integer DEFAULT nextval('seq_permission'::regclass) NOT NULL,
    perm_type_id integer,
    entity_id integer,
    name character varying(1024),
    description character varying(1024),
    type integer
);


ALTER TABLE public.permission OWNER TO "w-495";

--
-- Name: permission2group; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE permission2group (
    perm_id integer NOT NULL,
    group_id integer NOT NULL
);


ALTER TABLE public.permission2group OWNER TO "w-495";

--
-- Name: seq_permission_type; Type: SEQUENCE; Schema: public; Owner: w-495
--

CREATE SEQUENCE seq_permission_type
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.seq_permission_type OWNER TO "w-495";

--
-- Name: seq_permission_type; Type: SEQUENCE SET; Schema: public; Owner: w-495
--

SELECT pg_catalog.setval('seq_permission_type', 1, true);


--
-- Name: permission_type; Type: TABLE; Schema: public; Owner: w-495; Tablespace: 
--

CREATE TABLE permission_type (
    id integer DEFAULT nextval('seq_permission_type'::regclass) NOT NULL,
    name character varying(1024)
);


ALTER TABLE public.permission_type OWNER TO "w-495";

--
-- Data for Name: acv_banner; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY acv_banner (id, customer_id, name, datestart, datestop, banner_place_id, url, ref) FROM stdin;
44	1	some name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	1	url	ref
45	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
48	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
49	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
50	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
51	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
52	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
53	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
54	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
55	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
56	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
57	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
58	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
59	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	2	url	ref
\.


--
-- Data for Name: acv_video; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY acv_video (id, customer_id, name, datestart, datestop, url, ref, wish, postroll, preroll, midroll, pauseroll, user_male, age_from, age_to) FROM stdin;
29	\N	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	url	ref	1	t	t	t	t	t	1	2
30	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	url	ref	1	t	t	t	t	t	1	2
36	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	url	ref	1	t	t	t	t	t	1	2
37	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	url	ref	1	t	t	t	t	t	1	2
38	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	url	ref	1	t	t	t	t	t	1	2
39	1	some new name	10865-03-14 16:25:39.1	10855-06-04 07:16:30	url	ref	1	t	t	t	t	t	1	2
\.


--
-- Data for Name: acv_video2cat; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY acv_video2cat (acv_video_id, cat_id) FROM stdin;
\.


--
-- Data for Name: acv_video2geo_region; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY acv_video2geo_region (acv_video_id, geo_region_id) FROM stdin;
\.


--
-- Data for Name: banner_place; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY banner_place (id, name, alias) FROM stdin;
1	right	справа
2	bottom	снизу
\.


--
-- Data for Name: customer; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY customer (id, email, login, pic_url, city, organization, "position", firstname, lastname, patronimic, deleted, birthday, password_hash) FROM stdin;
1	\N	admin	\N	\N	\N	\N	fadmin	ladmin	padmin	f	\N	21232F297A57A5A743894A0E4A801FC3
2	sdsd	sds	null	sdsd	sdsd	sdsd	sdsd	sdsd	sdsds	f	\N	CC2BD8F09BB88B5DD20F9B432631B8CA
6	asas	asas	null	ASS	ASAS	SASAS	ASASA	ASAS	SASSASA	f	\N	BAA7A52965B99778F38EF37F235E9053
7	lamer	lamer	null	lamer	lamer	lamer	lamer	lamer	lamer	f	\N	3E3A378C63AA1E55E3E9AE9D2BDCD6A1
\.


--
-- Data for Name: customer2group; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY customer2group (customer_id, group_id) FROM stdin;
1	1
\.


--
-- Data for Name: customer_group; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY customer_group (id, name, description, deleted) FROM stdin;
1	admin	администраторы	f
2	SSS	SSS	f
\.


--
-- Data for Name: geo_region; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY geo_region (id, alias, name) FROM stdin;
1	world	весь мир
\.


--
-- Data for Name: permission; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY permission (id, perm_type_id, entity_id, name, description, type) FROM stdin;
1	1	\N	admin	полный доступ	\N
\.


--
-- Data for Name: permission2group; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY permission2group (perm_id, group_id) FROM stdin;
1	1
\.


--
-- Data for Name: permission_type; Type: TABLE DATA; Schema: public; Owner: w-495
--

COPY permission_type (id, name) FROM stdin;
1	static
\.


--
-- Name: acv_banner_pkey; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY acv_banner
    ADD CONSTRAINT acv_banner_pkey PRIMARY KEY (id);


--
-- Name: acv_video_pkey; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY acv_video
    ADD CONSTRAINT acv_video_pkey PRIMARY KEY (id);


--
-- Name: banner_place_name_key; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY banner_place
    ADD CONSTRAINT banner_place_name_key UNIQUE (name);


--
-- Name: banner_place_pkey; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY banner_place
    ADD CONSTRAINT banner_place_pkey PRIMARY KEY (id);


--
-- Name: customer_email_key; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY customer
    ADD CONSTRAINT customer_email_key UNIQUE (email);


--
-- Name: customer_group_pkey; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY customer_group
    ADD CONSTRAINT customer_group_pkey PRIMARY KEY (id);


--
-- Name: customer_login_key; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY customer
    ADD CONSTRAINT customer_login_key UNIQUE (login);


--
-- Name: customer_pkey; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY customer
    ADD CONSTRAINT customer_pkey PRIMARY KEY (id);


--
-- Name: geo_region_alias_key; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY geo_region
    ADD CONSTRAINT geo_region_alias_key UNIQUE (alias);


--
-- Name: geo_region_pkey; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY geo_region
    ADD CONSTRAINT geo_region_pkey PRIMARY KEY (id);


--
-- Name: permission_pkey; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY permission
    ADD CONSTRAINT permission_pkey PRIMARY KEY (id);


--
-- Name: permission_type_name_key; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY permission_type
    ADD CONSTRAINT permission_type_name_key UNIQUE (name);


--
-- Name: permission_type_pkey; Type: CONSTRAINT; Schema: public; Owner: w-495; Tablespace: 
--

ALTER TABLE ONLY permission_type
    ADD CONSTRAINT permission_type_pkey PRIMARY KEY (id);


--
-- Name: acv_banner_banner_place_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY acv_banner
    ADD CONSTRAINT acv_banner_banner_place_id_fkey FOREIGN KEY (banner_place_id) REFERENCES banner_place(id);


--
-- Name: acv_banner_customer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY acv_banner
    ADD CONSTRAINT acv_banner_customer_id_fkey FOREIGN KEY (customer_id) REFERENCES customer(id);


--
-- Name: acv_video2cat_acv_video_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY acv_video2cat
    ADD CONSTRAINT acv_video2cat_acv_video_id_fkey FOREIGN KEY (acv_video_id) REFERENCES acv_video(id);


--
-- Name: acv_video2geo_region_acv_video_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY acv_video2geo_region
    ADD CONSTRAINT acv_video2geo_region_acv_video_id_fkey FOREIGN KEY (acv_video_id) REFERENCES acv_video(id);


--
-- Name: acv_video2geo_region_geo_region_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY acv_video2geo_region
    ADD CONSTRAINT acv_video2geo_region_geo_region_id_fkey FOREIGN KEY (geo_region_id) REFERENCES geo_region(id);


--
-- Name: acv_video_customer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY acv_video
    ADD CONSTRAINT acv_video_customer_id_fkey FOREIGN KEY (customer_id) REFERENCES customer(id);


--
-- Name: customer2group_customer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY customer2group
    ADD CONSTRAINT customer2group_customer_id_fkey FOREIGN KEY (customer_id) REFERENCES customer(id);


--
-- Name: customer2group_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY customer2group
    ADD CONSTRAINT customer2group_group_id_fkey FOREIGN KEY (group_id) REFERENCES customer_group(id);


--
-- Name: permission2group_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY permission2group
    ADD CONSTRAINT permission2group_group_id_fkey FOREIGN KEY (group_id) REFERENCES customer_group(id);


--
-- Name: permission2group_perm_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY permission2group
    ADD CONSTRAINT permission2group_perm_id_fkey FOREIGN KEY (perm_id) REFERENCES permission(id);


--
-- Name: permission_perm_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: w-495
--

ALTER TABLE ONLY permission
    ADD CONSTRAINT permission_perm_type_id_fkey FOREIGN KEY (perm_type_id) REFERENCES permission_type(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

