--
-- PostgreSQL database dump
--

-- Dumped from database version 13.3
-- Dumped by pg_dump version 13.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: channels; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.channels (
    server_id text NOT NULL,
    channel_id text NOT NULL,
    codex integer,
    charsigns text[],
    charseps text[],
    inline boolean,
    ephemeral boolean,
    freeze_on_timeout boolean
);


ALTER TABLE public.channels OWNER TO cwstra;

--
-- Name: character_attributes; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.character_attributes (
    character_id integer NOT NULL,
    name text NOT NULL,
    value text NOT NULL
);


ALTER TABLE public.character_attributes OWNER TO cwstra;

--
-- Name: characters; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.characters (
    character_id integer NOT NULL,
    server_id text NOT NULL,
    member_id text NOT NULL,
    character_name text NOT NULL
);


ALTER TABLE public.characters OWNER TO cwstra;

--
-- Name: characters_character_id_seq; Type: SEQUENCE; Schema: public; Owner: cwstra
--

CREATE SEQUENCE public.characters_character_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.characters_character_id_seq OWNER TO cwstra;

--
-- Name: characters_character_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: cwstra
--

ALTER SEQUENCE public.characters_character_id_seq OWNED BY public.characters.character_id;


--
-- Name: codex_list; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.codex_list (
    id integer NOT NULL,
    prefix text not null unique,
    display_name text NOT NULL
);


ALTER TABLE public.codex_list OWNER TO cwstra;

--
-- Name: codex_list_id_seq; Type: SEQUENCE; Schema: public; Owner: cwstra
--

CREATE SEQUENCE public.codex_list_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.codex_list_id_seq OWNER TO cwstra;

--
-- Name: codex_list_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: cwstra
--

ALTER SEQUENCE public.codex_list_id_seq OWNED BY public.codex_list.id;


--
-- Name: server_mod_roles; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.server_mod_roles (
    server_id text NOT NULL,
    role_id text NOT NULL
);


ALTER TABLE public.server_mod_roles OWNER TO cwstra;

--
-- Name: servers; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.servers (
    server_id text NOT NULL,
    codex integer,
    charsigns text[],
    charseps text[],
    inline boolean,
    ephemeral boolean,
    freeze_on_timeout boolean
);


ALTER TABLE public.servers OWNER TO cwstra;

--
-- Name: characters character_id; Type: DEFAULT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.characters ALTER COLUMN character_id SET DEFAULT nextval('public.characters_character_id_seq'::regclass);


--
-- Name: codex_list id; Type: DEFAULT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.codex_list ALTER COLUMN id SET DEFAULT nextval('public.codex_list_id_seq'::regclass);


--
-- Data for Name: channels; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.channels (server_id, channel_id, codex, charsigns, charseps, inline, ephemeral, freeze_on_timeout) FROM stdin;
466825566566481930	881667322488713277	2	{$}	{:}	f	f	f
\.


--
-- Data for Name: character_attributes; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.character_attributes (character_id, name, value) FROM stdin;
\.


--
-- Data for Name: characters; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.characters (character_id, server_id, member_id, character_name) FROM stdin;
\.


--
-- Data for Name: codex_list; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.codex_list (id, prefix, display_name) FROM stdin;
1	dnd_5e	Dungeons and Dragons 5e SRD
\.


--
-- Data for Name: server_mod_roles; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.server_mod_roles (server_id, role_id) FROM stdin;
\.


--
-- Data for Name: servers; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.servers (server_id, codex, charsigns, charseps, inline, ephemeral, freeze_on_timeout) FROM stdin;
466825566566481930	7	{$}	{:}	f	t	f
\.


--
-- Name: characters_character_id_seq; Type: SEQUENCE SET; Schema: public; Owner: cwstra
--

SELECT pg_catalog.setval('public.characters_character_id_seq', 1, true);


--
-- Name: codex_list_id_seq; Type: SEQUENCE SET; Schema: public; Owner: cwstra
--

SELECT pg_catalog.setval('public.codex_list_id_seq', 8, true);


--
-- Name: channels channels_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT channels_pkey PRIMARY KEY (server_id, channel_id);


--
-- Name: character_attributes character_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.character_attributes
    ADD CONSTRAINT character_attributes_pkey PRIMARY KEY (character_id, name);


--
-- Name: characters characters_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.characters
    ADD CONSTRAINT characters_pkey PRIMARY KEY (character_id);


--
-- Name: characters characters_server_id_character_name_key; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.characters
    ADD CONSTRAINT characters_server_id_character_name_key UNIQUE (server_id, character_name);


--
-- Name: codex_list codex_list_display_name_key; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.codex_list
    ADD CONSTRAINT codex_list_display_name_key UNIQUE (display_name);


--
-- Name: codex_list codex_list_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.codex_list
    ADD CONSTRAINT codex_list_pkey PRIMARY KEY (id);


--
-- Name: server_mod_roles server_mod_roles_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.server_mod_roles
    ADD CONSTRAINT server_mod_roles_pkey PRIMARY KEY (server_id, role_id);


--
-- Name: servers servers_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.servers
    ADD CONSTRAINT servers_pkey PRIMARY KEY (server_id);


--
-- Name: character_attributes fk_character; Type: FK CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.character_attributes
    ADD CONSTRAINT fk_character FOREIGN KEY (character_id) REFERENCES public.characters(character_id);


--
-- Name: TABLE channels; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.channels TO bot;


--
-- Name: TABLE character_attributes; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.character_attributes TO bot;


--
-- Name: TABLE characters; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.characters TO bot;


--
-- Name: SEQUENCE characters_character_id_seq; Type: ACL; Schema: public; Owner: cwstra
--

GRANT SELECT,USAGE ON SEQUENCE public.characters_character_id_seq TO bot;


--
-- Name: TABLE codex_list; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.codex_list TO bot;


--
-- Name: TABLE server_mod_roles; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.server_mod_roles TO bot;


--
-- Name: TABLE servers; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.servers TO bot;


--
-- PostgreSQL database dump complete
--

