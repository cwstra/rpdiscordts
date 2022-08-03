--
-- PostgreSQL database dump
--

-- Dumped from database version 13.5
-- Dumped by pg_dump version 13.5

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
    prefix text NOT NULL,
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
-- Name: command_usage; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.command_usage (
    name text NOT NULL,
    uses bigint NOT NULL
);


ALTER TABLE public.command_usage OWNER TO cwstra;

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
-- Name: unique_servers; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.unique_servers (
    server_id text NOT NULL
);


ALTER TABLE public.unique_servers OWNER TO cwstra;

--
-- Name: unique_users; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.unique_users (
    user_id text NOT NULL
);


ALTER TABLE public.unique_users OWNER TO cwstra;

--
-- Name: characters character_id; Type: DEFAULT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.characters ALTER COLUMN character_id SET DEFAULT nextval('public.characters_character_id_seq'::regclass);


--
-- Name: codex_list id; Type: DEFAULT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.codex_list ALTER COLUMN id SET DEFAULT nextval('public.codex_list_id_seq'::regclass);

--
-- Data for Name: codex_list; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.codex_list (id, prefix, display_name) FROM stdin;
1	dnd_5e	Dungeons and Dragons 5e SRD
9	pta_03	PTA 3
10	ptu_05	PTU 1.05
11	ptu_pt	PTU 1.05 (With Playtest Packets)
12	ptu_al	PTU 1.05 (With Playtest + Alola)
13	ptu_ga	PTU 1.05 (With Playtest + Alola + Galar)
14	ptu_hi	PTU 1.05 (With Playtest + Alola + Galar + Hisui)
15	zelda_rw	The Legend of Zelda: Reclaim the Wild 1.03
\.


--
-- Name: characters_character_id_seq; Type: SEQUENCE SET; Schema: public; Owner: cwstra
--

SELECT pg_catalog.setval('public.characters_character_id_seq', 37, true);


--
-- Name: codex_list_id_seq; Type: SEQUENCE SET; Schema: public; Owner: cwstra
--

SELECT pg_catalog.setval('public.codex_list_id_seq', 15, true);


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
-- Name: codex_list codex_list_prefix_key; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.codex_list
    ADD CONSTRAINT codex_list_prefix_key UNIQUE (prefix);


--
-- Name: command_usage command_usage_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.command_usage
    ADD CONSTRAINT command_usage_pkey PRIMARY KEY (name);


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
-- Name: unique_servers unique_servers_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.unique_servers
    ADD CONSTRAINT unique_servers_pkey PRIMARY KEY (server_id);


--
-- Name: unique_users unique_users_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.unique_users
    ADD CONSTRAINT unique_users_pkey PRIMARY KEY (user_id);


--
-- Name: character_attributes fk_character; Type: FK CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.character_attributes
    ADD CONSTRAINT fk_character FOREIGN KEY (character_id) REFERENCES public.characters(character_id);


--
-- Name: channels fk_codex; Type: FK CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT fk_codex FOREIGN KEY (codex) REFERENCES public.codex_list(id);


--
-- Name: servers fk_codex; Type: FK CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.servers
    ADD CONSTRAINT fk_codex FOREIGN KEY (codex) REFERENCES public.codex_list(id);


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
-- Name: TABLE command_usage; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.command_usage TO bot;


--
-- Name: TABLE server_mod_roles; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.server_mod_roles TO bot;


--
-- Name: TABLE servers; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.servers TO bot;


--
-- Name: TABLE unique_servers; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.unique_servers TO bot;


--
-- Name: TABLE unique_users; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.unique_users TO bot;


--
-- PostgreSQL database dump complete
--

