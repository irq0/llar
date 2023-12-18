--
--  sources <--     items    --> item_data
--          1:*              1:*
--                         []--> tags
--                           *:*


CREATE EXTENSION IF NOT EXISTS btree_gin;
CREATE EXTENSION IF NOT EXISTS btree_gist;
CREATE EXTENSION IF NOT EXISTS hstore;
CREATE EXTENSION IF NOT EXISTS pg_trgm;
CREATE EXTENSION IF NOT EXISTS unaccent;
CREATE EXTENSION IF NOT EXISTS intarray;

-- Enums

CREATE TYPE item_data_type AS ENUM (
    'content',
    'description',
    'thumbnail'
);

CREATE TYPE item_type AS ENUM (
    'tweet',
    'mail',
    'link',
    'feed',
    'document',
    'bookmark',
    'website'
);

CREATE TABLE IF NOT EXISTS sources (
    id serial PRIMARY KEY,
    key text NOT NULL,
    name text NOT NULL,
    type item_type NOT NULL,
    created_ts timestamp WITHOUT time zone DEFAULT now() NOT NULL,
    updated_ts timestamp WITHOUT time zone DEFAULT now() NOT NULL,
    data hstore
);

CREATE TABLE IF NOT EXISTS tags (
    id serial PRIMARY KEY,
    tag text UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS item_data (
    id serial PRIMARY KEY,
    item_id bigint NOT NULL,
    mime_type character varying(50) NOT NULL,
    type item_data_type NOT NULL,
    data bytea,
    text text
);

CREATE TABLE IF NOT EXISTS items (
    id serial PRIMARY KEY,
    hash character varying(72) NOT NULL,
    title text NOT NULL,
    type item_type NOT NULL,
    source_id bigint NOT NULL,
    ts timestamp without time zone NOT NULL,
    nlp_nwords integer NOT NULL,
    nlp_urls text[] NOT NULL,
    nlp_names text[] NOT NULL,
    nlp_nouns text[] NOT NULL,
    nlp_verbs text[] NOT NULL,
    nlp_top jsonb NOT NULL,
    entry jsonb NOT NULL,
    author text NOT NULL,
    tagi integer[] NOT NULL
);

-- Constraints

--- On 'item_data'

-- item may only have one piece of data per (mime/type, type)
-- e.g $item -> (text/plain, content), (text/html, content), ...
ALTER TABLE ONLY item_data
    ADD CONSTRAINT item_data_mime_type_type_item_id_key UNIQUE (mime_type, type, item_id);

-- deduplicate items by source defined hash
ALTER TABLE ONLY items ADD CONSTRAINT items_hash_key UNIQUE (hash);

-- humans identify their sources by clojure keyword
ALTER TABLE ONLY sources ADD CONSTRAINT sources_key_key UNIQUE (key);

-- item gone -> item data gone
ALTER TABLE ONLY item_data
    ADD CONSTRAINT item_contents_item_id_fkey FOREIGN KEY (item_id) REFERENCES items(id) ON UPDATE CASCADE ON DELETE CASCADE;
