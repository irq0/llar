DROP VIEW IF EXISTS search_items;
--;;
DROP MATERIALIZED VIEW IF EXISTS search_index;
--;;
CREATE MATERIALIZED VIEW search_index AS
SELECT
    items.id,
    items.title,
    items.ts,
    sources.key,
    ((((((setweight(to_tsvector((i1.lang)::regconfig, items.title), 'A'::"char") ||
          setweight(to_tsvector('simple'::regconfig, unaccent (items.author)), 'D'::"char")) ||
	  setweight(array_to_tsvector (items.nlp_nouns), 'B'::"char")) ||
	  setweight(array_to_tsvector (items.nlp_names), 'B'::"char")) ||
	  setweight(array_to_tsvector (items.nlp_verbs), 'C'::"char")) ||
	  setweight(array_to_tsvector (items.nlp_urls), 'D'::"char"))) AS document
FROM (sources
    JOIN items ON (sources.id = items.source_id)),
LATERAL (
    SELECT
        CASE (items.entry ->> 'language'::text)
        WHEN 'en'::text THEN
            'english'::text
        WHEN 'de'::text THEN
            'german'::text
        ELSE
            'english'::text
        END AS lang) i1 WITH NO DATA;
--;;
CREATE VIEW search_items AS
SELECT
    items.id,
    items.title,
    sources.key,
    ((((((setweight(to_tsvector((i1.lang)::regconfig, items.title), 'A'::"char") ||
          setweight(to_tsvector('simple'::regconfig, unaccent (items.author)), 'D'::"char")) ||
	  setweight(array_to_tsvector (items.nlp_nouns), 'B'::"char")) ||
	  setweight(array_to_tsvector (items.nlp_names), 'B'::"char")) ||
	  setweight(array_to_tsvector (items.nlp_verbs), 'C'::"char")) ||
	  setweight(array_to_tsvector (items.nlp_urls), 'D'::"char"))) AS document
FROM (sources
    JOIN items ON (sources.id = items.source_id)),
LATERAL (
    SELECT
        CASE (items.entry ->> 'language'::text)
        WHEN 'en'::text THEN
            'english'::text
        WHEN 'de'::text THEN
            'german'::text
        ELSE
            'english'::text
        END AS lang) i1;
