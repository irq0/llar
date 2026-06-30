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
    CASE (items.entry ->> 'language'::text)
    WHEN 'de'::text THEN
        'german'::text
    ELSE
        'english'::text
    END AS search_config,
    setweight(to_tsvector(
        CASE (items.entry ->> 'language'::text)
        WHEN 'de'::text THEN 'german'::regconfig
        ELSE 'english'::regconfig
        END,
        COALESCE(items.title, '')), 'A'::"char") ||
    setweight(to_tsvector('simple'::regconfig, unaccent(COALESCE(items.author, ''))), 'D'::"char") ||
    setweight(array_to_tsvector(items.nlp_nouns), 'B'::"char") ||
    setweight(array_to_tsvector(items.nlp_names), 'B'::"char") ||
    setweight(array_to_tsvector(items.nlp_verbs), 'C'::"char") ||
    setweight(array_to_tsvector(items.nlp_urls), 'D'::"char") ||
    setweight(to_tsvector(
        CASE (items.entry ->> 'language'::text)
        WHEN 'de'::text THEN 'german'::regconfig
        ELSE 'english'::regconfig
        END,
        left(COALESCE(item_text.search_text, ''), 200000)), 'D'::"char") AS document,
    left(concat_ws(' ',
                   items.title,
                   items.author,
                   array_to_string(items.nlp_nouns, ' '),
                   array_to_string(items.nlp_names, ' '),
                   array_to_string(items.nlp_verbs, ' '),
                   array_to_string(items.nlp_urls, ' '),
                   item_text.search_text), 200000) AS headline_text
FROM sources
JOIN items ON sources.id = items.source_id
LEFT JOIN LATERAL (
    SELECT string_agg(item_data.text, ' ') AS search_text
    FROM item_data
    WHERE item_data.item_id = items.id
      AND item_data.text IS NOT NULL
      AND item_data.type IN ('content', 'description')
) item_text ON TRUE
WITH NO DATA;
--;;
CREATE INDEX search_index_document_idx ON search_index USING GIN (document);
--;;
CREATE INDEX search_index_key_idx ON search_index USING btree (key);
--;;
CREATE INDEX search_index_ts_idx ON search_index USING btree (ts);
--;;
CREATE VIEW search_items AS
SELECT
    id,
    title,
    key,
    search_config,
    document,
    headline_text
FROM search_index;
