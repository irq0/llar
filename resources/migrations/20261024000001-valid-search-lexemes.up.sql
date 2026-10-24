-- PostgreSQL full-text search stores each lexeme in fewer than 2 KiB. Recent
-- PostgreSQL releases enforce that limit in array_to_tsvector, while LLAR's
-- NLP arrays contain already-normalized lexemes (including complete URLs).
-- NLP fields are derived data. Remove only values PostgreSQL cannot index;
-- titles, item metadata, and item_data content remain unchanged.
UPDATE items
SET nlp_urls = ARRAY(
      SELECT lexeme
      FROM unnest(COALESCE(nlp_urls, ARRAY[]::text[])) AS candidate(lexeme)
      WHERE lexeme IS NOT NULL AND lexeme <> '' AND octet_length(lexeme) <= 2046),
    nlp_names = ARRAY(
      SELECT lexeme
      FROM unnest(COALESCE(nlp_names, ARRAY[]::text[])) AS candidate(lexeme)
      WHERE lexeme IS NOT NULL AND lexeme <> '' AND octet_length(lexeme) <= 2046),
    nlp_nouns = ARRAY(
      SELECT lexeme
      FROM unnest(COALESCE(nlp_nouns, ARRAY[]::text[])) AS candidate(lexeme)
      WHERE lexeme IS NOT NULL AND lexeme <> '' AND octet_length(lexeme) <= 2046),
    nlp_verbs = ARRAY(
      SELECT lexeme
      FROM unnest(COALESCE(nlp_verbs, ARRAY[]::text[])) AS candidate(lexeme)
      WHERE lexeme IS NOT NULL AND lexeme <> '' AND octet_length(lexeme) <= 2046)
WHERE EXISTS (
        SELECT 1
        FROM unnest(COALESCE(nlp_urls, ARRAY[]::text[])) AS candidate(lexeme)
        WHERE lexeme IS NULL OR lexeme = '' OR octet_length(lexeme) > 2046)
   OR EXISTS (
        SELECT 1
        FROM unnest(COALESCE(nlp_names, ARRAY[]::text[])) AS candidate(lexeme)
        WHERE lexeme IS NULL OR lexeme = '' OR octet_length(lexeme) > 2046)
   OR EXISTS (
        SELECT 1
        FROM unnest(COALESCE(nlp_nouns, ARRAY[]::text[])) AS candidate(lexeme)
        WHERE lexeme IS NULL OR lexeme = '' OR octet_length(lexeme) > 2046)
   OR EXISTS (
        SELECT 1
        FROM unnest(COALESCE(nlp_verbs, ARRAY[]::text[])) AS candidate(lexeme)
        WHERE lexeme IS NULL OR lexeme = '' OR octet_length(lexeme) > 2046);
