-- :name get-gem-facet-rows :? :*
WITH archived AS (
  SELECT i.id, i.tagi, s.key AS source_key
  FROM items i
  INNER JOIN sources s ON s.id = i.source_id
  WHERE i.tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = 'archive')::query_int
), topics AS (
  SELECT archived.id, tag
  FROM archived, unnest(tagi) AS tag_id
  INNER JOIN tags ON tags.id = tag_id
  WHERE tag NOT IN ('archive', 'saved', 'unread', 'in-progress')
)
SELECT 'summary' AS kind, 'total' AS value, count(*) AS count FROM archived
UNION ALL
SELECT 'summary', 'topics', count(DISTINCT tag) FROM topics
UNION ALL
SELECT 'summary', 'sources', count(DISTINCT source_key) FROM archived
UNION ALL
SELECT 'tag', tag, count(*) FROM topics GROUP BY tag
UNION ALL
SELECT 'tag', '__untagged__', count(*)
FROM archived
WHERE NOT EXISTS (SELECT 1 FROM topics WHERE topics.id = archived.id)
UNION ALL
SELECT 'source', source_key, count(*) FROM archived GROUP BY source_key
ORDER BY kind, count DESC, value;

-- :name get-gem-items :? :*
SELECT i.id,
       i.title,
       i.author,
       i.entry,
       i.entry ->> 'url' AS url,
       i.ts,
       i.type,
       i.nlp_nwords AS nwords,
       i.nlp_top AS "top-words",
       s.key AS "source-key",
       ARRAY(SELECT t.tag
             FROM unnest(i.tagi) AS tag_id
             INNER JOIN tags t ON t.id = tag_id
             ORDER BY t.tag) AS tags,
       description.text AS "description-text",
       count(*) OVER () AS "total-count"
FROM items i
INNER JOIN sources s ON s.id = i.source_id
LEFT JOIN LATERAL (
  SELECT text
  FROM item_data
  WHERE item_id = i.id AND type = 'description' AND mime_type = 'text/plain'
  ORDER BY id
  LIMIT 1
) description ON TRUE
WHERE i.tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = 'archive')::query_int
--~ (when (:tag params) "  AND i.tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = :tag)::query_int")
--~ (when (:untagged? params) "  AND NOT EXISTS (SELECT 1 FROM unnest(i.tagi) tag_id INNER JOIN tags t ON t.id = tag_id WHERE t.tag NOT IN ('archive', 'saved', 'unread', 'in-progress'))")
--~ (when (:source params) "  AND s.key = :source")
--~ (if (= "oldest" (:sort params)) "ORDER BY i.ts ASC, i.id ASC" "ORDER BY i.ts DESC, i.id DESC")
LIMIT :limit OFFSET :offset;

-- :name get-gem-rediscovery-candidates :? :*
SELECT i.id,
       i.title,
       i.author,
       i.entry,
       i.entry ->> 'url' AS url,
       i.ts,
       i.type,
       i.nlp_nwords AS nwords,
       i.nlp_top AS "top-words",
       s.key AS "source-key",
       ARRAY(SELECT t.tag
             FROM unnest(i.tagi) AS tag_id
             INNER JOIN tags t ON t.id = tag_id
             ORDER BY t.tag) AS tags,
       description.text AS "description-text",
       history.last_resurfaced AS "last-resurfaced",
       history.last_opened AS "last-opened"
FROM items i
INNER JOIN sources s ON s.id = i.source_id
LEFT JOIN LATERAL (
  SELECT text
  FROM item_data
  WHERE item_id = i.id AND type = 'description' AND mime_type = 'text/plain'
  ORDER BY id
  LIMIT 1
) description ON TRUE
LEFT JOIN LATERAL (
  SELECT max(recorded_at) FILTER (
           WHERE event_type = 'impression'
             AND metadata ->> 'feature' = 'gems') AS last_resurfaced,
         max(recorded_at) FILTER (
           WHERE event_type = 'item-opened') AS last_opened
  FROM item_events
  WHERE item_id = i.id AND recorded_at < :day_cutoff
) history ON TRUE
WHERE i.tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = 'archive')::query_int
ORDER BY GREATEST(COALESCE(history.last_resurfaced, '-infinity'::timestamptz),
                  COALESCE(history.last_opened, '-infinity'::timestamptz)) ASC,
         md5(i.id::text || :day_key) ASC
LIMIT :candidate_limit OFFSET :candidate_offset;
