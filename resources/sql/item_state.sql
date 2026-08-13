-- :name get-items-state-for-update :? :*
SELECT i.id,
       i.type,
       (SELECT array_agg(tag ORDER BY tag)
          FROM unnest(i.tagi) AS tag_id
          INNER JOIN tags ON tags.id = tag_id) AS tags,
       i.reading_selector AS "checkpoint-selector",
       i.reading_progress AS "checkpoint-progress"
FROM items i
WHERE i.id IN (:v*:item-ids)
ORDER BY i.id
FOR UPDATE

-- :name apply-items-tag-delta :! :n
UPDATE items
SET tagi = (tagi - COALESCE(
               (SELECT array_agg(id) FROM tags WHERE tag = ANY(:v:remove-tags)),
               ARRAY[]::integer[]))
           | COALESCE(
               (SELECT array_agg(id) FROM tags WHERE tag = ANY(:v:add-tags)),
               ARRAY[]::integer[])
WHERE id IN (:v*:item-ids)

-- :name set-items-reading-checkpoint :! :n
UPDATE items
SET reading_selector = :selector,
    reading_progress = :progress,
    reading_updated_ts = now()
WHERE id IN (:v*:item-ids)

-- :name clear-items-reading-checkpoint :! :n
UPDATE items
SET reading_selector = NULL,
    reading_progress = NULL,
    reading_updated_ts = NULL
WHERE id IN (:v*:item-ids)

-- :name get-reading-progress-items :? :*
SELECT i.id,
       i.title,
       i.author,
       i.entry->>'url' AS url,
       i.entry,
       i.ts,
       i.type,
       i.nlp_nwords AS nwords,
       i.nlp_top AS "top-words",
       s.key AS "source-key",
       (SELECT array_agg(tag ORDER BY tag)
          FROM unnest(i.tagi) AS tag_id
          INNER JOIN tags ON tags.id = tag_id) AS tags,
       i.reading_selector AS "checkpoint-selector",
       i.reading_progress AS "checkpoint-progress",
       i.reading_updated_ts AS "checkpoint-updated-ts"
FROM items i
INNER JOIN sources s ON s.id = i.source_id
WHERE i.reading_progress IS NOT NULL
  AND NOT (i.tagi @@ '3')
ORDER BY i.reading_updated_ts DESC, i.ts DESC, i.id DESC
LIMIT :limit

-- :name get-reading-queue-items :? :*
SELECT i.id,
       i.title,
       i.author,
       i.entry->>'url' AS url,
       i.entry,
       i.ts,
       i.type,
       i.nlp_nwords AS nwords,
       i.nlp_top AS "top-words",
       s.key AS "source-key",
       (SELECT array_agg(tag ORDER BY tag)
          FROM unnest(i.tagi) AS tag_id
          INNER JOIN tags ON tags.id = tag_id) AS tags,
       i.reading_selector AS "checkpoint-selector",
       i.reading_progress AS "checkpoint-progress",
       i.reading_updated_ts AS "checkpoint-updated-ts",
       description.text AS "description-text"
FROM reading_queue_items queue
INNER JOIN items i ON i.id = queue.item_id
INNER JOIN sources s ON s.id = i.source_id
LEFT JOIN item_data description
  ON description.item_id = i.id
 AND description.type = 'description'
 AND description.mime_type = 'text/plain'
ORDER BY CASE WHEN i.reading_progress IS NOT NULL
              THEN i.reading_updated_ts
              ELSE i.ts
         END DESC,
         i.id DESC
LIMIT :limit OFFSET :offset
