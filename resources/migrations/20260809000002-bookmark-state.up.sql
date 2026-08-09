UPDATE items
SET tagi = tagi | ARRAY[(SELECT id FROM tags WHERE tag = 'saved')]
WHERE type = 'bookmark'
  AND tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = 'unread')::query_int
  AND NOT (tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = 'archive')::query_int)
  AND NOT (tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = 'saved')::query_int);
--;;
DROP VIEW reading_queue_items;
--;;
CREATE VIEW reading_queue_items AS
SELECT id AS item_id
FROM items
WHERE NOT (tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = 'archive')::query_int)
  AND (tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = 'saved')::query_int
       OR reading_progress IS NOT NULL);
