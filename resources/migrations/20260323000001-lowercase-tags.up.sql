INSERT INTO tags (tag)
SELECT DISTINCT lower(tag) FROM tags
WHERE lower(tag) NOT IN (SELECT tag FROM tags WHERE tag = lower(tag))
ON CONFLICT DO NOTHING;
--;;
DO $$
DECLARE
    r RECORD;
BEGIN
    FOR r IN
        SELECT t.id AS old_id, tl.id AS new_id
        FROM tags t
        JOIN tags tl ON lower(t.tag) = tl.tag AND tl.tag = lower(tl.tag)
        WHERE t.tag != lower(t.tag)
    LOOP
        UPDATE items
        SET tagi = array_replace(tagi, r.old_id, r.new_id)
        WHERE tagi @> ARRAY[r.old_id];
    END LOOP;
END $$;
--;;
UPDATE items SET tagi = uniq(sort(tagi));
--;;
DELETE FROM tags WHERE tag != lower(tag);
--;;
ALTER TABLE tags ADD CONSTRAINT chk_tag_normalized
  CHECK (tag = lower(tag) AND length(tag) > 0);
