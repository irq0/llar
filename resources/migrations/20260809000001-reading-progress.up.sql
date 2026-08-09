ALTER TABLE items
    ADD COLUMN reading_selector jsonb,
    ADD COLUMN reading_progress double precision
        CHECK (reading_progress IS NULL
               OR (reading_progress >= 0.0 AND reading_progress <= 1.0)),
    ADD COLUMN reading_updated_ts timestamptz;
--;;
CREATE INDEX items_reading_updated_idx
    ON items (reading_updated_ts DESC)
    WHERE reading_progress IS NOT NULL;
--;;
UPDATE items
SET tagi = tagi - ARRAY[2],
    reading_progress = 0.0,
    reading_updated_ts = now()
WHERE tagi @@ '2';
--;;
CREATE VIEW reading_queue_items AS
SELECT id AS item_id
FROM items
WHERE NOT (tagi @@ '3')
  AND (tagi @@ '1'
       OR reading_progress IS NOT NULL
       OR (type = 'bookmark' AND tagi @@ '0'));
