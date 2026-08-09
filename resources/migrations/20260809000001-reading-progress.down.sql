UPDATE items
SET tagi = tagi | ARRAY[2]
WHERE reading_progress IS NOT NULL;
--;;
DROP VIEW reading_queue_items;
--;;
DROP INDEX items_reading_updated_idx;
--;;
ALTER TABLE items
    DROP COLUMN reading_selector,
    DROP COLUMN reading_progress,
    DROP COLUMN reading_updated_ts;
