-- :disable-transaction
-- search_index.id is one-to-one with items.id. Besides accelerating joins from
-- live item filters, this unique index enables non-blocking daily refreshes.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS search_index_id_idx
    ON search_index (id);
--;;
-- These legacy indexes are byte-for-byte duplicates of the canonical
-- items_reader_* indexes installed by 20260823000001.
DROP INDEX CONCURRENTLY IF EXISTS items_ts_id_idx;
--;;
DROP INDEX CONCURRENTLY IF EXISTS items_source_ts_id_idx;
--;;
DROP INDEX CONCURRENTLY IF EXISTS items_unread_ts_id_idx;
--;;
DROP INDEX CONCURRENTLY IF EXISTS items_unread_source_ts_id_idx;
