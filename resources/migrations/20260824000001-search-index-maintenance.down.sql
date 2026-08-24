-- :disable-transaction
CREATE INDEX CONCURRENTLY IF NOT EXISTS items_ts_id_idx
    ON items (ts DESC, id DESC);
--;;
CREATE INDEX CONCURRENTLY IF NOT EXISTS items_source_ts_id_idx
    ON items (source_id, ts DESC, id DESC);
--;;
CREATE INDEX CONCURRENTLY IF NOT EXISTS items_unread_ts_id_idx
    ON items (ts DESC, id DESC)
    WHERE tagi OPERATOR(@@) '0'::query_int;
--;;
CREATE INDEX CONCURRENTLY IF NOT EXISTS items_unread_source_ts_id_idx
    ON items (source_id, ts DESC, id DESC)
    WHERE tagi OPERATOR(@@) '0'::query_int;
--;;
DROP INDEX CONCURRENTLY IF EXISTS search_index_id_idx;
