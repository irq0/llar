CREATE INDEX items_reader_time_idx
    ON items (ts DESC, id DESC);
--;;
CREATE INDEX items_reader_unread_time_idx
    ON items (ts DESC, id DESC)
    WHERE tagi OPERATOR(@@) '0'::query_int;
--;;
CREATE INDEX items_reader_source_time_idx
    ON items (source_id, ts DESC, id DESC);
--;;
CREATE INDEX items_reader_source_unread_time_idx
    ON items (source_id, ts DESC, id DESC)
    WHERE tagi OPERATOR(@@) '0'::query_int;
