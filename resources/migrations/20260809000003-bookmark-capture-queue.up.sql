CREATE TABLE bookmark_capture_queue (
    id bigserial PRIMARY KEY,
    url text NOT NULL,
    url_fingerprint character varying(72) NOT NULL UNIQUE,
    title text,
    status text NOT NULL DEFAULT 'pending'
        CHECK (status IN ('pending', 'processing', 'complete', 'failed', 'dismissed')),
    attempt_count integer NOT NULL DEFAULT 0 CHECK (attempt_count >= 0),
    next_attempt_ts timestamptz NOT NULL DEFAULT now(),
    lease_expires_ts timestamptz,
    lease_version bigint NOT NULL DEFAULT 0 CHECK (lease_version >= 0),
    item_id bigint REFERENCES items(id) ON UPDATE CASCADE ON DELETE SET NULL,
    submitted_by text NOT NULL,
    failure_class text,
    last_error text,
    created_ts timestamptz NOT NULL DEFAULT now(),
    updated_ts timestamptz NOT NULL DEFAULT now(),
    last_attempt_ts timestamptz,
    completed_ts timestamptz
);
--;;
CREATE INDEX bookmark_capture_queue_ready_idx
    ON bookmark_capture_queue (next_attempt_ts, id)
    WHERE status = 'pending';
--;;
CREATE INDEX bookmark_capture_queue_lease_idx
    ON bookmark_capture_queue (lease_expires_ts, id)
    WHERE status = 'processing';
--;;
CREATE INDEX bookmark_capture_queue_recent_idx
    ON bookmark_capture_queue (updated_ts DESC, id DESC);
