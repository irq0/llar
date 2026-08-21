-- :name insert-bookmark-capture :<! :1
INSERT INTO bookmark_capture_queue
  (url, url_fingerprint, title, submitted_by)
VALUES
  (:url, :url-fingerprint, :title, :submitted-by)
ON CONFLICT (url_fingerprint) DO NOTHING
RETURNING *, true AS inserted

-- :name get-bookmark-capture-by-fingerprint :? :1
SELECT *, false AS inserted
FROM bookmark_capture_queue
WHERE url_fingerprint = :url-fingerprint

-- :name get-bookmark-capture :? :1
SELECT *
FROM bookmark_capture_queue
WHERE id = :id

-- :name claim-next-bookmark-capture :<! :1
WITH candidate AS (
  SELECT id
  FROM bookmark_capture_queue
  WHERE (status = 'pending' AND next_attempt_ts <= now())
     OR (status = 'processing' AND lease_expires_ts <= now())
  ORDER BY CASE status
             WHEN 'processing' THEN lease_expires_ts
             ELSE next_attempt_ts
           END,
           id
  FOR UPDATE SKIP LOCKED
  LIMIT 1
)
UPDATE bookmark_capture_queue capture
SET status = 'processing',
    attempt_count = capture.attempt_count + 1,
    lease_version = capture.lease_version + 1,
    last_attempt_ts = now(),
    lease_expires_ts = now() + (:lease-seconds * interval '1 second'),
    updated_ts = now()
FROM candidate
WHERE capture.id = candidate.id
RETURNING capture.*

-- :name set-bookmark-capture-item :<! :1
UPDATE bookmark_capture_queue
SET item_id = :item-id,
    updated_ts = now()
WHERE id = :id AND status = 'processing' AND lease_version = :lease-version
RETURNING *

-- :name complete-bookmark-capture :<! :1
UPDATE bookmark_capture_queue
SET status = 'complete',
    item_id = :item-id,
    lease_expires_ts = NULL,
    next_attempt_ts = now(),
    failure_class = NULL,
    last_error = NULL,
    completed_ts = now(),
    updated_ts = now()
WHERE id = :id AND status = 'processing' AND lease_version = :lease-version
RETURNING *

-- :name reschedule-bookmark-capture :<! :1
UPDATE bookmark_capture_queue
SET status = 'pending',
    lease_expires_ts = NULL,
    next_attempt_ts = now() + (:delay-seconds * interval '1 second'),
    failure_class = :failure-class,
    last_error = :last-error,
    updated_ts = now()
WHERE id = :id AND status = 'processing' AND lease_version = :lease-version
RETURNING *

-- :name fail-bookmark-capture :<! :1
UPDATE bookmark_capture_queue
SET status = 'failed',
    lease_expires_ts = NULL,
    failure_class = :failure-class,
    last_error = :last-error,
    updated_ts = now()
WHERE id = :id AND status = 'processing' AND lease_version = :lease-version
RETURNING *

-- :name retry-bookmark-capture :<! :1
UPDATE bookmark_capture_queue
SET status = 'pending',
    attempt_count = 0,
    next_attempt_ts = now(),
    lease_expires_ts = NULL,
    failure_class = NULL,
    last_error = NULL,
    completed_ts = NULL,
    updated_ts = now()
WHERE id = :id AND status IN ('failed', 'dismissed')
RETURNING *

-- :name dismiss-bookmark-capture :<! :1
UPDATE bookmark_capture_queue
SET status = 'dismissed',
    lease_expires_ts = NULL,
    updated_ts = now()
WHERE id = :id AND status IN ('pending', 'failed')
RETURNING *

-- :name list-bookmark-captures :? :*
SELECT *
FROM bookmark_capture_queue
ORDER BY CASE status
           WHEN 'failed' THEN 0
           WHEN 'processing' THEN 1
           WHEN 'pending' THEN 2
           ELSE 3
         END,
         updated_ts DESC,
         id DESC
LIMIT :limit

-- :name bookmark-capture-operational-counts :? :*
WITH states(state) AS (
  VALUES ('ready'), ('processing'), ('retry_wait'), ('failed')
), counts AS (
  SELECT CASE
           WHEN status = 'pending' AND next_attempt_ts <= now() THEN 'ready'
           WHEN status = 'processing' AND lease_expires_ts <= now() THEN 'ready'
           WHEN status = 'processing' THEN 'processing'
           WHEN status = 'pending' THEN 'retry_wait'
           WHEN status = 'failed' THEN 'failed'
         END AS state
  FROM bookmark_capture_queue
  WHERE status IN ('pending', 'processing', 'failed')
)
SELECT states.state, count(counts.state) AS count
FROM states
LEFT JOIN counts USING (state)
GROUP BY states.state
ORDER BY states.state

-- :name bookmark-capture-oldest-ready :? :1
SELECT min(CASE status
             WHEN 'processing' THEN lease_expires_ts
             ELSE next_attempt_ts
           END) AS ready_since
FROM bookmark_capture_queue
WHERE (status = 'pending' AND next_attempt_ts <= now())
   OR (status = 'processing' AND lease_expires_ts <= now())

-- :name bookmark-capture-dashboard-counts :? :1
SELECT count(*) FILTER (
         WHERE (status = 'pending' AND next_attempt_ts <= now())
            OR (status = 'processing' AND lease_expires_ts <= now())) AS ready,
       count(*) FILTER (
         WHERE status = 'processing' AND lease_expires_ts > now()) AS processing,
       count(*) FILTER (
         WHERE status = 'pending' AND next_attempt_ts > now()) AS retry_wait,
       count(*) FILTER (WHERE status = 'failed') AS failed,
       count(*) FILTER (WHERE status = 'complete') AS complete
FROM bookmark_capture_queue

-- :name bookmark-capture-reader-activity-counts :? :1
SELECT count(*) FILTER (
         WHERE status IN ('pending', 'processing')) AS active,
       count(*) FILTER (
         WHERE status = 'complete'
           AND item_id IS NOT NULL
           AND completed_ts >= now() - interval '1 hour') AS recent_complete,
       count(*) FILTER (
         WHERE status = 'failed'
           AND updated_ts >= now() - interval '1 hour') AS recent_failed
FROM bookmark_capture_queue
WHERE submitted_by = 'reader'

-- :name bookmark-capture-reader-recent-complete :? :*
SELECT capture.id,
       capture.item_id,
       capture.url,
       capture.completed_ts,
       items.title AS item_title
FROM bookmark_capture_queue capture
LEFT JOIN items ON items.id = capture.item_id
WHERE capture.submitted_by = 'reader'
  AND capture.status = 'complete'
  AND capture.item_id IS NOT NULL
  AND capture.completed_ts >= now() - interval '1 hour'
ORDER BY capture.completed_ts DESC, capture.id DESC
LIMIT :limit
