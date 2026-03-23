CREATE MATERIALIZED VIEW source_stats AS
SELECT
  source_id,
  COUNT(*)::float / GREATEST(
    EXTRACT(epoch FROM (now() - MIN(ts))) / 86400.0, 1.0
  ) AS items_per_day
FROM items
WHERE ts > now() - interval '90 days'
GROUP BY source_id;
--;;
CREATE UNIQUE INDEX source_stats_source_id_idx ON source_stats (source_id);
