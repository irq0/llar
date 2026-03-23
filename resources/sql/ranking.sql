-- :name get-source-stats :? :*
SELECT
  sources.key as source_key,
  ss.items_per_day,
  LEAST(:rarity-cap, 24.0 / GREATEST(COALESCE(ss.items_per_day, 1.0), 0.01)) as rarity_boost_hours,
  (SELECT count(*) FROM items i WHERE i.source_id = sources.id
   AND i.tagi @@ (SELECT format('(%s)', id)::query_int FROM tags WHERE tag = 'highlight')) as highlight_count,
  (SELECT count(*) FROM items i WHERE i.source_id = sources.id
   AND i.ts > now() - interval '7 days') as items_7d
FROM sources
LEFT JOIN source_stats ss ON sources.id = ss.source_id
ORDER BY rarity_boost_hours DESC

-- :name get-ranked-vs-time-preview :? :*
SELECT
  items.id,
  items.title,
  sources.key as source_key,
  items.ts,
  (GREATEST(0, extract(epoch from now() - items.ts) / 3600.0)
   - CASE WHEN items.tagi @@ (SELECT format('(%s)', id)::query_int FROM tags WHERE tag = 'highlight')
     THEN :highlight-boost ELSE 0.0 END
   - LEAST(:rarity-cap, 24.0 / GREATEST(COALESCE(ss.items_per_day, 1.0), 0.01))
  ) as effective_age_hours,
  (items.tagi @@ (SELECT format('(%s)', id)::query_int FROM tags WHERE tag = 'highlight')) as is_highlighted
FROM items
INNER JOIN sources ON items.source_id = sources.id
LEFT JOIN source_stats ss ON items.source_id = ss.source_id
WHERE items.ts > now() - interval '7 days'
ORDER BY effective_age_hours ASC
LIMIT 50
