-- :name create-item-type :! :raw
create type item_type as enum(
  'tweet',
  'mail',
  'link',
  'feed',
  'document',
  'bookmark'
  'website'
)

-- :name get-source-ids-with-tag :? :*
select distinct items.source_id
from items
where items.source_id in (:v*:source-ids)
  and items.tagi @@ (select format('(%s)', id) from tags where tag = :tag)::query_int

-- :name get-source-item-counts :? :*
select items.source_id, count(*) as item_count
from items
where items.source_id in (:v*:source-ids)
--~ (when (:simple-filter params) "and :sql:simple-filter")
--~ (when (:with-tag params) "and items.tagi @@ (select format('(%s)', id) from tags where tag = :with-tag)::query_int")
group by items.source_id

-- :name get-items-by-tag :? :*
select key, title, author, items.type, items.id, entry
from items inner join sources
on items.source_id = sources.id
where tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = :tag)::query_int

-- :name get-item-preview-descriptions :? :*
select item_id as "item-id",
       left(text, cast(:max-characters as integer)) as description
from item_data
where item_id in (:v*:item-ids)
  and type = 'description'
  and mime_type = 'text/plain'
  and text is not null

-- :snip item-select-default-snip
select
--~ (when (:with-rank-score? params) "  selected_items.rank_score as \"rank-score\",")
  items.source_id as feed_id,
  title,
  author,
  entry->'url' as url,
  entry,
  tagi @@ '1' as saved,
  tagi @@ '!0' as read,
--~ (when (:with-reading-progress? params) "  reading_selector as \"checkpoint-selector\",")
--~ (when (:with-reading-progress? params) "  reading_progress as \"checkpoint-progress\",")
--~ (when (:with-reading-progress? params) "  reading_updated_ts as \"checkpoint-updated-ts\",")
  (select array_agg(tag) from unnest(tagi) as tag_id inner join tags on tag_id = id) as tags,
  ts,
  nlp_names as names,
  nlp_nouns as nouns,
  nlp_verbs as verbs,
  nlp_top as "top-words",
  nlp_urls as urls,
  items.type,
  nlp_nwords as nwords,
  items.id,
  sources.key as "source-key"

-- :snip item-select-with-data-snip
select
  items.source_id as feed_id,
  title,
  author,
  entry->'url' as url,
  entry,
  tagi @@ '1' as saved,
  tagi @@ '!0' as read,
--~ (when (:with-reading-progress? params) "  reading_selector as \"checkpoint-selector\",")
--~ (when (:with-reading-progress? params) "  reading_progress as \"checkpoint-progress\",")
--~ (when (:with-reading-progress? params) "  reading_updated_ts as \"checkpoint-updated-ts\",")
  (select array_agg(tag) from unnest(tagi) as tag_id inner join tags on tag_id = id) as tags,
  ts,
  nlp_names as names,
  nlp_nouns as nouns,
  nlp_verbs as verbs,
  nlp_top as "top-words",
  nlp_urls as urls,
  items.type,
  nlp_nwords as nwords,
  items.id,
  max(sources.key) as "source-key",
  json_agg(mime_type) as mime_types,
  json_agg(item_data.type) as data_types,
  json_agg(item_data.text) as text,
  json_agg(item_data.data) as "bin-data"

-- :snip item-from-join-default-snip
from items
  inner join sources on items.source_id = sources.id

-- :snip item-from-join-with-data-table-snip
from items
  inner join sources on items.source_id = sources.id
  left join item_data on
    items.id = item_data.item_id
    and (item_data.type = 'content' or item_data.type = 'description')

-- :snip item-from-join-ranked-snip
from items
  inner join sources on items.source_id = sources.id
  left join source_stats ss on items.source_id = ss.source_id

-- :snip item-from-join-with-data-table-ranked-snip
from items
  inner join sources on items.source_id = sources.id
  left join item_data on
    items.id = item_data.item_id
    and (item_data.type = 'content' or item_data.type = 'description')
  left join source_stats ss on items.source_id = ss.source_id

-- :name get-item-by-id :? :1
:snip:select
:snip:from
where
  items.id = :id
--~ (when (:where params) ":snip:where")
--~ (when (:group-by-columns params) "group by :i*:group-by-columns")

-- :snip cond-before
(items.ts, items.id) < (:ts, :id)

-- :snip cond-after
(items.ts, items.id) > (:ts, :id)

-- :snip order-by-newest-snip
order by items.ts desc, items.id desc

-- :snip order-by-oldest-snip
order by items.ts asc, items.id asc

-- :snip order-by-ranked-snip
order by (
  GREATEST(0, extract(epoch from now() - items.ts) / 3600.0)
  - CASE WHEN items.tagi @@ (SELECT format('(%s)', id)::query_int FROM tags WHERE tag = 'highlight')
    THEN :highlight-boost ELSE 0.0 END
  - CASE WHEN items.type IN ('bookmark', 'mail') THEN 0.0
    ELSE LEAST(:rarity-cap, 24.0 / GREATEST(COALESCE(ss.items_per_day, 1.0), 0.01)) END
) ASC, items.id DESC

-- :snip order-by-selected-rank-snip
order by selected_items.rank_score asc, selected_items.id desc

-- :snip cond-with-source-keys
sources.key in (:v*:keys)

-- :snip cond-with-source-ids
sources.id in (:v*:ids)

-- :snip cond-with-tag
tagi @@ (select format('(%s)', id) FROM tags WHERE tag = :tag)::query_int

-- :snip cond-with-type
items.type = :type::item_type

-- :name get-items-recent :? :*
:snip:select
:snip:from
--~ (when (:where params) "where :snip*:where")
--~ (when (:group-by-columns params) "group by :i*:group-by-columns")
:snip:order-by
limit :limit
--~ (when (some? (:offset params)) "offset :offset")

-- :name get-items-recent-by-tag :? :*
-- Keep tag discovery behind an optimization fence. GIN can identify the
-- matching items efficiently, but cannot provide timestamp order. Without
-- this boundary PostgreSQL can choose the timestamp index for ORDER BY/LIMIT
-- and inspect most of the library when the requested tag is sparse.
with matching_items as materialized (
  select items.id, items.source_id, items.ts, items.type, items.tagi
  from items
  where items.tagi @@ (select format('(%s)', id) from tags where tag = :tag)::query_int
),
selected_items as materialized (
  select items.id
  from matching_items as items
    inner join sources on items.source_id = sources.id
  --~ (when (:where params) "where :snip*:where")
  :snip:order-by
  limit :limit
)
:snip:select
from selected_items
  inner join items on items.id = selected_items.id
  inner join sources on items.source_id = sources.id
:snip:order-by

-- :name get-items-ranked-bounded :? :*
-- Ranking score is age minus bounded boosts. A seed batch establishes an
-- exact timestamp horizon: an item older than worst_seed_score + max_boost
-- cannot enter the requested page even if it receives every possible boost.
with
--~ (when (:tag params) "tagged_items as materialized (")
--~ (when (:tag params) "  select items.id, items.source_id, items.ts, items.type, items.tagi")
--~ (when (:tag params) "  from items")
--~ (when (:tag params) "  where items.tagi @@ (select format('(%s)', id) from tags where tag = :tag)::query_int")
--~ (when (:tag params) "),")
ranking_constants as materialized (
  select :ranked-at::timestamptz as ranked_at,
         :highlight-boost::double precision as highlight_boost,
         :rarity-cap::double precision as rarity_cap,
         :max-boost::double precision as max_boost
),
seed_items as materialized (
  select items.id, items.source_id, items.ts, items.type, items.tagi
  from :i:rank-source as items
    inner join sources on items.source_id = sources.id
    cross join ranking_constants constants
  --~ (when (:where params) "where :snip*:where")
  --~ (when (:rank-cursor params) (if (:where params) "and" "where"))
  --~ (when (:rank-cursor params) "items.ts < (constants.ranked_at - (GREATEST(0.0, :rank-cursor.score + constants.max_boost) * interval '1 hour'))::timestamp")
  order by items.ts desc, items.id desc
  limit :limit
),
seed_scores as materialized (
  select seed_items.id,
         (
           GREATEST(0, extract(epoch from constants.ranked_at - seed_items.ts) / 3600.0)
           - case when seed_items.tagi @@ (select format('(%s)', id)::query_int from tags where tag = 'highlight')
             then constants.highlight_boost else 0.0 end
           - case when seed_items.type in ('bookmark', 'mail') then 0.0
             else least(constants.rarity_cap,
                        24.0 / greatest(coalesce(stats.items_per_day, 1.0), 0.01)) end
         )::double precision as rank_score
  from seed_items
    left join source_stats stats on seed_items.source_id = stats.source_id
    cross join ranking_constants constants
),
ranking_horizon as materialized (
  select coalesce((select max(seed_scores.rank_score) from seed_scores) + constants.max_boost,
--~ (if (:rank-cursor params) "                  :rank-cursor.score + constants.max_boost" "                  0.0")
                 ) as hours
  from ranking_constants constants
),
candidate_scores as materialized (
  select items.id,
         (
           GREATEST(0, extract(epoch from constants.ranked_at - items.ts) / 3600.0)
           - case when items.tagi @@ (select format('(%s)', id)::query_int from tags where tag = 'highlight')
             then constants.highlight_boost else 0.0 end
           - case when items.type in ('bookmark', 'mail') then 0.0
             else least(constants.rarity_cap,
                        24.0 / greatest(coalesce(stats.items_per_day, 1.0), 0.01)) end
         )::double precision as rank_score
  from :i:rank-source as items
    inner join sources on items.source_id = sources.id
    left join source_stats stats on items.source_id = stats.source_id
    cross join ranking_constants constants
    cross join ranking_horizon horizon
  where items.ts >= (constants.ranked_at - (horizon.hours * interval '1 hour'))::timestamp
  --~ (when (:where params) "and :snip*:where")
),
selected_items as materialized (
  select candidate_scores.id, candidate_scores.rank_score
  from candidate_scores
  --~ (when (:rank-cursor params) "where candidate_scores.rank_score > :rank-cursor.score or (candidate_scores.rank_score = :rank-cursor.score and candidate_scores.id < :rank-cursor.id)")
  order by candidate_scores.rank_score asc, candidate_scores.id desc
  limit :limit
)
:snip:select
from selected_items
  inner join items on items.id = selected_items.id
  inner join sources on items.source_id = sources.id
:snip:order-by
