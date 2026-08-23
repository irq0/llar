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
