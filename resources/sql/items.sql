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

-- :name get-item-count-of-source :? :1
select count(*) from items where source_id = :id

-- :name get-item-count-by-tag-of-source :? :raw
select count(tag), tag
from items, tags, lateral (select unnest(tagi) as tag_id) as tags_un
where source_id = :id
and tags.id = tag_id
group by tag;

-- :name get-item-count-unread-today :? :1
select count(*) from items
  where source_id = :id
  and date(ts) = current_date
  and tagi @@ '0';

-- :name get-sources-with-item-tags-count :? :raw
select distinct on (key) key,
  name as title,
   key,
   created_ts,
   sources.id,
   sources.type
from sources
inner join items
on sources.id = items.source_id
where tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = :item-tag)::query_int
--~ (when (:simple-filter params) "and :sql:simple-filter")

-- :name get-items-by-tag :? :*
select key, title, author, items.type, items.id, entry
from items inner join sources
on items.source_id = sources.id
where tagi @@ (SELECT format('(%s)', id) FROM tags WHERE tag = :tag)::query_int

-- :snip item-select-default-snip
select
  items.source_id as feed_id,
  title,
  author,
  entry->'url' as url,
  entry,
  tagi @@ '1' as saved,
  tagi @@ '!0' as read,
  (select array_agg(tag) from unnest(tagi) as tag_id inner join tags on tag_id = id) as tags,
  ts,
  nlp_names as names,
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
  (select array_agg(tag) from unnest(tagi) as tag_id inner join tags on tag_id = id) as tags,
  ts,
  nlp_names as names,
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

-- :snip item-from-join-with-preview-data-snip
from items
  inner join sources on items.source_id = sources.id
  left join item_data on
     items.id = item_data.item_id
     and item_data.type = 'description'
     and mime_type = 'text/plain'

-- :name get-item-by-id :? :1
:snip:select
:snip:from
where
  items.id = :id
--~ (when (:where params) ":snip:where")
--~ (when (:group-by-columns params) "group by :i*:group-by-columns")

-- :snip cond-before
(items.ts, items.id) < (:ts, :id)

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
order by
  items.ts desc, items.id desc
limit :limit
