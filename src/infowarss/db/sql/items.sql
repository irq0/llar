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
select count(*), skeys(tags) as tag
from items
where source_id = :id
group by skeys(tags)

-- :name get-item-count-unread-today :? :1
select count(*) from items
  where source_id = :id
  and date(ts) = current_date
and exist_inline(tags,'unread')

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
where exist_inline(items.tags, :v:item-tag)
--~ (when (:simple-filter params) "and :sql:simple-filter")

-- :name get-items-by-tag :? :*
select key, title, author, items.type, tags, items.id, entry
from items inner join sources
on items.source_id = sources.id
where exist_inline(tags, :v:tag)

-- :snip item-select-default-snip
select
  items.source_id as feed_id,
  title,
  author,
  entry->'url' as url,
  entry,
  exist_inline(tags, 'saved') as saved,
  (not exist_inline(tags, 'unread')) as read,
  akeys(tags) as tags,
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
  exist_inline(tags, 'saved') as saved,
  (not exist_inline(tags, 'unread')) as read,
  akeys(tags) as tags,
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

-- :snip cond-with-source
sources.key in (:v*:keys)

-- :snip cond-with-tag
exist_inline(items.tags, :v:tag)

-- :snip cond-with-type
items.type = :type::item_type

-- :name get-items-recent :? :*
:snip:select
:snip:from
--~ (when (:where params) "where :snip*:where")
--~ (when (:group-by-columns params) "group by :i*:group-by-columns")
order by
  ts desc, id desc
limit :limit  
