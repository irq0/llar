-- :name create-item-type
-- :command :execute
-- :result :raw
-- :doc Create sources table
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
and :sql:simple-filter

-- :name get-items-by-tag :? :*
select key, title, author, items.type, tags, items.id, entry
from items inner join sources
on items.source_id = sources.id
where exist_inline(tags, :v:tag)



-- -- :snip items-by-id-sql-select-snip
-- select
--   items.source_id as feed_id,
--   title,
--   author,
--   entry->'url' as url,
--   entry, 
--   exist_inline(tags, 'saved') as saved,
--   (not exist_inline(tags, 'unread')) as read,
--   akeys(tags) as tags, 
--   ts,
--   nlp_names as names,
--   nlp_verbs as verbs,
--   nlp_top as "top-words",
--   nlp_urls as urls,
--   items.type,
--   nlp_nwords as nwords,
--   sources.key as "source-key",

-- -- :snip items-by-id-sql-select-with-data-snip
-- select
--   items.source_id as feed_id,
--   title,
--   author,
--    "entry->'url' as url, "
--    "entry, "
--    "exist_inline(tags, 'saved') as saved, "
--    "(not exist_inline(tags, 'unread')) as read, "
--    "akeys(tags) as tags, "
--    "ts, "
--    "nlp_names as names, "
--    "nlp_verbs as verbs, "
--    "nlp_top as \"top-words\", "
--    "nlp_urls as urls, "
--    "items.type, "
--    "nlp_nwords as nwords, "
--    "max(sources.key) as \"source-key\", "))


