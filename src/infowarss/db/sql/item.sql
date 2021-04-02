-- :snip conflict-items-overwrite-snip
do update set
  hash = excluded.hash,
  source_id = excluded.source_id,
  ts = excluded.ts,
  title = excluded.title,
  author = excluded.author,
  type = excluded.type,
  tags = excluded.tags,
  nlp_nwords = excluded.nlp_nwords,
  nlp_urls = excluded.nlp_urls,
  nlp_names = excluded.nlp_names,
  nlp_nouns = excluded.nlp_nouns,
  nlp_verbs = excluded.nlp_verbs,
  nlp_top = excluded.nlp_top,
  entry = excluded.entry

-- :snip conflict-items-ignore-dupe-snip
do nothing

-- Store item and create source when necessary
-- 
-- Collision handling:
--  Sources by unique key. Existing sources update their name, data and updated_ts
--  Items by hash. Existing items are not updated
--
-- Returns id, hash or nil if the item already exists
--
-- :name store-item :i! :*
with source_upsert as (
  insert into sources (
    name, key, type, data
  )
  values (
    :source.name, :source.key, :source.type, :source.data
  )
  on conflict on constraint sources_key_key
  do update set updated_ts = now(), name = :source.name, data = :source.data
    where sources.name != :source.name and sources.data != :source.data
  returning id
)
insert into items(
  hash,
  source_id,
  ts,
  title,
  author,
  type,
  tags,
  nlp_nwords, nlp_urls, nlp_names, nlp_nouns, nlp_verbs, nlp_top,
  entry
)
values (
  :hash,
  (select coalesce(
     (select id from source_upsert),
     (select id from sources where key = :source.key))),
  :ts,
  :title,
  :author,
  :type,
  :tags,
  :nlp-nwords, :nlp-urls, :nlp-names, :nlp-nouns, :nlp-verbs, :nlp-top,
  :entry
)
on conflict on constraint items_hash_key :snip:on-conflict
returning id, hash

-- Store item attachment
-- :name store-item-data :i! :*
insert into item_data (
  item_id,
  mime_type,
  type,
  text,
  data
)
values (
  :item-id,
  :mime-type,
  :type,
  :text,
  :data
)
on conflict on constraint item_data_mime_type_type_item_id_key do update set
  mime_type = excluded.mime_type,
  type = excluded.type,
  item_id = excluded.item_id
returning id, mime_type, type, data is not null as is_binary
