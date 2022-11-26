-- Create source
-- :name create-source :<!
insert into sources (name, key, created_ts, type, data)
values (:name, :key, :created_ts, :data)
returning id
  
-- Get source by key
-- :name get-source-by-key :? :1
select * from sources where key = :key

-- Get sources
-- :name get-sources :? :raw
select name, key, created_ts, id, type, data->':title' as title
from sources

-- :name resolve-source-keys-to-ids :? :raw
select id from sources where key in (:v*:keys)

