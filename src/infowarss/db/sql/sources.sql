-- :name create-sources :! :raw
create table sources(
  id   	     serial primary key,
  key	     text not null,
  name 	     text not null,
  created_ts timestamp without time zone not null,
  data	     hstore,
  type	     item_type not null,
)

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

