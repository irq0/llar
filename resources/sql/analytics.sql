-- Word count groups: {group -> count}
-- :name get-word-count-groups :? :raw
select
case
  when nlp_nwords > 0    and nlp_nwords <= 200  then 200
  when nlp_nwords > 200  and nlp_nwords <= 400  then 400
  when nlp_nwords > 400  and nlp_nwords <= 800  then 800
  when nlp_nwords > 800  and nlp_nwords <= 1600 then 1600
  when nlp_nwords > 1600 and nlp_nwords <= 3200 then 3200
  when nlp_nwords > 3200                        then 0
end as nword_groups,
count(*)
from items
group by nword_groups
order by nword_groups

-- Tag Statistics {tag -> item count}
-- :name get-tag-stats :? :raw
select tag, count(*) as sum
from items, tags, lateral (select unnest(tagi) as tag_id) as un
where tags.id = tag_id group by tag_id, tags.tag;

-- Tags {list of tags}
-- :name get-tags :? :raw
select tag from tags

-- Type Statistics (type -> item count)
-- :name get-type-stats :? :raw
select type, count(*)
from items
group by type

-- :name get-table-row-counts :? :*
select 'items' as table, count(*) from items
union
select 'sources' as table, count(*) from sources
union
select 'item_data' as table, count(*) from item_data
