-- :name fever-sources :? :*
select id, key, name, type, data, updated_ts
from sources
order by id

-- :name fever-items :? :*
select i.id,
       i.source_id as feed_id,
       i.title,
       i.author,
       i.ts,
       i.entry->>'url' as url,
       i.tagi @@ '1' as is_saved,
       i.tagi @@ '!0' as is_read,
       coalesce(content_html.text,
                description_html.text,
                content_text.text,
                description_text.text,
                '') as content,
       case
         when content_html.text is not null or description_html.text is not null then true
         else false
       end as content_is_html
from items i
left join item_data content_html
  on content_html.item_id = i.id
 and content_html.type = 'content'
 and content_html.mime_type = 'text/html'
left join item_data description_html
  on description_html.item_id = i.id
 and description_html.type = 'description'
 and description_html.mime_type = 'text/html'
left join item_data content_text
  on content_text.item_id = i.id
 and content_text.type = 'content'
 and content_text.mime_type = 'text/plain'
left join item_data description_text
  on description_text.item_id = i.id
 and description_text.type = 'description'
 and description_text.mime_type = 'text/plain'
where i.source_id in (:v*:source-ids)
  and ((i.tagi @@ '0' and i.ts >= :unread-after)
       or (i.tagi @@ '!0' and i.ts >= :read-after)
       or i.tagi @@ '1')
--~ (when (:since-id params) "and i.id > :since-id")
--~ (when (:max-id params) "and i.id < :max-id")
--~ (when (:with-ids params) "and i.id in (:v*:with-ids)")
order by i.id
--~ (if (:descending? params) "desc" "asc")
limit :limit

-- :name fever-item-state-ids :? :*
select i.id
from items i
where i.source_id in (:v*:source-ids)
  and ((i.tagi @@ '0' and i.ts >= :unread-after)
       or (i.tagi @@ '!0' and i.ts >= :read-after)
       or i.tagi @@ '1')
  and tagi @@ :state-query::query_int
order by i.id

-- :name fever-item-selected :? :1
select exists (
  select 1 from items
  where id = :item-id and source_id in (:v*:source-ids)
) as selected

-- :name fever-mark-read :! :n
update items
set tagi = tagi - array[0]
where source_id in (:v*:source-ids)
  and ts <= :before
  and tagi @@ '0'
--~ (when (:feed-id params) "and source_id = :feed-id")

-- :name fever-total-items :? :1
select count(*) as total
from items
where source_id in (:v*:source-ids)
  and ((tagi @@ '0' and ts >= :unread-after)
       or (tagi @@ '!0' and ts >= :read-after)
       or tagi @@ '1')
