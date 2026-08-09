-- :name fever-sources :? :*
select id, key, name, type, data, updated_ts
from sources
order by id

-- :name fever-items :? :*
select i.id,
       case when i.source_id in (:v*:source-ids)
            then i.source_id
            else :queue-feed-id
       end as feed_id,
       i.title,
       i.author,
       i.ts,
       i.entry->>'url' as url,
       q.item_id is not null as is_saved,
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
left join reading_queue_items q on q.item_id = i.id
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
where ((i.source_id in (:v*:source-ids)
        and ((i.tagi @@ '0' and i.ts >= :unread-after)
             or (i.tagi @@ '!0' and i.ts >= :read-after)))
       or q.item_id is not null)
--~ (when (:since-id params) "and i.id > :since-id")
--~ (when (:max-id params) "and i.id < :max-id")
--~ (when (:with-ids params) "and i.id in (:v*:with-ids)")
order by i.id
--~ (if (:descending? params) "desc" "asc")
limit :limit

-- :name fever-item-state-ids :? :*
select i.id
from items i
left join reading_queue_items q on q.item_id = i.id
where ((i.source_id in (:v*:source-ids)
        and ((i.tagi @@ '0' and i.ts >= :unread-after)
             or (i.tagi @@ '!0' and i.ts >= :read-after)))
       or q.item_id is not null)
--~ (if (:queue-state? params) "and q.item_id is not null" "and i.tagi @@ :state-query::query_int")
order by i.id

-- :name fever-item-selected :? :1
select true as selected
from items i
left join reading_queue_items q on q.item_id = i.id
where i.id = :item-id
  and (i.source_id in (:v*:source-ids)
       or q.item_id is not null)

-- :name fever-bulk-item-ids :? :*
select id
from items
where source_id in (:v*:source-ids)
  and ts <= :before
  and tagi @@ '0'
--~ (when (:feed-id params) "and source_id = :feed-id")
order by id

-- :name fever-total-items :? :1
select count(*) as total
from items i
left join reading_queue_items q on q.item_id = i.id
where ((i.source_id in (:v*:source-ids)
        and ((i.tagi @@ '0' and i.ts >= :unread-after)
             or (i.tagi @@ '!0' and i.ts >= :read-after)))
       or q.item_id is not null)
