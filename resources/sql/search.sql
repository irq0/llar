-- :name search-item :? :*
with query as (
  select
    case :syntax
      when 'advanced' then to_tsquery('english', :query)
      when 'plain' then plainto_tsquery('english', :query)
      when 'phrase' then phraseto_tsquery('english', :query)
      else websearch_to_tsquery('english', :query)
    end as english,
    case :syntax
      when 'advanced' then to_tsquery('german', :query)
      when 'plain' then plainto_tsquery('german', :query)
      when 'phrase' then phraseto_tsquery('german', :query)
      else websearch_to_tsquery('german', :query)
    end as german
), matches as (
select
  search_index.id,
  search_index.title,
  search_index.key,
  search_index.ts,
  live_items.author,
  live_items.entry,
  live_items.type,
  live_items.nlp_nwords as nwords,
  live_items.nlp_top as "top-words",
  (select array_agg(t.tag order by t.tag)
   from unnest(live_items.tagi) tag_id
   inner join tags t on t.id = tag_id) as tags,
  :syntax as syntax,
  case search_config
    when 'german' then query.german
    else query.english
  end as q,
  search_config,
  document,
  headline_text
from search_index
inner join items live_items on live_items.id = search_index.id
cross join query
where document @@ case search_config
  when 'german' then query.german
  else query.english
end
--~ (when (:archived-only? params) "and live_items.tagi @@ (select format('(%s)', id) from tags where tag = 'archive')::query_int")
--~ (when (:with-tag params) "and live_items.tagi @@ (select format('(%s)', id) from tags where tag = :with-tag)::query_int")
--~ (when (:untagged? params) "and not exists (select 1 from unnest(live_items.tagi) tag_id inner join tags t on t.id = tag_id where t.tag not in ('archive', 'saved', 'unread', 'in-progress'))")
--~ (when (:time-ago params) "and search_index.ts > :time-ago")
--~ (when (:source-key params) "and search_index.key = :source-key")
)
select
  id,
  title,
  key,
  ts,
  author,
  entry,
  type,
  nwords,
  "top-words",
  tags,
  count(*) over () as "total-count",
  syntax,
  ts_rank_cd(document, q, 32) as rank,
  ts_rank_cd(
    to_tsvector(search_config::regconfig, COALESCE(title, '')),
    q,
    32
  ) as title_rank,
  ts_headline(
    search_config::regconfig,
    headline_text,
    q,
    'StartSel="[[[", StopSel="]]]", MaxFragments=2, MinWords=8, MaxWords=24, FragmentDelimiter=" ... "'
  ) as headline
from matches
--~ (case (:sort params) "oldest" "order by ts asc, id asc" "newest" "order by ts desc, id desc" "order by rank desc, ts desc, id desc")
limit :limit offset :offset


-- :name saved-items-tf-idf :? :raw
select
  id,
  json_agg(json_build_array(term_tf->>0, (term_tf->>1)::float * idf_top_words.ln))
from
  (select items.id,
          jsonb_array_elements(items.nlp_top->'words') as term_tf
   from items
   inner join reading_queue_items queue on queue.item_id = items.id) as i
inner join idf_top_words on term_tf->0 = idf_top_words.term
group by id


-- :name item-tf-idf-terms :? :*
select term_tf->>0 as term,
       (term_tf->>1)::float * idf_top_words.ln as score
from items,
     lateral jsonb_array_elements(nlp_top->'words') as term_tf
inner join idf_top_words on term_tf->0 = idf_top_words.term
where items.id = :item-id
  and length(term_tf->>0) > 2
order by score desc
limit 20


-- :name saved-items-tf-idf-terms :? :raw
select array_agg(foo.term)
from
  (select distinct term_tf->>0 as term,
                   (term_tf->>1)::float * idf_top_words.ln as tf_idf
   from
     (select items.id,
             jsonb_array_elements(items.nlp_top->'words') as term_tf
      from items
      inner join reading_queue_items queue on queue.item_id = items.id) as i
   inner join idf_top_words on term_tf->0 = idf_top_words.term
   where (term_tf->>1)::float > :min-tf-idf
     and length(term_tf->>0) > 4
     and not (term_tf->>0) like '%/%') as foo
