-- :name search-item :? :*
with query as not materialized (
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
), index_matches as (
  -- Keep the query operand independent of each search_index row. A single
  -- CASE on search_config prevents PostgreSQL from constructing a GIN scan
  -- key and turns every search into a sequential scan of the materialized
  -- view. The fixed-language branches both use search_index_document_idx.
  select
    search_index.id,
    search_index.title,
    search_index.key,
    search_index.ts,
    search_index.search_config,
    search_index.document,
    search_index.headline_text,
    query.english as q
  from search_index
  cross join query
  where search_index.search_config = 'english'
    and search_index.document @@ query.english
  --~ (when (:time-ago params) "and search_index.ts > :time-ago")
  --~ (when (:source-key params) "and search_index.key = :source-key")

  union all

  select
    search_index.id,
    search_index.title,
    search_index.key,
    search_index.ts,
    search_index.search_config,
    search_index.document,
    search_index.headline_text,
    query.german as q
  from search_index
  cross join query
  where search_index.search_config = 'german'
    and search_index.document @@ query.german
  --~ (when (:time-ago params) "and search_index.ts > :time-ago")
  --~ (when (:source-key params) "and search_index.key = :source-key")
), filtered_matches as (
  select index_matches.*
  from index_matches
  inner join items live_items on live_items.id = index_matches.id
  where true
  --~ (when (:archived-only? params) "and live_items.tagi @@ (select format('(%s)', id) from tags where tag = 'archive')::query_int")
  --~ (when (:with-tag params) "and live_items.tagi @@ (select format('(%s)', id) from tags where tag = :with-tag)::query_int")
  --~ (when (:untagged? params) "and not exists (select 1 from unnest(live_items.tagi) tag_id inner join tags t on t.id = tag_id where t.tag not in ('archive', 'saved', 'unread', 'in-progress'))")
), selected_matches as materialized (
  select
    id,
    title,
    key,
    ts,
    search_config,
    headline_text,
    q,
--~ (when (:with-total-count? params) "    count(*) over () as total_count,")
    ts_rank_cd(document, q, 32) as rank
  from filtered_matches
  --~ (case (:sort params) "oldest" "order by ts asc, id asc" "newest" "order by ts desc, id desc" "order by rank desc, ts desc, id desc")
  limit :limit offset :offset
)
select
  selected_matches.id,
  selected_matches.title,
  selected_matches.key,
  selected_matches.ts,
  live_items.author,
  live_items.entry,
  live_items.type,
  live_items.nlp_nwords as nwords,
  live_items.nlp_top as "top-words",
  (select array_agg(t.tag order by t.tag)
   from unnest(live_items.tagi) tag_id
   inner join tags t on t.id = tag_id) as tags,
--~ (when (:with-total-count? params) "  selected_matches.total_count as \"total-count\",")
  :syntax as syntax,
  selected_matches.rank,
  ts_rank_cd(
    to_tsvector(selected_matches.search_config::regconfig, COALESCE(selected_matches.title, '')),
    selected_matches.q,
    32
  ) as title_rank,
  ts_headline(
    selected_matches.search_config::regconfig,
    selected_matches.headline_text,
    selected_matches.q,
    'StartSel="[[[", StopSel="]]]", MaxFragments=2, MinWords=8, MaxWords=24, FragmentDelimiter=" ... "'
  ) as headline
from selected_matches
inner join items live_items on live_items.id = selected_matches.id
--~ (case (:sort params) "oldest" "order by selected_matches.ts asc, selected_matches.id asc" "newest" "order by selected_matches.ts desc, selected_matches.id desc" "order by selected_matches.rank desc, selected_matches.ts desc, selected_matches.id desc")


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
