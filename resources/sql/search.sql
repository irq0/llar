-- :name search-item :? :*
select
  id,
  title,
  key,
  ts,
  ts_rank(document, to_tsquery('english', :query)) as rank
from search_index
where document @@ to_tsquery('english', :query)
--~ (when (:time-ago params) "and ts > :time-ago")
--~ (when (:source-key params) "and key = :source-key")
order by rank desc


-- :name saved-items-tf-idf :? :raw
select
  id,
  json_agg(json_build_array(term_tf->>0 , (term_tf->>1)::float * idf_top_words.ln ))
from
  ( select id, jsonb_array_elements(nlp_top->'words') as term_tf
    from items
    where tagi @@ '1'
      or (items.type = 'bookmark' and tagi @@ '0')
  ) as i
inner join
  idf_top_words on (term_tf->0 = idf_top_words.term)
group by id


-- :name saved-items-tf-idf-terms :? :raw
select
  array_agg(foo.term)
from
  ( select distinct term_tf->>0 as term, (term_tf->>1)::float * idf_top_words.ln as tf_idf
    from
      (
        select
	  id,
	  jsonb_array_elements(nlp_top->'words') as term_tf
	from items
        where tagi @@ '1'
          or (items.type = 'bookmark' and tagi @@ '0')
      ) as i
    inner join
      idf_top_words on (term_tf->0 = idf_top_words.term)
    where
      (term_tf->>1)::float > :min-tf-idf
      and length(term_tf->>0) > 4
      and not (term_tf->>0) like '%/%'
  ) as foo
