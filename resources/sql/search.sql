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
  id,
  title,
  key,
  ts,
  :syntax as syntax,
  case search_config
    when 'german' then query.german
    else query.english
  end as q,
  search_config,
  document,
  headline_text
from search_index, query
where document @@ case search_config
  when 'german' then query.german
  else query.english
end
--~ (when (:time-ago params) "and ts > :time-ago")
--~ (when (:source-key params) "and key = :source-key")
)
select
  id,
  title,
  key,
  ts,
  syntax,
  ts_rank_cd(document, q, 32) as rank,
  ts_headline(
    search_config::regconfig,
    headline_text,
    q,
    'StartSel="[[[", StopSel="]]]", MaxFragments=2, MinWords=8, MaxWords=24, FragmentDelimiter=" ... "'
  ) as headline
from matches
order by rank desc, ts desc
limit 100


-- :name saved-items-tf-idf :? :raw
select
  id,
  json_agg(json_build_array(term_tf->>0 , (term_tf->>1)::float * idf_top_words.ln ))
from
  ( select id, jsonb_array_elements(nlp_top->'words') as term_tf
    from items
    where tagi @@ '1'
      or tagi @@ '2'
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
          or tagi @@ '2'
          or (items.type = 'bookmark' and tagi @@ '0')
      ) as i
    inner join
      idf_top_words on (term_tf->0 = idf_top_words.term)
    where
      (term_tf->>1)::float > :min-tf-idf
      and length(term_tf->>0) > 4
      and not (term_tf->>0) like '%/%'
  ) as foo
