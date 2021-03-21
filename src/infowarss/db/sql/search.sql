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
