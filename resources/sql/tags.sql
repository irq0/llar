-- :snip tag-cond-by-id
id = :id

-- :snip tag-cond-by-source-id
source_id = :id

-- :snip tag-cond-by-source-id-in
source_id in (:v*:ids)

-- :snip tag-cond-le-ts
ts <= (:v:ts)

-- :name set-tags :i! :*
update items
set tagi = tagi | (select array_agg(id) from tags where tag = ANY(:v:tags))
--~ (when (:where params) "where :snip*:where")
returning tags

-- :name remove-tags :i! :*
update items
set tagi = tagi - (select array_agg(id) from tags where tag = ANY(:v:tags))
--~ (when (:where params) "where :snip*:where")
returning tags
