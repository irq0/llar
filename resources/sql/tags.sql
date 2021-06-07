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
set tags = tags || hstore(:v*:tags, :v*:vals)
--~ (when (:where params) "where :snip*:where")
returning tags

-- :name remove-tags :i! :*
update items
set tags = delete(tags, :v*:tags)
--~ (when (:where params) "where :snip*:where")
returning tags
