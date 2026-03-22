-- :name get-annotations-for-item :? :*
SELECT id, item_id, selector, body, created_ts
FROM annotations
WHERE item_id = :item-id
ORDER BY created_ts

-- :name get-annotation-by-id :? :1
SELECT id, item_id, selector, body, created_ts
FROM annotations
WHERE id = :id

-- :name create-annotation :<! :1
INSERT INTO annotations (item_id, selector, body)
VALUES (:item-id, :selector, :body)
RETURNING *

-- :name delete-annotation :! :n
DELETE FROM annotations
WHERE id = :id

-- :name count-annotations-for-item :? :1
SELECT count(*) AS count
FROM annotations
WHERE item_id = :item-id
