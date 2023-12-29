(ns u1f596.db.sql
  (:gen-class)
  (:require
   [hugsql.core :as hugsql]
   [clojure.java.io :as io]))

(declare
 get-item-count-of-source
 get-item-count-by-tag-of-source
 get-item-count-unread-today
 get-sources-with-item-tags-count
 get-items-by-tag
 item-select-default-snip
 item-select-with-data-snip
 item-from-join-default-snip
 item-from-join-with-data-table-snip
 item-from-join-with-preview-data-snip
 cond-before
 cond-with-source-keys
 cond-with-source-ids
 cond-with-tag
 cond-with-type
 get-items-recent
 get-item-by-id
 store-item
 store-item-data
 get-sources
 resolve-source-keys-to-ids
 set-tags
 remove-tags
 ensure-tags
 get-word-count-groups
 get-tag-stats
 get-tags
 get-type-stats
 get-table-row-counts)

(hugsql/def-db-fns (io/resource "sql/items.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/sources.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/analytics.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/search.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/tags.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/item.sql") {:quoting :ansi})
