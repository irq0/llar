(ns u1f596.db.sql
  (:gen-class)
  (:require
   [hugsql.core :as hugsql]
   [clojure.java.io :as io]))

(hugsql/def-db-fns (io/resource "sql/items.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/sources.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/analytics.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/search.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/tags.sql") {:quoting :ansi})
(hugsql/def-db-fns (io/resource "sql/item.sql") {:quoting :ansi})
