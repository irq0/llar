(ns infowarss.db.sql
  (:require
   [hugsql.core :as hugsql]))

(hugsql/def-db-fns "infowarss/db/sql/items.sql" {:quoting :ansi})
(hugsql/def-db-fns "infowarss/db/sql/sources.sql" {:quoting :ansi})
(hugsql/def-db-fns "infowarss/db/sql/analytics.sql" {:quoting :ansi})
(hugsql/def-db-fns "infowarss/db/sql/search.sql" {:quoting :ansi})
(hugsql/def-db-fns "infowarss/db/sql/tags.sql" {:quoting :ansi})
(hugsql/def-db-fns "infowarss/db/sql/item.sql" {:quoting :ansi})


