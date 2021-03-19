(ns infowarss.db.core
  (:require
   [digest]
   [java-time :as time]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.jdbc :as j]
   [mpg.core :as mpg]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [byte-streams :refer [to-byte-buffer]]
   [cheshire.core :refer :all]
   [mount.core :refer [defstate]]
   [org.bovinegenius [exploding-fish :as uri]]
   [hikari-cp.core :as hikari]
   [hugsql.core :as hugsql]
   [cheshire.generate :as json :refer [encode-str]]))

(def config (:postgresql (edn/read-string (slurp (io/resource "config.edn")))))

(def ^:dynamic *db*
  (merge {:dbtype "postgresql"} config))

(mpg/patch {:default-map :hstore})

(defstate db
  :start {:datasource (hikari/make-datasource config)})

(defn check-connectivity []
  (log/infof "Database ready. %s items, %s sources"
             (:count (first (j/query db ["select count(*) from items"])))
             (:count (first (j/query db ["select count(*) from sources"])))))

(defn kw->pgenum [kw]
  (let [type (some-> (namespace kw)
                     (string/replace "-" "_"))
        value (name kw)]
    (doto (org.postgresql.util.PGobject.)
      (.setType type)
      (.setValue value))))

(extend-protocol j/ISQLValue
  clojure.lang.Keyword
  (sql-value [kw]
    (kw->pgenum kw)))

(extend-protocol j/ISQLValue
  org.bovinegenius.exploding_fish.Uri
  (sql-value [url]
    (str url)))
  
(extend-protocol j/ISQLValue
  org.bovinegenius.exploding_fish.UniformResourceIdentifier
  (sql-value [url]
    (str url)))

(json/add-encoder org.bovinegenius.exploding_fish.Uri encode-str)

(def +schema-enums+
  "A set of all PostgreSQL enums in schema.sql. Used to convert
  enum-values back into Clojure keywords."
  #{"item_type"})

(extend-type String
  clojure.java.jdbc/IResultSetReadColumn
  (result-set-read-column [val rsmeta idx]
    (let [type (.getColumnTypeName rsmeta idx)]
      (if (contains? +schema-enums+ type)
        (keyword (string/replace type "_" "-") val)
        val))))
