(ns infowarss.db.core
  (:require
   [digest]
   [infowarss.db.query]
   [infowarss.db.modify]
   [infowarss.db.search]
   [java-time :as time]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.jdbc :as j]
   [mpg.core :as mpg]
   [byte-streams :refer [to-byte-buffer]]
   [cheshire.core :refer :all]
   [org.bovinegenius [exploding-fish :as uri]]
   [hikari-cp.core :as hikari]
   [hugsql.core :as hugsql]
   [cheshire.generate :as json :refer [encode-str]]))

(mpg/patch {:default-map :hstore})

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

(defrecord PostgresqlDataStore
    [datasource]
  Object
  (toString [this] (format "PostgresqlDataSource{->%s}"
                           (.toString (:datasource this)))))


(defn make-postgresql-pooled-datastore [config]
  (->PostgresqlDataStore (hikari/make-datasource config)))

(defn make-postgresql-datastore [dbspec]
  (map->PostgresqlDataStore dbspec))

(defn make-postgresql-dbspec [config]
  {:dbtype (:adapter config)
   :dbname (:database-name config)
   :host (:server-name config)
   :user (:username config)
   :password (:password config)})
