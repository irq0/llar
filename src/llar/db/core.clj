(ns llar.db.core
  (:require
   [cheshire.generate :as json :refer [encode-str]]
   [clojure.java.jdbc :as j]
   [clojure.string :as string]
   [digest]
   [hikari-cp.core :as hikari]
   [mpg.core :as mpg])
  (:import
   (org.bovinegenius.exploding_fish UniformResourceIdentifier Uri)))

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
  Uri
  (sql-value [url]
    (str url)))

(extend-protocol j/ISQLValue
  UniformResourceIdentifier
  (sql-value [url]
    (str url)))

(json/add-encoder Uri encode-str)

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
