(ns llar.db.core
  (:require
   [cheshire.core :as json]
   [cheshire.generate :refer [encode-str add-encoder]]
   [clojure.string :as string]
   [digest]
   [hikari-cp.core :as hikari]
   [java-time.api :as time]
   [next.jdbc.prepare :as prepare]
   [next.jdbc.protocols :as p]
   [next.jdbc.result-set :as rs])
  (:import
   (org.bovinegenius.exploding_fish UniformResourceIdentifier Uri)))

(defn kw->pgenum [kw]
  (let [type (some-> (namespace kw)
                     (string/replace "-" "_"))
        value (name kw)]
    (doto (org.postgresql.util.PGobject.)
      (.setType type)
      (.setValue value))))

;; Write: Clojure → PostgreSQL
(extend-protocol prepare/SettableParameter
  clojure.lang.Keyword
  (set-parameter [kw ^java.sql.PreparedStatement stmt ^long i]
    (.setObject stmt i (kw->pgenum kw))))

(extend-protocol prepare/SettableParameter
  Uri
  (set-parameter [url ^java.sql.PreparedStatement stmt ^long i]
    (.setString stmt i (str url))))

(extend-protocol prepare/SettableParameter
  UniformResourceIdentifier
  (set-parameter [url ^java.sql.PreparedStatement stmt ^long i]
    (.setString stmt i (str url))))

(extend-protocol prepare/SettableParameter
  java.time.ZonedDateTime
  (set-parameter [zdt ^java.sql.PreparedStatement stmt ^long i]
    (.setTimestamp stmt i (java.sql.Timestamp/from (time/instant zdt)))))

(extend-protocol prepare/SettableParameter
  clojure.lang.IPersistentVector
  (set-parameter [v ^java.sql.PreparedStatement stmt ^long i]
    (let [conn (.getConnection stmt)
          arr (.createArrayOf conn "text" (to-array v))]
      (.setArray stmt i arr))))

(extend-protocol prepare/SettableParameter
  clojure.lang.IPersistentMap
  (set-parameter [m ^java.sql.PreparedStatement stmt ^long i]
    (let [json-str (json/generate-string m)
          obj (doto (org.postgresql.util.PGobject.)
                (.setType "jsonb")
                (.setValue json-str))]
      (.setObject stmt i obj))))

(add-encoder Uri encode-str)

(def +schema-enums+
  "A set of all PostgreSQL enums in schema.sql. Used to convert
  enum-values back into Clojure keywords."
  #{"item_type"})

;; Read: PostgreSQL → Clojure
(extend-protocol rs/ReadableColumn
  String
  (read-column-by-label [val label]
    val)
  (read-column-by-index [val rsmeta idx]
    (let [type (.getColumnTypeName rsmeta idx)]
      (if (contains? +schema-enums+ type)
        (keyword (string/replace type "_" "-") val)
        val)))

  org.postgresql.jdbc.PgArray
  (read-column-by-label [val label]
    (vec (.getArray val)))
  (read-column-by-index [val rsmeta idx]
    (vec (.getArray val)))

  org.postgresql.util.PGobject
  (read-column-by-label [val label]
    (let [type (.getType val)
          value (.getValue val)]
      (case type
        "jsonb" (json/parse-string value true)
        "json" (json/parse-string value true)
        "hstore" (json/parse-string value true)
        val)))
  (read-column-by-index [val rsmeta idx]
    (let [type (.getType val)
          value (.getValue val)]
      (case type
        "jsonb" (json/parse-string value true)
        "json" (json/parse-string value true)
        "hstore" (json/parse-string value true)
        val)))

  java.sql.Timestamp
  (read-column-by-label [val label]
    (-> val (time/zoned-date-time "UTC")))
  (read-column-by-index [val rsmeta idx]
    (-> val (time/zoned-date-time "UTC"))))

(defrecord PostgresqlDataStore
           [datasource]
  Object
  (toString [this] (format "PostgresqlDataSource{->%s}"
                           (.toString (:datasource this))))

  ;; Make PostgresqlDataStore work as a connectable for next.jdbc
  p/Sourceable
  (get-datasource [this] datasource))

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
