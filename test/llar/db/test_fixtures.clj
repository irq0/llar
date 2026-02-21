(ns llar.db.test-fixtures
  "Test fixture utilities for managing PostgreSQL testcontainers and test data."
  (:require
   [clj-test-containers.core :as tc]
   [clojure.java.io :as io]
   [clojure.test :refer [use-fixtures]]
   [digest :as digest]
   [llar.db.sql :as sql]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [llar.db.core :as db]
   [migratus.core :as migratus]
   [next.jdbc :as jdbc]))

(def ^:dynamic *test-container* nil)
(def ^:dynamic *test-db* nil)

(defn create-postgres-container
  "Creates and starts a PostgreSQL testcontainer.
  Returns the started container."
  []
  (-> (tc/create {:image-name "postgres:16"
                  :exposed-ports [5432]
                  :env-vars {"POSTGRES_USER" "postgres"
                             "POSTGRES_PASSWORD" "test"
                             "POSTGRES_DB" "llar_test"}
                  :wait-for {:wait-strategy :port}})
      (tc/start!)))

(defn get-db-config
  "Gets database configuration map for testcontainer."
  [container]
  (let [host (:host container)
        port (get (:mapped-ports container) 5432)]
    {:jdbc-url (format "jdbc:postgresql://%s:%s/llar_test" host port)
     :username "postgres"
     :password "test"
     :auto-commit true
     :read-only false
     :connection-timeout 30000
     :validation-timeout 5000
     :idle-timeout 600000
     :max-lifetime 1800000
     :minimum-idle 2
     :maximum-pool-size 5
     :register-mbeans false
     :pool-name "llar-test-pool"}))

(defn create-test-datastore
  "Creates a PostgresqlDataStore instance connected to the test container."
  [container]
  (db/make-postgresql-ds-for-testing (get-db-config container)))

(defn get-migratus-config
  "Gets Migratus configuration for test database."
  [container]
  {:store :database
   :migration-dir "migrations/"
   :init-script "init.sql"
   :db {:dbtype "postgresql"
        :dbname "llar_test"
        :host (:host container)
        :port (get (:mapped-ports container) 5432)
        :user "postgres"
        :password "test"}})

(defn run-migrations!
  "Applies all Migratus migrations to test database."
  [container]
  (let [config (get-migratus-config container)]
    (migratus/init config)
    (migratus/migrate config)))

(defn clear-test-data!
  "Truncates all tables in the test database, preserving schema.
  Useful for resetting state between tests."
  [db]
  (jdbc/execute! db ["TRUNCATE TABLE item_data, items, sources, tags RESTART IDENTITY CASCADE"]))

(defn seed-default-tags!
  "Seeds the default tags (unread, saved, in-progress, archive) into the test database with specific IDs."
  [db]
  (jdbc/execute! db ["INSERT INTO tags (id, tag) VALUES (0, 'unread'), (1, 'saved'), (2, 'in-progress'), (3, 'archive') ON CONFLICT DO NOTHING"])
  (jdbc/execute! db ["SELECT setval('tags_id_seq', 100, false)"]))

(defn dump-db [db]
  (log/info (jdbc/execute! db ["select * from items"])))

(defn create-test-source
  "Creates a test source with minimal required fields.
  Returns the created source row."
  [db & {:keys [key name type data]
         :or {key "test-source"
              name "Test Source"
              type :item-type/link
              data {}}}]
  (first
   (jdbc/execute! db
                  ["INSERT INTO sources (key, name, type, data) VALUES (?, ?, ?, ?) RETURNING *"
                   key name type data]
                  {:return-keys true
                   :builder-fn next.jdbc.result-set/as-unqualified-lower-maps})))

(defn create-test-tag
  "Creates a test tag. Returns the created tag row."
  [db tag-name]
  (first
   (jdbc/execute! db
                  ["INSERT INTO tags (tag) VALUES (?) ON CONFLICT (tag) DO UPDATE SET tag = EXCLUDED.tag RETURNING *"
                   (name tag-name)]
                  {:return-keys true
                   :builder-fn next.jdbc.result-set/as-unqualified-lower-maps})))

(defn create-test-item
  "Creates a test item with minimal required fields.
  Returns the created item row."
  [db & {:keys [src-name hash title ts author tags entry type overwrite?]
         :or {src-name "testsource"
              type :item-type/link
              hash (str "test-hash-" (rand-int 1000000))
              title "Test Item"
              ts (time/zoned-date-time)
              author "Test Author"
              tags #{}
              entry {:url "https://example.com"}
              overwrite? false}}]
  ;; Ensure tags exist before storing item
  (when (seq tags)
    (sql/ensure-tags db {:tags (map (fn [kw] [(name kw)]) tags)}))
  (first (sql/store-item db {:source {:name (str "test-" src-name)
                                      :data {}
                                      :key (str "test-" src-name)
                                      :type type}
                             :hash (str "SHA-256:" (digest/sha-256 hash))
                             :ts ts
                             :title title
                             :author author
                             :type type
                             :entry entry
                             :tags (into [] (map name tags))
                             :nlp-nwords -1
                             :nlp-urls []
                             :nlp-names []
                             :nlp-nouns []
                             :nlp-verbs []
                             :nlp-top {}
                             :on-conflict (if overwrite?
                                            (sql/conflict-items-overwrite-snip)
                                            (sql/conflict-items-ignore-dupe-snip))})))

(defn create-test-item-data
  "Creates item_data entry for an item.
  Returns the created item_data row."
  [db & {:keys [item-id mime-type type data text]
         :or {mime-type "text/plain"
              type :item-data-type/content}}]
  (first
   (jdbc/execute! db
                  ["INSERT INTO item_data (item_id, mime_type, type, text)
                    VALUES (?, ?, ?, ?)
                    RETURNING *"
                   item-id mime-type type text]
                  {:return-keys true
                   :builder-fn next.jdbc.result-set/as-unqualified-lower-maps})))

(defn with-test-db-impl
  "Implementation of test database lifecycle management.
  Starts container, runs migrations, executes test-fn, then stops container."
  [test-fn]
  (log/info "Starting PostgreSQL testcontainer...")
  (let [container (create-postgres-container)]
    (try
      (log/info "Running migrations...")
      (run-migrations! container)
      (let [db (create-test-datastore container)]
        (log/info "Test database ready")
        (binding [*test-container* container
                  *test-db* db]
          (test-fn)))
      (finally
        (log/info "Stopping testcontainer...")
        (tc/stop! container)))))

(defmacro with-test-db
  "Macro for managing test database lifecycle.

  Usage in tests:

  (use-fixtures :once with-test-db-fixture)

  (deftest my-test
    ;; *test-db* is available here
    (is (some? *test-db*)))"
  [& body]
  `(with-test-db-impl (fn [] ~@body)))

(defn with-test-db-fixture
  "Test fixture for use with clojure.test use-fixtures.
  Manages container lifecycle for entire test namespace.

  Usage:
  (use-fixtures :once with-test-db-fixture)"
  [test-fn]
  (with-test-db (test-fn)))

(defn with-clean-db-fixture
  "Test fixture that clears all data before each test.
  Use with :each to ensure test isolation.

  Usage:
  (use-fixtures :each with-clean-db-fixture)"
  [test-fn]
  (clear-test-data! *test-db*)
  (seed-default-tags! *test-db*)
  (test-fn))
