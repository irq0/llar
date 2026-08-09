(ns llar.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.tools.cli :refer [parse-opts]]
   [llar.commands :as commands]
   [llar.core :as uut]
   [llar.docs.config :as docs.config]
   [llar.fetch.streaming :as streaming]
   [llar.pool :as pool]
   [llar.store :as store]
   [mount.core :as mount]
   [migratus.core :as migratus]))

(deftest fetch-concurrency-resources-are-part-of-dry-startup
  (doseq [state [#'commands/command-throttle
                 #'commands/av-download-throttle
                 #'streaming/streaming-throttle
                 #'pool/source-pool
                 #'pool/item-pool]]
    (is (some #{state} uut/dry-states))))

(deftest runtime-migrates-before-starting-query-serving-states
  (let [events (atom [])
        application-states [:reader :scheduler]]
    (with-redefs [mount/start (fn [states]
                                (swap! events conj [:start states]))
                  uut/migrate-db! (fn [_]
                                    (swap! events conj [:migrate]))]
      (uut/start-runtime! application-states))
    (is (= [[:start #'store/backend-db]
            [:migrate]
            [:start application-states]]
           @events))
    (is (not-any? #{#'store/backend-db} uut/dry-states))))

(deftest write-docs-option-parses-output-directory
  (let [{:keys [options errors]} (parse-opts ["--write-docs" "/tmp/llar-docs"]
                                             uut/cli-options)]
    (is (nil? errors))
    (is (= "/tmp/llar-docs" (:write-docs options)))))

(deftest rollback-db-option-parses
  (let [{:keys [options errors]} (parse-opts ["--rollback-db"] uut/cli-options)]
    (is (nil? errors))
    (is (true? (:rollback-db options)))))

(deftest rollback-db-rolls-back-last-migration
  (let [db (Object.)
        called-with (atom nil)]
    (with-redefs [migratus/rollback #(reset! called-with %)]
      (uut/rollback-db! db))
    (is (= {:store :database
            :db db
            :migration-dir "migrations/"
            :command-separator "--;;"}
           @called-with))))

(deftest write-docs-delegates-to-docs-writer
  (let [called (atom nil)]
    (with-redefs [docs.config/write-static! (fn [dir]
                                              (reset! called dir)
                                              [])]
      (uut/write-docs! "/tmp/llar-docs")
      (is (= "/tmp/llar-docs" @called)))))
