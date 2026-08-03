(ns llar.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.tools.cli :refer [parse-opts]]
   [llar.commands :as commands]
   [llar.core :as uut]
   [llar.docs.config :as docs.config]
   [llar.fetch.streaming :as streaming]
   [llar.pool :as pool]))

(deftest fetch-concurrency-resources-are-part-of-dry-startup
  (doseq [state [#'commands/command-throttle
                 #'commands/av-download-throttle
                 #'streaming/streaming-throttle
                 #'pool/source-pool
                 #'pool/item-pool]]
    (is (some #{state} uut/dry-states))))

(deftest write-docs-option-parses-output-directory
  (let [{:keys [options errors]} (parse-opts ["--write-docs" "/tmp/llar-docs"]
                                             uut/cli-options)]
    (is (nil? errors))
    (is (= "/tmp/llar-docs" (:write-docs options)))))

(deftest write-docs-delegates-to-docs-writer
  (let [called (atom nil)]
    (with-redefs [docs.config/write-static! (fn [dir]
                                              (reset! called dir)
                                              [])]
      (uut/write-docs! "/tmp/llar-docs")
      (is (= "/tmp/llar-docs" @called)))))
