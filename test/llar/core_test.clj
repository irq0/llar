(ns llar.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.tools.cli :refer [parse-opts]]
   [llar.core :as uut]
   [llar.docs.config :as docs.config]))

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
