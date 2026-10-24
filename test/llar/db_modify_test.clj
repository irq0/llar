(ns llar.db-modify-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.db.modify :as uut]
   [llar.db.sql :as sql]))

(deftest oversized-nlp-lexemes-are-not-persisted
  (let [oversized-multibyte (apply str (repeat 700 "€"))
        largest-valid (apply str (repeat 2046 "a"))
        stored-params (atom nil)
        item {:type :link
              :hash "SHA-256:oversized-search-lexeme"
              :feed {}
              :meta {:source-name "Test source"
                     :source-key :test-source
                     :tags []}
              :summary {:title "Still searchable"}
              :entry {:authors []
                      :nlp {:nwords 2
                            :urls ["https://example.com" oversized-multibyte]
                            :names [largest-valid]
                            :nouns [nil "" "searchable"]
                            :verbs []
                            :top {}}}}]
    (with-redefs [sql/store-item (fn [_ params]
                                   (reset! stored-params params)
                                   [{:id 1 :hash (:hash params)}])]
      (uut/store-item-without-data! nil item {}))
    (is (= ["https://example.com"] (:nlp-urls @stored-params)))
    (is (= [largest-valid] (:nlp-names @stored-params)))
    (is (= ["searchable"] (:nlp-nouns @stored-params)))
    (is (= [] (:nlp-verbs @stored-params)))))
