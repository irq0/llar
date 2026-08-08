(ns llar.db.search-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.db.search :as search]
   [llar.db.sql :as sql]))

(deftest related-items-excludes-seed-thresholds-and-caps-sources
  (with-redefs [sql/get-item-by-id
                (constantly {:id 42
                             :title "Berlin election result"
                             :names ["Berlin"]
                             :nouns ["election"]
                             :top-words {:words [["result" 2.0]]}})
                sql/search-item
                (constantly [{:id 42 :key "a" :rank 1.0}
                             {:id 1 :key "a" :rank 0.4
                              :headline "[[[Berlin]]] [[[election]]] [[[berlin]]]"}
                             {:id 2 :key "a" :rank 0.3}
                             {:id 3 :key "a" :rank 0.2}
                             {:id 4 :key "a" :rank 0.1}
                             {:id 5 :key "b" :rank 0.009}
                             {:id 6 :key "b" :rank 0.2}])
                sql/item-tf-idf-terms
                (constantly [{:term "result" :score 2.0}])]
    (let [{:keys [query results]} (search/related-items :db 42)]
      (is (re-find #"Berlin" query))
      (is (= [1 2 3 6] (mapv :id results)))
      (is (= 1.0 (:relative-score (first results))))
      (is (= 0.5 (:relative-score (last results))))
      (is (= ["Berlin" "election"] (:matched-terms (first results)))))))
