(ns llar.item-state-persistence-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.db.modify :as modify]
   [llar.db.sql :as sql]))

(deftest bulk-transitions-use-set-based-writes
  (let [lock-calls (atom [])
        tag-writes (atom [])]
    (with-redefs [sql/get-items-state-for-update
                  (fn [_ args]
                    (swap! lock-calls conj args)
                    [{:id 1 :type :item-type/link :tags ["unread" "saved"]}
                     {:id 2 :type :item-type/link :tags ["unread" "saved"]}
                     {:id 3 :type :item-type/link :tags ["saved"]}])
                  sql/apply-items-tag-delta
                  (fn [_ args] (swap! tag-writes conj args))
                  sql/ensure-tags
                  (fn [& _] (throw (ex-info "not expected" {})))
                  sql/set-items-reading-checkpoint
                  (fn [& _] (throw (ex-info "not expected" {})))
                  sql/clear-items-reading-checkpoint
                  (fn [& _] (throw (ex-info "not expected" {})))]
      (let [result (#'modify/transition-item-state-in-tx! :tx [3 2 1 2] :seen)]
        (is (= [{:item-ids [1 2 3]}] @lock-calls))
        (is (= [{:item-ids [1 2]
                 :add-tags []
                 :remove-tags ["unread"]}]
               @tag-writes))
        (is (= [1 2 3] (mapv :id result)))))))
