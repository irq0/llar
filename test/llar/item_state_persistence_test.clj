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

(deftest bulk-tag-edits-use-one-set-based-delta
  (let [tag-writes (atom [])]
    (with-redefs [sql/get-items-state-for-update
                  (fn [_ _]
                    [{:id 1 :type :item-type/link :tags ["unread" "later"]}
                     {:id 2 :type :item-type/link :tags ["research"]}])
                  sql/apply-items-tag-delta
                  (fn [_ args] (swap! tag-writes conj args))
                  sql/ensure-tags (fn [& _] nil)]
      (#'modify/transition-item-state-in-tx!
       :tx [1 2] {:action :edit-tags
                  :add-tags #{:research :project-x}
                  :remove-tags #{:later}
                  :require-all? true})
      (is (= [{:item-ids [1]
               :add-tags #{"project-x" "research"}
               :remove-tags #{"later"}}
              {:item-ids [2]
               :add-tags #{"project-x"}
               :remove-tags #{}}]
             (->> @tag-writes
                  (map #(-> %
                            (update :add-tags set)
                            (update :remove-tags set)))
                  (sort-by :item-ids)))))))

(deftest required-bulk-transition-stops-before-writing-when-an-item-is-missing
  (let [writes (atom 0)]
    (with-redefs [sql/get-items-state-for-update
                  (fn [_ _] [{:id 1 :type :item-type/link :tags []}])
                  sql/apply-items-tag-delta (fn [& _] (swap! writes inc))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (#'modify/transition-item-state-in-tx!
                    :tx [1 2] {:action :edit-tags
                               :add-tags #{:research}
                               :remove-tags #{}
                               :require-all? true})))
      (is (zero? @writes)))))
