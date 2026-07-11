(ns llar.db.fever-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [java-time.api :as time]
   [llar.db.sql :as sql]
   [llar.db.test-fixtures :refer [*test-db* create-test-item
                                  create-test-item-data
                                  with-clean-db-fixture
                                  with-test-db-fixture]]))

(use-fixtures :once with-test-db-fixture)
(use-fixtures :each with-clean-db-fixture)

(deftest fever-queries-project-and-bound-items
  (let [recent (create-test-item *test-db*
                                 :src-name "mobile"
                                 :hash "fever-recent"
                                 :title "Recent"
                                 :tags #{:unread :saved})
        old (create-test-item *test-db*
                              :src-name "mobile"
                              :hash "fever-old"
                              :title "Old unread"
                              :ts (time/minus (time/zoned-date-time) (time/days 60))
                              :tags #{:unread})
        source-id (:id (first (sql/fever-sources *test-db*)))]
    (create-test-item-data *test-db*
                           :item-id (:id recent)
                           :mime-type "text/html"
                           :text "<p>Full article</p>")
    (let [args {:source-ids [source-id]
                :queue-feed-id 2147483647
                :unread-after (time/minus (time/zoned-date-time) (time/days 30))
                :read-after (time/minus (time/zoned-date-time) (time/days 10))
                :limit 50}
          items (sql/fever-items *test-db* args)]
      (is (= [(:id recent)] (mapv :id items)))
      (is (= "<p>Full article</p>" (:content (first items))))
      (is (:is_saved (first items)))
      (is (not (:is_read (first items))))
      (is (= [(:id recent)]
             (mapv :id (sql/fever-item-state-ids
                        *test-db*
                        (assoc (dissoc args :limit) :state-query "0")))))
      (is (= [(:id recent)]
             (mapv :id (sql/fever-items
                        *test-db*
                        (assoc args :with-ids [(:id recent) (:id old)]))))))))

(deftest fever-queue-includes-global-reader-queue-items
  (let [mobile (create-test-item *test-db*
                                 :src-name "mobile"
                                 :hash "mobile-item"
                                 :tags #{:unread})
        saved (create-test-item *test-db*
                                :src-name "other"
                                :hash "global-saved"
                                :tags #{:saved})
        in-progress (create-test-item *test-db*
                                      :src-name "other"
                                      :hash "global-progress"
                                      :tags #{:in-progress})
        bookmark (create-test-item *test-db*
                                   :src-name "bookmarks"
                                   :hash "global-bookmark"
                                   :type :item-type/bookmark
                                   :tags #{:unread})
        read-bookmark (create-test-item *test-db*
                                        :src-name "bookmarks"
                                        :hash "read-bookmark"
                                        :type :item-type/bookmark)
        unrelated (create-test-item *test-db*
                                    :src-name "other"
                                    :hash "unrelated"
                                    :tags #{:unread})
        mobile-source-id (:id (first (filter #(= "test-mobile" (:key %))
                                             (sql/fever-sources *test-db*))))
        bounds {:source-ids [mobile-source-id]
                :queue-feed-id 2147483647
                :unread-after (time/minus (time/zoned-date-time) (time/days 30))
                :read-after (time/minus (time/zoned-date-time) (time/days 10))}
        items (sql/fever-items *test-db* (assoc bounds :limit 50))
        by-id (into {} (map (juxt :id identity)) items)
        queue-ids #{(:id saved) (:id in-progress) (:id bookmark)}]
    (is (= (conj queue-ids (:id mobile)) (set (keys by-id))))
    (is (= mobile-source-id (get-in by-id [(:id mobile) :feed_id])))
    (is (every? #(= 2147483647 (get-in by-id [% :feed_id])) queue-ids))
    (is (every? #(true? (get-in by-id [% :is_saved])) queue-ids))
    (is (not (contains? by-id (:id read-bookmark))))
    (is (not (contains? by-id (:id unrelated))))
    (is (= queue-ids
           (set (map :id (sql/fever-item-state-ids
                          *test-db*
                          (assoc bounds :state-query "1" :queue-state? true))))))
    (is (:selected (sql/fever-item-selected
                    *test-db*
                    {:source-ids [mobile-source-id] :item-id (:id saved)})))
    (is (nil? (sql/fever-item-selected
               *test-db*
               {:source-ids [mobile-source-id] :item-id (:id unrelated)})))
    (is (= 4 (:total (sql/fever-total-items *test-db* bounds))))))
