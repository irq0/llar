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
