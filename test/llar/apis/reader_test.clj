(ns llar.apis.reader-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [llar.apis.reader :as uut]
   [llar.rc :as rc]))

(deftest list-style-uses-rc-defaults
  (with-redefs [rc/rc (fn [path]
                        (when (= [:reader :default-list-view :blog] path)
                          :headlines))]
    (is (= :headlines (uut/get-list-style {:group-item :blog})))
    (is (= :gallery (uut/get-list-style {:group-item :blog
                                         :list-style :gallery})))))

(deftest ranked-query-args-use-rc-ranking
  (with-redefs [rc/rc (fn [path]
                        (when (= [:reader :ranking] path)
                          {:highlight-boost-hours 6
                           :rarity-boost-cap-hours 12}))]
    (let [args (#'uut/build-items-query-args {:mode :list-items} :ranked)]
      (is (= 6 (:highlight-boost args)))
      (is (= 12 (:rarity-cap args)))
      (is (= :ranked (:sort-order args))))))

(deftest reading-queue-reasons
  (is (= [:saved]
         (#'uut/queue-item-reasons {:tags ["saved"]
                                    :type :item-type/link})))
  (is (= [:in-progress]
         (#'uut/queue-item-reasons {:tags ["in-progress"]
                                    :type :item-type/link})))
  (is (= [:unread-bookmark]
         (#'uut/queue-item-reasons {:tags ["unread"]
                                    :type :item-type/bookmark})))
  (is (= [:saved :unread-bookmark]
         (#'uut/queue-item-reasons {:tags ["saved" "unread"]
                                    :type :item-type/bookmark})))
  (is (= []
         (#'uut/queue-item-reasons {:tags ["unread" "highlight"]
                                    :type :item-type/link}))))

(deftest reading-queue-filters
  (let [saved {:tags ["saved"] :type :item-type/link}
        in-progress {:tags ["in-progress"] :type :item-type/link}
        unread-bookmark {:tags ["unread"] :type :item-type/bookmark}
        read-bookmark {:tags [] :type :item-type/bookmark}
        highlighted {:tags ["highlight"] :type :item-type/link}]
    (is (#'uut/queue-item-matches-filter? nil saved))
    (is (not (#'uut/queue-item-matches-filter? nil read-bookmark)))
    (is (not (#'uut/queue-item-matches-filter? nil highlighted)))
    (is (#'uut/queue-item-matches-filter? :saved saved))
    (is (#'uut/queue-item-matches-filter? :in-progress in-progress))
    (is (#'uut/queue-item-matches-filter? :unread-bookmarks unread-bookmark))
    (is (#'uut/queue-item-matches-filter? :unread unread-bookmark))
    (is (not (#'uut/queue-item-matches-filter? :saved unread-bookmark)))
    (is (not (#'uut/queue-item-matches-filter? :unread-bookmarks read-bookmark)))
    (is (not (#'uut/queue-item-matches-filter? :saved highlighted)))))

(deftest reading-queue-stats
  (is (= {:total 4
          :saved 2
          :in-progress 1
          :unread-bookmarks 1
          :unread 2}
         (#'uut/queue-stats [{:tags ["saved"] :type :item-type/link}
                             {:tags ["saved" "unread"] :type :item-type/link}
                             {:tags ["in-progress"] :type :item-type/link}
                             {:tags ["unread"] :type :item-type/bookmark}
                             {:tags [] :type :item-type/bookmark}]))))

(deftest reading-queue-sql-sources-saved-in-progress-and-unread-bookmarks
  (let [sql (slurp (io/resource "sql/search.sql"))]
    (is (re-find #"tagi @@ '1'" sql))
    (is (re-find #"tagi @@ '2'" sql))
    (is (re-find #"items\.type = 'bookmark' and tagi @@ '0'" sql))))
