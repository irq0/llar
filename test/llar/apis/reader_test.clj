(ns llar.apis.reader-test
  (:require
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
