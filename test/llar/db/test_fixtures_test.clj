(ns llar.db.test-fixtures-test
  (:require
   [clj-test-containers.core :as tc]
   [clojure.test :refer [deftest is]]
   [llar.db.test-fixtures :as uut]))

(deftest postgres-container-waits-for-final-readiness
  (with-redefs [tc/create identity
                tc/start! identity]
    (is (= {:wait-strategy :log
            :message "database system is ready to accept connections"
            :times 2
            :startup-timeout 60}
           (:wait-for (uut/create-postgres-container))))))
