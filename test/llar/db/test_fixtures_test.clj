(ns llar.db.test-fixtures-test
  (:require
   [clj-test-containers.core :as tc]
   [clojure.test :refer [deftest is]]
   [llar.db.test-fixtures :as uut])
  (:import
   [org.testcontainers.containers GenericContainer]))

(deftest postgres-container-waits-for-final-readiness
  (with-redefs [tc/create identity
                tc/start! identity]
    (is (= {:wait-strategy :log
            :message "database system is ready to accept connections"
            :times 2
            :startup-timeout 60}
           (:wait-for (uut/create-postgres-container))))))

(deftest postgres-container-configures-the-java-wrapper
  (let [initialize tc/init
        container (proxy [GenericContainer] ["postgres:16"]
                    (getHost [] "localhost"))
        configured (with-redefs [tc/init #(initialize (assoc % :container container))
                                 tc/start! identity]
                     (uut/create-postgres-container))]
    (is (= "localhost" (:host configured)))
    (is (= [5432] (:exposed-ports configured)))
    (is (= {"POSTGRES_USER" "postgres"
            "POSTGRES_PASSWORD" "test"
            "POSTGRES_DB" "llar_test"}
           (:env-vars configured)))
    (is (= ".*database system is ready to accept connections.*\\n"
           (:wait-for-log-message configured)))))
