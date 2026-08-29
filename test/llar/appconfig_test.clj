(ns llar.appconfig-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [llar.appconfig :as uut]
   [slingshot.slingshot :refer [try+]]))

(deftest invalid-config-explains-the-failure-without-carrying-secrets
  (let [password "database-password-that-must-not-be-logged"
        pool-config {:adapter "postgresql"
                     :server-name "database.example.test"
                     :database-name "llar"
                     :username "llar"
                     :password password}
        config {:credentials-file "/tmp"
                :runtime-config-dir "/tmp"
                :commands {}
                :api {}
                :ui {:default-list-view {}
                     :favorites []}
                :postgresql {:frontend pool-config
                             :backend pool-config}}
        failure (try+
                 (uut/verify-config config)
                 (catch [:type ::uut/config-verification-failed] error
                   error))]
    (is (= ::uut/config-verification-failed (:type failure)))
    (is (string? (:explain failure)))
    (is (not (contains? failure :config)))
    (is (not (contains? failure :spec)))
    (is (not (string/includes? (:explain failure) password)))
    (is (string/includes? (:explain failure) "--secret removed--"))))
