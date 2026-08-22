(ns llar.auth-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llar.auth :as auth]))

(deftest bearer-authorization-header-parsing
  (testing "Bearer headers use case-insensitive lookup"
    (is (= "token-value"
           (auth/bearer-token
            {:headers {"Authorization" "Bearer token-value"}}))))
  (testing "the scheme is case insensitive and malformed headers are rejected"
    (is (= "token-value"
           (auth/bearer-token
            {:headers {"authorization" "bearer token-value"}})))
    (is (nil? (auth/bearer-token
               {:headers {"authorization" "Basic token-value"}})))
    (is (nil? (auth/bearer-token {:headers {}})))))

(deftest named-bearer-token-validation-and-lookup
  (let [tokens (auth/validate-named-tokens!
                {:tokens {:phone "0123456789abcdef0123456789abcdef"}}
                "Test API")]
    (is (= [{:name "phone" :token "0123456789abcdef0123456789abcdef"}]
           tokens))
    (is (= "phone"
           (auth/token-owner tokens "0123456789abcdef0123456789abcdef")))
    (is (nil? (auth/token-owner tokens "wrong")))
    (is (nil? (auth/token-owner tokens nil))))
  (doseq [bad [{}
               {:tokens {}}
               {:tokens {:phone ""}}
               {:tokens {:phone "too-short"}}
               {:tokens {"bad name" "0123456789abcdef0123456789abcdef"}}
               {:tokens {:phone "0123456789abcdef0123456789abcdef"
                         :tablet "0123456789abcdef0123456789abcdef"}}
               {:tokens {:phone "0123456789abcdef0123456789abcdef"
                         "phone" "fedcba9876543210fedcba9876543210"}}]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (auth/validate-named-tokens! bad "Test API")))))
