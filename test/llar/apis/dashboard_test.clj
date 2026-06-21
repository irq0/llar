(ns llar.apis.dashboard-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [llar.apis.dashboard :as uut]))

(deftest metrics-endpoint-has-prometheus-content-type
  (testing "/metrics returns a non-blank Prometheus text format content type"
    (let [response (uut/app {:request-method :get
                             :uri "/metrics"})]
      (is (= 200 (:status response)))
      (is (string/starts-with? (get-in response [:headers "Content-Type"])
                               "text/plain; version=0.0.4"))
      (is (string/includes? (:body response) "# HELP")))))
