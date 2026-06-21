(ns llar.apis.dashboard-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [llar.apis.dashboard :as uut]))

(deftest status-index-renders-non-source-tabs-lazily
  (testing "initial dashboard render does not execute heavy hidden tabs"
    (with-redefs [uut/tabs {:sources (fn [] [:div "sources table"])
                            :heavy (fn [] (throw (ex-info "should not render" {})))}]
      (let [body (uut/status-index)]
        (is (string/includes? body "sources table"))
        (is (string/includes? body "data-tab-name=\"heavy\""))
        (is (string/includes? body "Click tab to load..."))
        (is (not (string/includes? body "should not render")))))))

(deftest dashboard-tab-renders-known-non-source-tabs
  (with-redefs [uut/tabs {:sources (fn [] [:div "sources table"])
                          :memory (fn [] [:div "memory tab"])}]
    (testing "known non-source tab renders HTML"
      (let [response (uut/dashboard-tab "memory")]
        (is (= 200 (:status response)))
        (is (= "text/html; charset=utf-8" (get-in response [:headers "Content-Type"])))
        (is (string/includes? (:body response) "memory tab"))))

    (testing "sources remains part of the initial page and is not loaded via /tab"
      (is (= 404 (:status (uut/dashboard-tab "sources")))))

    (testing "unknown tab returns 404"
      (is (= 404 (:status (uut/dashboard-tab "unknown")))))))

(deftest dashboard-tab-route-renders-known-tab
  (with-redefs [uut/tabs {:sources (fn [] [:div "sources table"])
                          :memory (fn [] [:div "memory tab"])}]
    (let [response (uut/app {:request-method :get
                             :uri "/tab/memory"})]
      (is (= 200 (:status response)))
      (is (string/includes? (:body response) "memory tab")))))

(deftest metrics-endpoint-has-prometheus-content-type
  (testing "/metrics returns a non-blank Prometheus text format content type"
    (let [response (uut/app {:request-method :get
                             :uri "/metrics"})]
      (is (= 200 (:status response)))
      (is (string/starts-with? (get-in response [:headers "Content-Type"])
                               "text/plain; version=0.0.4"))
      (is (string/includes? (:body response) "# HELP")))))
