(ns llar.apis.dashboard-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [hiccup2.core :as h]
   [java-time.api :as time]
   [llar.appconfig :as appconfig]
   [llar.rc :as rc]
   [llar.sched :as sched]
   [llar.apis.dashboard :as uut]))

(deftest status-index-renders-non-source-tabs-lazily
  (testing "initial dashboard render does not execute heavy hidden tabs"
    (with-redefs [uut/tabs {:overview (fn [] [:div "overview summary"])
                            :sources (fn [] (throw (ex-info "should not render sources" {})))
                            :heavy (fn [] (throw (ex-info "should not render" {})))}]
      (let [body (uut/status-index)]
        (is (string/includes? body "overview summary"))
        (is (string/includes? body "data-tab-name=\"sources\""))
        (is (string/includes? body "data-tab-name=\"heavy\""))
        (is (string/includes? body "Click tab to load..."))
        (is (not (string/includes? body "should not render sources")))
        (is (not (string/includes? body "should not render")))))))

(deftest dashboard-tab-renders-known-non-source-tabs
  (with-redefs [uut/tabs {:overview (fn [] [:div "overview summary"])
                          :sources (fn [] [:div "sources table"])
                          :memory (fn [] [:div "memory tab"])}]
    (testing "known non-source tab renders HTML"
      (let [response (uut/dashboard-tab "memory")]
        (is (= 200 (:status response)))
        (is (= "text/html; charset=utf-8" (get-in response [:headers "Content-Type"])))
        (is (string/includes? (:body response) "memory tab"))))

    (testing "sources is lazy-loadable"
      (let [response (uut/dashboard-tab "sources")]
        (is (= 200 (:status response)))
        (is (string/includes? (:body response) "sources table"))))

    (testing "overview is reloadable"
      (let [response (uut/dashboard-tab "overview")]
        (is (= 200 (:status response)))
        (is (string/includes? (:body response) "overview summary"))))

    (testing "unknown tab returns 404"
      (is (= 404 (:status (uut/dashboard-tab "unknown")))))))

(deftest dashboard-tab-route-renders-known-tab
  (with-redefs [uut/tabs {:overview (fn [] [:div "overview summary"])
                          :sources (fn [] [:div "sources table"])
                          :memory (fn [] [:div "memory tab"])}]
    (let [response (uut/app {:request-method :get
                             :uri "/tab/memory"})]
      (is (= 200 (:status response)))
      (is (string/includes? (:body response) "memory tab")))))

(deftest docs-tab-renders-config-docs
  (let [response (uut/dashboard-tab "docs")]
    (is (= 200 (:status response)))
    (is (string/includes? (:body response) "LLAR Configuration"))
    (is (string/includes? (:body response) "sched-fetch"))
    (is (string/includes? (:body response) ":now-and-hourly"))))

(deftest config-tab-renders-system-config-and-rc
  (with-redefs [appconfig/appconfig {:postgresql {:frontend {:password "secret"}
                                                  :backend {:password "secret"}}
                                     :ui {:favorites [[:appconfig :source-tag]]
                                          :default-list-view {:appconfig :gallery}}
                                     :ranking {:highlight-boost-hours 12
                                               :rarity-boost-cap-hours 24}}]
    (rc/reset-rc!)
    (rc/rc [:reader :ranking :highlight-boost-hours] 96)
    (let [response (uut/dashboard-tab "config")]
      (is (= 200 (:status response)))
      (is (string/includes? (:body response) "System config"))
      (is (string/includes? (:body response) "Runtime config (rc)"))
      (is (string/includes? (:body response) "Effective"))
      (is (string/includes? (:body response) "Overrides"))
      (is (string/includes? (:body response) "Defaults"))
      (is (string/includes? (:body response) "Supported rc paths"))
      (is (string/includes? (:body response) "[:reader :favorites]"))
      (is (string/includes? (:body response) "[:ui :favorites]"))
      (is (string/includes? (:body response) "96"))
      (is (string/includes? (:body response) "--secret removed--")))
    (rc/reset-rc!)))

(deftest metrics-endpoint-has-prometheus-content-type
  (testing "/metrics returns a non-blank Prometheus text format content type"
    (let [response (uut/app {:request-method :get
                             :uri "/metrics"})]
      (is (= 200 (:status response)))
      (is (string/starts-with? (get-in response [:headers "Content-Type"])
                               "text/plain; version=0.0.4"))
      (is (string/includes? (:body response) "# HELP")))))

(deftest schedule-tab-renders-runtime-state
  (let [schedule (sched/make-schedule
                  {:key :daily
                   :mount-state "#'llar.test/daily"
                   :sched-name "daily"
                   :sched-type :defsched
                   :chime-times :hourly
                   :schedule-times [(time/plus (time/zoned-date-time) (time/minutes 5))]
                   :pred '(do :work)
                   :run-fn (constantly :ok)})]
    (swap! (:state* schedule) assoc
           :running? false
           :last-trigger :manual
           :last-result {:count 1})
    (with-redefs [sched/find-schedules (constantly [schedule])]
      (let [body (str (h/html (uut/schedule-tab)))]
        (is (string/includes? body "daily"))
        (is (string/includes? body "Next Run"))
        (is (string/includes? body "in "))
        (is (string/includes? body "llar.test/daily"))
        (is (string/includes? body "manual"))
        (is (string/includes? body ":count"))
        (is (string/includes? body "btn-run-schedule"))))))

(deftest run-schedule-route-handles-resolution-results
  (testing "unknown schedule"
    (with-redefs [sched/find-schedule (constantly {:error :not-found})]
      (is (= 404 (:status (uut/run-schedule "missing"))))))

  (testing "ambiguous schedule"
    (with-redefs [sched/find-schedule (constantly {:error :ambiguous
                                                   :matches [{:key :x}]})]
      (is (= 409 (:status (uut/run-schedule "x"))))))

  (testing "known schedule"
    (with-redefs [sched/find-schedule (constantly {:schedule :schedule})
                  sched/trigger-schedule! (constantly :triggered)]
      (let [response (uut/app {:request-method :post
                               :uri "/api/schedule/daily/run"})]
        (is (= 200 (:status response)))
        (is (= :triggered (get-in response [:body :result])))))))
