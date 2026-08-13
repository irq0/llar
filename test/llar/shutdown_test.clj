(ns llar.shutdown-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [llar.shutdown :as uut]
   [llar.work :as work]
   [mount.core :as mount])
  (:import
   [java.util Properties]))

(deftest teardown-is-reported-before-logging-is-stopped
  (let [events (atom [])]
    (with-redefs-fn
      {#'mount/stop #(swap! events conj :mount-stop)
       #'mount/running-states (constantly #{})
       #'work/in-flight (constantly [])
       #'uut/report! (fn [status error _]
                       (swap! events conj [:report status error]))
       #'uut/stop-logging! #(swap! events conj :logging-stop)}
      #'uut/run-shutdown!)
    (is (= [:mount-stop
            [:report :finished nil]
            :logging-stop]
           @events))))

(deftest stop-failure-is-reported-and-logging-is-still-stopped
  (let [error (ex-info "boom" {:reason :test})
        events (atom [])]
    (with-redefs-fn
      {#'mount/stop #(do (swap! events conj :mount-stop) (throw error))
       #'mount/running-states (constantly #{"#'llar.test/a"})
       #'work/in-flight (constantly [])
       #'uut/report! (fn [status reported _]
                       (swap! events conj [:report status reported]))
       #'uut/stop-logging! #(swap! events conj :logging-stop)}
      #'uut/run-shutdown!)
    (is (= [:mount-stop
            [:report :failed error]
            :logging-stop]
           @events))))

(deftest verification-failure-cannot-skip-logging-shutdown
  (let [error (ex-info "snapshot failed" {:reason :test})
        events (atom [])]
    (with-redefs-fn
      {#'mount/stop #(swap! events conj :mount-stop)
       #'mount/running-states #(throw error)
       #'uut/report! (fn [status reported message]
                       (swap! events conj [:report status reported message]))
       #'uut/stop-logging! #(swap! events conj :logging-stop)}
      #'uut/run-shutdown!)
    (is (= [:mount-stop :report :logging-stop]
           (mapv #(if (vector? %) (first %) %) @events)))
    (is (= [:failed error]
           (subvec (second @events) 1 3)))
    (is (re-find #"before teardown could be verified"
                 (last (second @events))))))

(deftest terminal-message-only-claims-what-can-be-observed
  (is (= "LLAR shutdown finished in 12ms: remaining-states=0 remaining-work=0"
         (#'uut/shutdown-message :finished 12 [] [] nil)))
  (is (= (str "LLAR shutdown incomplete in 12ms: remaining-states=1 remaining-work=1 "
              "states=[\"#'llar.test/a\"] "
              "work=[{:id 7, :stage :fetch}]")
         (#'uut/shutdown-message :incomplete 12
                                 ["#'llar.test/a"] [{:id 7 :stage :fetch}] nil))))

(deftest logging-failure-falls-back-to-stdout
  (let [output (atom [])]
    (with-redefs-fn
      {#'uut/emit-log! (fn [& _]
                         (throw (IllegalStateException. "logger unavailable")))
       #'uut/stdout! #(swap! output conj %)}
      #(#'uut/report! :finished nil "LLAR shutdown finished"))
    (is (= ["LLAR shutdown finished logging-error=java.lang.IllegalStateException: logger unavailable"]
           @output))))

(deftest log4j-shutdown-and-journald-output-are-owned-by-llar
  (let [properties (Properties.)]
    (with-open [reader (io/reader (io/resource "log4j2.properties"))]
      (.load properties reader))
    (is (= "disable" (.getProperty properties "shutdownHook")))
    (is (= "SYSTEM_OUT" (.getProperty properties "appender.console.target")))
    (is (= "true" (.getProperty properties "appender.console.direct")))))
