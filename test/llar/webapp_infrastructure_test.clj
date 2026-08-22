(ns llar.webapp-infrastructure-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.webapp :as uut]
   [ring.adapter.jetty :as jetty]))

(deftest jetty-start-supports-a-private-bind-host
  (let [seen (atom nil)]
    (with-redefs [jetty/run-jetty
                  (fn [app options]
                    (reset! seen [app options])
                    :server)]
      (is (= :server
             (uut/try-start-jetty :handler 8024 {:host "127.0.0.1"
                                                 :port 9000
                                                 :join? true})))
      (is (= [:handler {:port 8024
                        :join? false
                        :host "127.0.0.1"}]
             @seen)))))
