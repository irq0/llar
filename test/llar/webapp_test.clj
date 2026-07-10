(ns llar.webapp-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.webapp :as uut]))

(deftest security-headers-adds-referrer-policy
  (let [handler (uut/wrap-security-headers (constantly {:status 200
                                                        :headers {}
                                                        :body "ok"}))
        response (handler {:request-method :get :uri "/"})]
    (is (= "no-referrer" (get-in response [:headers "Referrer-Policy"])))
    (is (nil? (get-in response [:headers "Content-Security-Policy"])))))

(deftest security-headers-adds-csp-when-configured
  (let [handler (uut/wrap-security-headers
                 (constantly {:status 200 :headers {} :body "ok"})
                 :content-security-policy "default-src 'self'")
        response (handler {:request-method :get :uri "/"})]
    (is (= "no-referrer" (get-in response [:headers "Referrer-Policy"])))
    (is (= "default-src 'self'"
           (get-in response [:headers "Content-Security-Policy"])))))
