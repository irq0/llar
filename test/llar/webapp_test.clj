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

(deftest reader-csp-allows-required-content-without-external-scripts
  (let [policy @#'uut/reader-content-security-policy]
    (is (re-find #"script-src 'self'" policy))
    (is (re-find #"style-src 'self' 'unsafe-inline'" policy))
    (is (re-find #"img-src 'self' data: https:" policy))
    (is (re-find #"frame-src https://www.youtube-nocookie.com https://www.youtube.com" policy))
    (is (re-find #"media-src 'self' https: blob:" policy))
    (is (re-find #"frame-ancestors 'none'" policy))))
