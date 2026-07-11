(ns llar.webapp-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.metrics :as metrics]
   [llar.webapp :as uut]))

(deftest http-latency-histogram-uses-a-small-useful-bucket-set
  (is (= [0.01 0.025 0.05 0.1 0.25 0.5 1 5]
         metrics/http-latency-buckets)))

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

(deftest prometheus-paths-have-bounded-cardinality
  (is (= "/reader/item/by-id/:item-id"
         (uut/prom-path-fn
          {:uri "/reader/group/source-tag/hackernews/source/all/item/by-id/48324"})))
  (is (= "/reader/item/by-id/:item-id"
         (uut/prom-path-fn {:uri "/reader/item/by-id/48324"})))
  (is (= "/reader/item/by-id/:item-id"
         (uut/prom-path-fn {:uri "/reader/group/default/all/source/golem/item/by-id/48324/focus"})))
  (is (= "/blob/:hash" (uut/prom-path-fn {:uri "/blob/abcdef"})))
  (is (= "/static" (uut/prom-path-fn {:uri "/static/llar.js"})))
  (is (= "/static-assets" (uut/prom-path-fn {:uri "/static-assets"}))))

(deftest fever-prometheus-calls-are-semantic-and-bounded
  (is (= :fever/authenticate
         (uut/fever-call-fn {:uri "/" :params {"api_key" "key"}})))
  (is (= :fever/items
         (uut/fever-call-fn {:uri "/" :params {"items" "" "since_id" "42"}})))
  (is (= :fever/saved-item-ids
         (uut/fever-call-fn {:uri "/" :params {"saved_item_ids" ""}})))
  (is (= :fever/sync
         (uut/fever-call-fn {:uri "/" :params {"feeds" "" "groups" ""}})))
  (is (= :fever/mark-item
         (uut/fever-call-fn {:uri "/" :params {"mark" "item" "as" "saved"}})))
  (is (= :fever/blob
         (uut/fever-call-fn {:uri "/blob/abcdef" :params {}})))
  (is (= :fever/other
         (uut/fever-call-fn {:uri "/unexpected" :params {}}))))
