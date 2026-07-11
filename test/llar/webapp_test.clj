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

(deftest prometheus-paths-have-bounded-cardinality
  (doseq [[uri expected]
          [["/reader/item/by-id/48324" "/reader/item/by-id/:item-id"]
           ["/reader/group/source-tag/hackernews/source/all/item/by-id/48324"
            "/reader/item/by-id/:item-id"]
           ["/reader/group/default/all/source/golem/item/by-id/48324/focus"
            "/reader/item/by-id/:item-id"]
           ["/reader/group/default/all/source/golem/item/by-id/48324/download"
            "/reader/item/by-id/:item-id/download"]
           ["/reader/group/source-tag/tech/source/all/item/by-id/48324/dump"
            "/reader/item/by-id/:item-id/dump"]
           ["/reader/group/source-tag/bike/source/all/items"
            "/reader/group/source-tag/:tag/source/all/items"]
           ["/reader/group/item-tags/saved/source/golem/items/"
            "/reader/group/item-tags/:tag/source/:source-key/items"]
           ["/reader/group/type/bookmark/source/bookmark-example/items"
            "/reader/group/type/:type/source/:source-key/items"]
           ["/reader/group/default/all/source/all/items"
            "/reader/group/default/all/source/all/items"]
           ["/reader/group/source-tag/bike/source/all/update"
            "/reader/group/source-tag/:tag/source/all/update"]
           ["/reader/group/custom/value/source/private/items"
            "/reader/group/:group/:group-item/source/:source-key/items"]
           ["/reader/annotation/48324" "/reader/annotation/:id"]
           ["/reader/export/48324/zotero" "/reader/export/:item-id/zotero"]
           ["/reader/export/48324/url-handler" "/reader/export/:item-id/url-handler"]
           ["/reader/tools/saved-overview" "/reader/tools/reading-queue"]
           ["/reader/tools/search" "/reader/tools/search"]
           ["/reader/tools/future-view" "/reader/tools/:view"]
           ["/blob/abcdef" "/blob/:hash"]
           ["/static/llar.js" "/static"]
           ["/static-assets" "/static-assets"]]]
    (is (= expected (uut/prom-path-fn {:uri uri})) uri)))

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
