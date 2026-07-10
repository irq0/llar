(ns llar.apis.fever-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.apis.fever :as uut]
   [llar.appconfig :as appconfig]
   [llar.config :as config]
   [llar.db.sql :as sql]
   [llar.persistency :as persistency]))

(def test-config
  {:username "reader@example.test"
   :credentials :mobile-sync
   :source-tag :mobile
   :initial-days 30
   :recent-read-days 10
   :max-content-bytes 1024})

(def api-key
  (uut/expected-api-key "reader@example.test" "correct horse"))

(deftest authentication-uses-fever-key
  (with-redefs [appconfig/credentials (constantly {:password "correct horse"})]
    (is (uut/authenticated? test-config api-key))
    (is (not (uut/authenticated? test-config "wrong")))
    (is (not (uut/authenticated? test-config nil)))))

(deftest source-selection-is-based-on-runtime-source-tag
  (with-redefs [config/get-sources (constantly {:phone {:tags #{:mobile}}
                                                :desktop {:tags #{:other}}})
                sql/fever-sources (constantly [{:id 1 :key "phone"}
                                               {:id 2 :key "desktop"}
                                               {:id 3 :key "removed"}])]
    (is (= [{:id 1 :key "phone"}]
           (uut/selected-sources :db test-config)))))

(deftest content-is-sanitized-and-bounded
  (let [html (uut/sanitize-content
              "<script>alert(1)</script><p>Hello <a href='/story'>world</a></p>"
              true
              "https://example.test/base/"
              1024)]
    (is (not (re-find #"script" html)))
    (is (re-find #"Hello" html))
    (is (re-find #"https://example.test/story" html)))
  (is (re-find #"Content truncated"
               (uut/sanitize-content (apply str (repeat 100 "ä")) false nil 30))))

(deftest unauthenticated-response-does-not-query-data
  (with-redefs [appconfig/credentials (constantly {:password "correct horse"})
                sql/fever-sources (fn [_] (throw (ex-info "must not query" {})))]
    (is (= {:status 200 :body {:api_version 3 :auth 0}}
           (uut/fever-response :db test-config {:params {"api_key" "wrong"}})))))

(deftest empty-account-has-complete-empty-responses
  (with-redefs [appconfig/credentials (constantly {:password "correct horse"})
                config/get-sources (constantly {})
                sql/fever-sources (constantly [])]
    (let [response (uut/fever-response
                    :db test-config
                    {:params {"api_key" api-key
                              "groups" ""
                              "feeds" ""
                              "items" ""
                              "unread_item_ids" ""
                              "saved_item_ids" ""}})]
      (is (= 1 (get-in response [:body :auth])))
      (is (= [] (get-in response [:body :groups])))
      (is (= [] (get-in response [:body :feeds])))
      (is (= [] (get-in response [:body :items])))
      (is (= "" (get-in response [:body :unread_item_ids])))
      (is (= "" (get-in response [:body :saved_item_ids]))))))

(deftest item-write-maps-to-llar-tags
  (let [writes (atom [])
        params {"api_key" api-key "mark" "item" "as" "saved" "id" "42"}]
    (with-redefs [appconfig/credentials (constantly {:password "correct horse"})
                  config/get-sources (constantly {:phone {:tags #{:mobile}}})
                  sql/fever-sources (constantly [{:id 7 :key "phone"}])
                  sql/fever-item-selected (constantly {:selected true})
                  sql/fever-item-state-ids (constantly [{:id 42}])
                  persistency/item-set-tags! (fn [_ id tags] (swap! writes conj [id tags]))]
      (let [response (uut/fever-response :db test-config {:params params})]
        (is (= [[42 [:saved]]] @writes))
        (is (= "42" (get-in response [:body :saved_item_ids])))))))

(deftest malformed-write-is-rejected-by-handler
  (with-redefs [appconfig/credentials (constantly {:password "correct horse"})
                config/get-sources (constantly {:phone {:tags #{:mobile}}})
                sql/fever-sources (constantly [{:id 7 :key "phone"}])]
    (let [response ((uut/handler :db test-config)
                    {:request-method :post
                     :uri "/"
                     :params {"api_key" api-key
                              "mark" "item"
                              "as" "saved"
                              "id" "not-an-id"}})]
      (is (= 400 (:status response))))))
