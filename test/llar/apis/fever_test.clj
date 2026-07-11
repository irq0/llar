(ns llar.apis.fever-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is]]
   [llar.apis.fever :as uut]
   [llar.appconfig :as appconfig]
   [llar.blobstore :as blobstore]
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

(deftest fever-blob-urls-are-made-public-and-absolute
  (let [hash (apply str (repeat 64 "a"))
        html (uut/sanitize-content
              (format "<img src='/blob/%s'><img src='https://cdn.test/image'>" hash)
              true
              "https://article.test/story"
              4096
              "https://reader.test/fever/")]
    (is (string/includes? html (str "src=\"https://reader.test/fever/blob/" hash "\"")))
    (is (string/includes? html "src=\"https://cdn.test/image\""))))

(deftest fever-serves-content-addressed-blobs
  (let [hash (apply str (repeat 64 "a"))
        data (java.io.ByteArrayInputStream.
              (.getBytes "image" java.nio.charset.StandardCharsets/UTF_8))]
    (with-redefs [blobstore/get-blob (constantly {:mime-type "image/jpeg"
                                                  :created (java.time.ZonedDateTime/now)
                                                  :size 5
                                                  :data data})]
      (let [response ((uut/handler :db test-config)
                      {:request-method :get :uri (str "/blob/" hash)})]
        (is (= 200 (:status response)))
        (is (= "image/jpeg" (get-in response [:headers "Content-Type"])))
        (is (= "public, max-age=31536000, immutable"
               (get-in response [:headers "Cache-Control"])))
        (is (= (str "W/\"" hash "\"") (get-in response [:headers "ETag"])))
        (is (identical? data (:body response)))))))

(deftest unauthenticated-response-does-not-query-data
  (with-redefs [appconfig/credentials (constantly {:password "correct horse"})
                sql/fever-sources (fn [_] (throw (ex-info "must not query" {})))]
    (is (= {:status 200 :body {:api_version 3 :auth 0}}
           (uut/fever-response :db test-config {:params {"api_key" "wrong"}})))))

(deftest empty-account-has-complete-empty-responses
  (with-redefs [appconfig/credentials (constantly {:password "correct horse"})
                config/get-sources (constantly {})
                sql/fever-sources (constantly [])
                sql/fever-items (constantly [])
                sql/fever-total-items (constantly {:total 0})
                sql/fever-item-state-ids (constantly [])]
    (let [response (uut/fever-response
                    :db test-config
                    {:params {"api_key" api-key
                              "groups" ""
                              "feeds" ""
                              "items" ""
                              "unread_item_ids" ""
                              "saved_item_ids" ""}})]
      (is (= 1 (get-in response [:body :auth])))
      (is (= [{:id 1 :title "LLAR"}] (get-in response [:body :groups])))
      (is (= [uut/queue-feed] (get-in response [:body :feeds])))
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

(deftest unsaving-a-bookmark-removes-all-queue-reasons
  (let [removed (atom nil)
        params {"api_key" api-key "mark" "item" "as" "unsaved" "id" "42"}]
    (with-redefs [appconfig/credentials (constantly {:password "correct horse"})
                  config/get-sources (constantly {})
                  sql/fever-sources (constantly [])
                  sql/fever-item-selected (constantly {:selected true :bookmark true})
                  sql/fever-item-state-ids (constantly [])
                  persistency/item-remove-tags! (fn [_ id tags] (reset! removed [id tags]))]
      (uut/fever-response :db test-config {:params params})
      (is (= [42 [:saved :in-progress :unread]] @removed)))))

(deftest reading-a-bookmark-refreshes-both-state-lists
  (let [removed (atom nil)
        params {"api_key" api-key "mark" "item" "as" "read" "id" "42"}]
    (with-redefs [appconfig/credentials (constantly {:password "correct horse"})
                  config/get-sources (constantly {})
                  sql/fever-sources (constantly [])
                  sql/fever-item-selected (constantly {:selected true :bookmark true})
                  sql/fever-item-state-ids (constantly [])
                  persistency/item-remove-tags! (fn [_ id tags] (reset! removed [id tags]))]
      (let [response (uut/fever-response :db test-config {:params params})]
        (is (= [42 [:unread]] @removed))
        (is (= "" (get-in response [:body :unread_item_ids])))
        (is (= "" (get-in response [:body :saved_item_ids])))))))

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

(deftest synthetic-queue-feed-rejects-bulk-mark-read
  (with-redefs [appconfig/credentials (constantly {:password "correct horse"})
                config/get-sources (constantly {})
                sql/fever-sources (constantly [])
                sql/fever-mark-read (fn [& _] (throw (ex-info "must not write" {})))]
    (let [response ((uut/handler :db test-config)
                    {:request-method :post
                     :uri "/"
                     :params {"api_key" api-key
                              "mark" "feed"
                              "as" "read"
                              "id" (str uut/queue-feed-id)
                              "before" "1700000000"}})]
      (is (= 400 (:status response)))
      (is (= "Unknown feed" (get-in response [:body :error]))))))

(deftest logging-summaries-redact-and-bound-data
  (let [request-summary (#'uut/request-summary
                         {:request-method :post
                          :uri "/"
                          :remote-addr "192.0.2.1"
                          :headers {"user-agent" "Fever Client"
                                    "content-type" "application/x-www-form-urlencoded"}
                          :params {"api_key" api-key
                                   "items" ""
                                   "with_ids" (apply str (repeat 200 "1"))}})
        response-summary (#'uut/response-summary
                          {:status 200
                           :body {:auth 1
                                  :items [{:html (apply str (repeat 1000 "x"))}]
                                  :unread_item_ids "1,2,3"}}
                          12)]
    (is (= api-key (get-in request-summary [:params "api_key"])))
    (is (re-find #"^1+\.\.\. \[200 chars\]$"
                 (get-in request-summary [:params "with_ids"])))
    (is (= {:type :collection :count 1}
           (get-in response-summary [:body :items])))
    (is (= {:type :string :chars 5 :bytes 5}
           (get-in response-summary [:body :unread_item_ids])))
    (is (not (re-find #"xxx" (pr-str response-summary))))))
