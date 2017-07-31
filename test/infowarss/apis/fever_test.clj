(ns infowarss.apis.fever-test
  (:require [clojure.test :refer :all]
            [clj-time.core :as time]
            [digest]
            [clojurewerkz.serialism.core :refer [deserialize]]
            [ring.mock.request :as mock]
            [infowarss.couchdb :as couch]
            [infowarss.webapp :refer [fever-app]]
            [infowarss.schema :as schema]
            [infowarss.schema-test :as schema-test]
            [slingshot.slingshot :refer [throw+ try+]]
            [infowarss.fetch :as fetch]
            [infowarss.fetch.feed :as feed]
            [schema.core :as s]
            [clojure.string :as string]
            [cheshire.core :as json]
            [infowarss.persistency :as persistency]
            [clojure.java.io :as io]
            [schema.test]
            [schema-generators.complete :as c]
            [schema-generators.generators :as g]
            [clojure.test.check.generators :as gen]
            [infowarss.apis.fever :refer :all]))


(def demo-item
  (feed/map->FeedItem
  {:hash (str "SHA-256:" (digest/sha-256 "Example"))
   :meta {:app "infowarss.test"
          :fetch-ts (time/now)
          :ns (str *ns*)
          :source {:url (io/as-url "http://example.com")}
          :source-name "[Example-Source]"
          :source-key :example
          :tags #{:test :unread}
          :version 0}
   :feed {:title "Example"
          :url (io/as-url "http://example.com")
          :language "en"
          :descriptions {"text/plain" "Example feed"}
          :encoding "utf-8"
          :feed-type "something-23"
          :pub-ts (time/now)}
   :entry {:url (io/as-url "http://example.com/entry")
           :pub-ts (time/now)
           :updated-ts (time/now)
           :title "Example entry"
           :authors ["Example Author 1" "Example Author 2"]
           :contents {"text/plain" "Example plain text content"
                      "text/html" "<h1>Example html content</h1>"}
           :descriptions {"text/plain" "Example description"}}
   :summary {:ts (time/now)
             :title "example"}}))

(defonce demo-item-id (atom ""))
(defonce generated-item-ids (atom ""))

(defn setup-database [f]
  (binding [couch/*couch-default-db* "testdb"]
    (try+
      (couch/init-db!)
      (catch [:status 409] _ nil))
    ;; Insert some generated entries and the demo above
    (reset! demo-item-id (first (persistency/store-items! [demo-item])))
    (reset! generated-item-ids (persistency/store-items! (g/sample 42 infowarss.fetch.feed.FeedItem schema-test/leaf-generators)))
    (f)
    (couch/clear-db!)))

(use-fixtures :once (join-fixtures [setup-database schema.test/validate-schemas]))

(defn mock-req
  ([uri]
   (mock-req uri {}))
  ([uri params]
   (mock/request :post uri (merge params {:api_key (api-key)}))))

(defn mock-write-op [id mark as]
  (mock/request :post "?api" {:api_key (api-key)
                              :id id
                              :mark mark
                              :as as}))

(deftest validate-response-schemas
  (let [do-req (fn [uri sch]
                 (let [{:keys [body status] :as resp} (fever-app (mock-req uri))
                         data (deserialize body :json)]
                   (is (= 200 status))
                   (is (= 3 (:api_version data)))
                   (is (= 1 (:auth data)))
                   {:http resp :data (s/validate sch data)}))]

    (testing "Empty req (?api)"
      (do-req "?api" schema/FeverAPIRoot))
    (testing "Groups req"
      (do-req "?api&groups" schema/FeverAPIGroups))
    (testing "Feeds req"
      (do-req "?api&feeds" schema/FeverAPIFeeds))
    (testing "Items req"
      (do-req "?api&items" schema/FeverAPIItems))
    (testing "Links req"
      (do-req "?api&links" schema/FeverAPILinks))
    (testing "Favicons req"
      (do-req "?api&favicons" schema/FeverAPIFavicons))
    (testing "Unread item ids req"
      (do-req "?api&unread_item_ids" schema/FeverAPIUnreadItemIds))
    (testing "Saved item ids req"
      (do-req "?api&saved_item_ids" schema/FeverAPISavedItemIds))))

(deftest test-feeds-request
  (let [resp (fever-app (mock-req "?api&feeds"))
        demo-feed-id (fever-feed-id (get demo-item :meta))
        all-group-id (fever-group-id-for-tag :all)
        data (json/parse-string (:body resp))]
    (is (some (fn [x] (= "Example" (get x "title"))) (get data "feeds")))
    (is (some (fn [x] (= demo-feed-id (get x "id"))) (get data "feeds")))
    (is (some (fn [x] (and (= all-group-id (get x "group_id"))
                        (some (partial = (str demo-feed-id)) (string/split (get x "feed_ids") #",") )))
          (get data "feeds_groups")))))

(deftest test-items-request-item-content
  (let [demo-id (fever-item-id @demo-item-id)
        resp (fever-app (mock-req "?api&items" {:with_ids demo-id}))
        data #spy/p (json/parse-string (:body resp))
        items (get data "items")
        item (first items)]
    (is (= 1 (count items)))
    (is (= (fever-feed-id (get demo-item :meta)) (get item "feed_id")))
    (is (= (fever-item-id @demo-item-id) (get item "id")))
    (is (= (str (get-in demo-item [:entry :url])) (get item "url")))
    (is (= (get-in demo-item [:entry :contents "text/html"]) (get item "html")))
    (is (= (string/join ", " (get-in demo-item [:entry :authors])) (get item "author")))))


(deftest test-unread-item-ids-request
  (let [resp (fever-app (mock-req "?api&unread_item_ids"))
        demo-id (fever-item-id @demo-item-id)
        data (json/parse-string (:body resp))]
    (is (some (partial = (str demo-id))
          (string/split (get data "unread_item_ids") #",")))))

(deftest test-items-request-range-options
  (let [demo-id (fever-item-id @demo-item-id)]
    (testing "with - single item"
      (let [resp (fever-app (mock-req "?api&items" {:with_ids demo-id}))
            data (json/parse-string (:body resp))
            items (get data "items")]
        (is (= 1 (count items)))))

    (testing "with - multiple items"
      (let [ids-to-fetch (set (map fever-item-id (random-sample 0.5 @generated-item-ids)))
            resp (fever-app (mock-req "?api&items"
                              {:with_ids (string/join "," ids-to-fetch)}))
            data (json/parse-string (:body resp))
            items (get data "items")]
        (is (= (count ids-to-fetch) (count items)))
        (every? (fn [item] (contains? ids-to-fetch (:id item))) items)))

    (testing "since empty"
      (let [ids (map fever-item-id @generated-item-ids)
            resp (fever-app (mock-req "?api&items"
                              {:since_id (+ 1 (apply max ids))}))
            data (json/parse-string (:body resp))
            items (get data "items")]
        (is (= 0 (count items)))))

    (comment (testing "max - from 0 reply all"
      (let [ids (conj (map fever-item-id @generated-item-ids)
                  (fever-item-id @demo-item-id))
            resp (fever-app (mock-req "?api&items"
                              {:max_id 0}))
            data (json/parse-string (:body resp))
            items (get data "items")]
        (is (= (count ids) (count items))))))

    (testing "since range"
      (let [ids (sort (conj (map fever-item-id @generated-item-ids)
                        (fever-item-id @demo-item-id)))
            half-index (int (/ (count ids) 2))
            resp (fever-app (mock-req "?api&items"
                              {:since_id (nth ids half-index)}))
            data (json/parse-string (:body resp))
            items (get data "items")]
        (is (= (count (nthrest ids (+ 1 half-index))) (count items)))))

    (testing "since max"
      (let [resp (fever-app (mock-req "?api&items" {:since_id demo-id :max_id demo-id}))
            data (json/parse-string (:body resp))
            items (get data "items")]
        (is (= 0 (count items)))))))

(deftest test-mark-feed-read
  (let [req (fever-app (mock-req "?api"
                         {:id (fever-feed-id (:meta (couch/get-document @demo-item-id)))
                          :mark "feed"
                          :as "read"}))
        doc (couch/get-document @demo-item-id)]
    (is (not (some #(= % "unread") (get-in doc [:meta :tags]))))))

(deftest test-mark-item-read-unread
  (let [set-read (mock-write-op (fever-item-id @demo-item-id) "item" "read")
        set-unread (mock-write-op (fever-item-id @demo-item-id) "item" "unread")]

    (testing "Set Read"
      (fever-app set-read)

      (let [doc (couch/get-document @demo-item-id)]
        (is (not (some #{"unread"} (get-in doc [:meta :tags]))))))

    (testing "Check item response"
      (let [resp (fever-app (mock-req "?api&items"
                              {:with_ids (fever-item-id @demo-item-id)}))
            data (json/parse-string (:body resp))
            items (get data "items")
            item (first items)]
        (is (= 1 (count items)))
        (is (= 1 (get item "is_read")))))

    (testing "Set Unread"
      (fever-app set-unread)

      (let [doc (couch/get-document @demo-item-id)]
        (is (some #{"unread"} (get-in doc [:meta :tags])))))

    (testing "Check item response"
      (let [resp (fever-app (mock-req "?api&items"
                              {:with_ids (fever-item-id @demo-item-id)}))
            data (json/parse-string (:body resp))
            items (get data "items")
            item (first items)]
        (is (= 1 (count items)))
        (is (= 0 (get item "is_read")))))))
