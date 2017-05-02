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
            [schema.core :as s]
            [infowarss.persistency :as persistency]
            [clojure.java.io :as io]
            [schema.test]
            [schema-generators.complete :as c]
            [schema-generators.generators :as g]
            [clojure.test.check.generators :as gen]
            [infowarss.apis.fever :refer :all]))


(def demo-item
  (fetch/map->FeedItem
  {:hash (str "SHA-256:" (digest/sha-256 "Example"))
   :meta {:app "infowarss.test"
          :fetch-ts (time/now)
          :ns (str *ns*)
          :source {:url (io/as-url "http://example.com")}
          :source-name "[Example-Source]"
          :tags #{:test :unread}
          :version 0}
   :feed {:title "Example"
          :url (io/as-url "http://example.com")
          :language "en"
          :description {"text/plain" "Example feed"}
          :encoding "utf-8"
          :feed-type "something-23"
          :pub-ts (time/now)}
   :entry {:url (io/as-url "http://example.com/entry")
           :pub-ts (time/now)
           :updated-ts (time/now)
           :title "Example entry"
           :authors ["Example Author 1" "Example Author 2"]
           :contents {"text/plain" "Example plain text content"
                      "text/html" "<h1>Example html content"}
           :description {"text/plain" "Example description"}}
   :summary {:ts (time/now)
             :title "example"}}))

(defonce demo-item-id (atom ""))

(defn setup-database [f]
  (binding [couch/*couch-default-db* "testdb"]
    (try+
      (couch/init-db!)
      (catch [:status 409] _ nil))
    ;; Insert some generated entries and the demo above
    (reset! demo-item-id (:id (persistency/store-items! [demo-item])))
    (persistency/store-items! (g/sample 42 infowarss.fetch.FeedItem schema-test/leaf-generators))
    (f)
    (couch/clear-db!)))

(use-fixtures :once (join-fixtures [setup-database schema.test/validate-schemas]))

(defn mock-req [uri]
  (mock/request :post uri {:api_key (api-key)}))

(defn mock-write-op [id mark as]
  (mock/request :post "?api" {:api_key (api-key)
                              :id id
                              :mark mark
                              :as as}))

(deftest requests
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

(deftest write-op
  (let [set-read (mock-write-op (fever-item-id @demo-item-id) "item" "read")
        set-unread (mock-write-op (fever-item-id @demo-item-id) "item" "unread")]

    (fever-app set-read)

    (let [doc (couch/get-document @demo-item-id)]
      (is (not (contains? (:tags doc) :unread))))

    (fever-app set-unread)

    (let [doc (couch/get-document @demo-item-id)]
      (is (contains? (:tags doc) :unread)))))
