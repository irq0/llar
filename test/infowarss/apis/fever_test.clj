(ns infowarss.apis.fever-test
  (:require [clojure.test :refer :all]
            [clj-time.core :as time]
            [digest]
            [clojurewerkz.serialism.core :refer [deserialize]]
            [ring.mock.request :as mock]
            [infowarss.couchdb :as couch]
            [infowarss.webapp :refer [fever-app]]
            [infowarss.schema :as schema]
            [schema.core :as s]
            [schema.test]
            [infowarss.apis.fever :refer :all]))


(def demo-item
  {:hash (digest/sha-256 "Example")
   :feed {:title "Example"
          :language "en"
          :link "http://example.com"
          :description "Example feed"
          :encoding "utf-8"
          :published-date (time/now)}
   :feed-entry {:link "http://example.com/entry"
                :published-date (time/now)
                :title "Example entry"
                :authors ["Example Author 1" "Example Author 2"]
                :contents {"text/plain" "Example plain text content"
                           "text/html" "<h1>Example html content"}
                :description {"text/plain" "Example description"}}
   :summary {:from "example"
             :title "example"}})

(defn setup-database [f]
  (binding [couch/*couch-default-db* "testdb"]
    (couch/init-db!)
    (couch/add-document! demo-item)
    (f)
    (couch/clear-db!)
    ))

(use-fixtures :once setup-database)
(use-fixtures :once schema.test/validate-schemas)

(defn mock-req [uri]
  (mock/request :post uri {:api_key (api-key)}))


(deftest schema
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
