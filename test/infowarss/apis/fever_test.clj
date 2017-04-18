(ns infowarss.apis.fever-test
  (:require [clojure.test :refer :all]
            [clj-time.core :as time]
            [digest]
            [clojurewerkz.serialism.core :refer [deserialize]]
            [ring.mock.request :as mock]
            [infowarss.couchdb :as couch]
            [infowarss.webapp :refer [fever-app]]
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

(defn check-schema [uri sch]
  (testing (format "URI: %s - %s" uri (name (quote sch)))
    (let [resp (fever-app (mock-req uri))
          data (deserialize (:body resp) :json)]
      (s/validate sch data))))

(deftest schema
  (check-schema "?api" FeverAPIRoot)
  (check-schema "?api&groups" FeverAPIGroups)
  (check-schema "?api&feeds" FeverAPIFeeds)
  (check-schema "?api&items" FeverAPIItems)
  (check-schema "?api&links" FeverAPILinks)
  (check-schema "?api&favicons" FeverAPIFavicons)
  (check-schema "?api&unread_item_ids" FeverAPIUnreadItemIds)
  (check-schema "?api&saved_item_ids" FeverAPISavedItemIds))
