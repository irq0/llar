(ns infowarss.apis.infowarss
  (:require
   [infowarss.src :as src]
   [infowarss.core :as core]
   [infowarss.fetch :as fetch]
   [infowarss.postproc :as proc]
   [infowarss.update :as update]
   [clojure.java.io :as io]
   [infowarss.persistency :as persistency]
   [taoensso.timbre :as log]
   [compojure.api.sweet :refer :all]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.core.async :as async]
   [schema.core :as s]
   [ring.util.http-response :refer :all]))

(defn api-authenticated? [user pass]
  (let [entry (:api core/creds)]
    (and
      (= user (:username entry))
      (= pass (:password entry)))))


(defn add [feed key]
  (log/debug "[Infowarss API] Add: " feed)
  (try+
    (let [state (assoc update/src-state-template :key key)
          items (fetch/fetch feed)
          processed (proc/process feed state items)
          dbks (persistency/store-items! processed)]
      {:status (:status state)
       :exception (:message (:last-exception state))
       :title (get-in (first items) [:summary :title])})
    (catch Object e
      (log/warn e "add-url failed. Probably broken url: " feed)
      {:status :fail
       :exception (:message e)
       :title ""})))


(def app
 (api
   {:swagger
    {:ui "/api-docs"
     :spec "/swagger.json"
     :data {:info {:title "Infowarss API"
                   :description "Infowarss API"}
            :tags [{:name "api", :description "some apis"}]}}}

   (context "/api" []
     :tags ["api"]

     (POST "/documents/add" []
       :return {:status s/Keyword :exception (s/maybe s/Str) :title s/Str}
       :form-params [url :- String]
       (ok (add (core/make-doc-feed url) :document)))

     (GET "/documents/add" []
       :return {:status s/Keyword :exception (s/maybe s/Str) :title s/Str}
       :query-params [url :- s/Str]
       (ok (add (core/make-doc-feed url) :document)))

     (POST "/bookmarks/add" []
       :return {:status s/Keyword :exception (s/maybe s/Str) :title s/Str}
       :form-params [url :- s/Str]
       :summary "Add URL"
       (ok (add (core/make-bookmark-feed url) :bookmark))))))
