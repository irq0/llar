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
   [compojure.route :as route]
   [clojure.string :as string]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.core.async :as async]
   [schema.core :as s]
   [pantomime.mime]
   [pantomime.extract]
   [ring.util.response :as response]
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


(defn podcast-item [filename]
  (let [file (io/as-file filename)
        size (.length file)
        meta (pantomime.extract/parse file)
        mime-type (pantomime.mime/mime-type-of file)]
    (string/join "\n"
      ["<item>"
       (format "<title><![CDATA[%s]]></title>" (first (:title meta)))
       "<link></link>"
       (format "<description><![CDATA[%s]]></description>" (:text meta))
       (format "<enclosure url=\"http://10.23.1.23:7654%s\" length=\"%s\" type=\"%s\" />"
         (str "/files/videos/" (.getName file))
         size mime-type)
       "</item>"])))


(defn podcast-feed [directory]
  (let [path (io/as-file directory)
        files (seq (.list path (reify java.io.FilenameFilter
                                 (^boolean accept [_ ^java.io.File dir ^String name]
                                  (some? (re-find #".+\.mp4" name))))))]
    (string/join "\n"
      (concat
        ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
         "<rss version=\"2.0\""
         "     xmlns:itunes=\"http://www.itunes.com/dtds/podcast-1.0.dtd\">"
         "<channel>"
         "<title>infowarss podcast feed</title>"
         "<description>All the extracted videos!!1111!1!elf</description>"
         "<generator>infowarss</generator>"]
        (map #(podcast-item (str directory "/" %)) files)
        ["</channel>"
         "</rss>"]))))


(def app-api
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


(def app
  (routes
    app-api
    (GET "/feeds/videos" []
      (-> (ok (podcast-feed "/tank/scratch/youtube-dl"))
        (content-type "application/rss+xml; charset=UTF-8")))
    (route/files "/files/videos" {:root "/tank/scratch/youtube-dl"})
    (route/not-found "404 Not found")))
