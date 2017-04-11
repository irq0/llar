(ns infowarss.apis.fever
  (:require
   [infowarss.core :refer [*srcs*]]
   [infowarss.couchdb :as couch]
   [ring.util.response :as response]
   [digest]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.io :as io])
  (:import [java.util.Base64.Encoder]))

(defmacro swallow-exceptions [& body]
  `(try ~@body (catch Exception e#)))

(defonce ^:dynamic *api-config*
  {:email "infowarss@irq0.org"
   :pass  "foo"
   :version 42})

(defn- api-key []
  (let [{:keys [email pass]} *api-config*]
    (digest/md5 (str email ":" pass))))

(defn- auth? [key]
  (= (string/lower-case (str key)) (api-key)))

(defn- auth-error []
  (-> (response/response "Auth error!")
      (response/status 401)))

(defn wrap-auth [handler]
  "Check fever api key and set :fever-auth in request
   Overwrite response with (fever-auth-error) if unauthorized"
  (fn [request]
    #spy/p request
    (let [api-key ((request :params) :api_key)
          auth? (auth? api-key)
          request (assoc-in request [:fever-auth] auth?)
          response (handler request)]
      (if auth?
        #spy/p response
        (auth-error)))))


(defn mk-favicon [filename]
  (str "image/gif;"
    "base64;"
    (.encodeToString
      (java.util.Base64/getMimeEncoder)
      (.getBytes (slurp filename)))))

(defn groups []
  (let [feedids (->> @*srcs*
                  keys
                  (map hash))]

  {:last_refreshed_on_time 0
   :groups [{:id 1, :title "All"}]
   :feeds_groups [:group_id 1
                  :feed_ids (apply str (interpose "," feedids))]}))

(defn- fever-src [src]
  (let [[k {:keys [src state]}] src]
    {:id (hash (get src :title))
     :favicon_id 1337
     :title (:title src)
     :url (:url src)
     :site_url (:url src)
     :is_spark 0
     :last_updated_on_time (tc/to-long (:last-successful-fetch-ts state))}))

(defn feeds []
  {:last_refreshed_on_time 0,
   :feeds (for [src @*srcs*]
            (fever-src src))})

(defn favicons []
  {:favicons [{:id 1337
               :data (mk-favicon (io/resource "favicon.ico"))}]})

(defn- seq-contains? [coll target] (some #(= target %) coll))

(defn- fever-item-id [id]
  (Long/parseUnsignedLong (subs id 24 32) 16))


(defn- fever-doc [doc]
  (let [contents (get-in doc [:feed-entry :contents])]
  {:id (fever-item-id (:_id doc))
   :feed_id (hash (get-in doc [:source :title]))
   :title (get-in doc [:summary :title])
   :author (string/join ", " (get-in doc [:feed-entry :authors]))
   :html (or (get contents "text/html") (get contents "text/plain"))
   :url (get-in doc [:feed-entry :link])
   :is_saved false
   :is_read false
   :created_on_time (get-in doc [:feed :published-date])}))

;; Use the since_id argument with the highest id of locally cached
;; items to request 50 additional items. Repeat until the items array
;; in the response is empty.
;; Use the max_id argument with the lowest id of locally cached
;; items (or 0 initially) to request 50 previous items. Repeat until
;; the items array in the response is empty. (added in API version 2)
;; Use the with_ids argument with a comma-separated list of item ids to
;; request (a maximum of 50) specific items. (added in API version 2)
(defn items [{:keys [since-id max-id with-ids]}]
  (let [ids (couch/all-doc-ids)

        since (swallow-exceptions (Long/parseLong since-id)),
        max (swallow-exceptions (Long/parseLong max-id)),
        with (swallow-exceptions (map #(Long/parseLong %) (re-seq #"\d+" with-ids)))
        sincef (if since
                 (fn [items] (filter (fn [id] (> id since)) ids))
                 (fn [items] ids))
        maxf (if max
               (fn [items] (filter (fn [id] (< id max)) ids))
               (fn [items] ids))

        withf (if with
                (fn [items] (filter (fn [id] (seq-contains? with id)) ids))
                (fn [items] ids))

        ids-to-return (-> ids
                       sort
                       withf
                       sincef
                       maxf)]
    {:total_items (count ids)
     :items (for [id ids-to-return
                  :let [doc (couch/get-document-with-attachments id)]]
              (fever-doc doc))}))




;; Or the first page (page=1) of Hot links for the past week (range=7)
;; starting now (offset=0).
(defn links [offset range page]
  {:links [{:id 10
            :feed_id 23
            :item_id 1
            :temperature 0.5
            :is_item 1
            :is_local 1
            :is_saved 1
            :title "link 10 -> feed 23 item 1"
            :url "fooo"
            :item_ids "1,2"}]})

;; return: string/comma-separated list of positive integers)
(defn unread-item-ids []
  {:unread_item_ids
   (string/join ","
     (map fever-item-id (couch/doc-ids-with-tag :unread)))})

(defn saved-item-ids [])


(defn api-root []
  {:api_version 3
   :auth 1})

(defn fever-api [req]
  (let [op (->> req
             :query-string
             (re-find #"^api&?(\w+)?&?.*$")
             second
             keyword)
        params (:query-params req)
        resp-body (condp = op
                    :groups (groups)
                    :feeds (feeds)
                    :items (items params)
                    :links (not-implemented)
                    :favicons (favicons)
                    :unread_item_ids (unread-item-ids)
                    :saved_item_ids (saved-item-ids)
                    nil (api-root)
                    {}
                    )]
    (response/response (-> resp-body
                         (merge (api-root)))

                         )))
