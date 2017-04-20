(ns infowarss.apis.fever
  (:require
   [infowarss.core :refer [*srcs*]]
   [infowarss.couchdb :as couch]
   [infowarss.schema :as schema]
   [ring.util.response :as response]
   [digest]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [schema.core :as s]
   [clojure.java.io :as io])
  (:import [java.util.Base64.Encoder]))

;; Utility functions

(defmacro swallow-exceptions [& body]
  `(try ~@body (catch Exception e#)))

(defn- fever-item-id
  "Convert infowarss feed item id to fever compatible id"
  [id]
  ;; our database keys are to long for the fever api
  ;; shorten to positive java long
  (let [parts [(subs id 0 4)  (subs id 28 32)]]
    (Long/parseLong (string/join parts) 16)))

(defn- fever-feed-id
  "Convert infowarss feed id to fever compatible id"
  [id]
  (let [hash (digest/sha-256 (str id))]
    (Long/parseUnsignedLong (subs hash 0 8) 16)))

(defn- fever-group-id-for-tag
  "Convert infowarss feed id to fever compatible id"
  [tag]
  (let [hash (digest/sha-256 (name tag))]
    (Long/parseUnsignedLong (subs hash 0 8) 16)))


(defn- fever-timestamp
  "Convert clj-time time object to fever unix timestamp"
  [time]
  (try+
    (-> time
      tc/to-long
      (/ 1000)
      (.longValue))
    (catch Object _
      0)))

(defn mk-favicon
  "
  Return file as fever favicon string representation
  File must be a image/gif
  "
  [filename]
  (str "image/gif;"
    "base64;"
    (.encodeToString
      (java.util.Base64/getMimeEncoder)
      (.getBytes (slurp filename)))))

;; Fever Authentication

(defonce ^:dynamic *api-config*
  {:email "infowarss@irq0.org"
   :pass  "foo"
   :version 42})

(defn api-key
  "Return fever api key"
  []
  (let [{:keys [email pass]} *api-config*]
    (digest/md5 (str email ":" pass))))

(defn- auth? [key]
  (= (string/lower-case (str key)) (api-key)))

(defn- auth-error []
  (response/status (response/response "Auth error!") 401))

(defn wrap-auth
  "Check fever api key and set :fever-auth in request
   Overwrite response with (fever-auth-error) if unauthorized"
  [handler]
  (fn [request]
    (let [api-key ((request :params) :api_key)
          auth? (auth? api-key)
          request (assoc-in request [:fever-auth] auth?)
          response (handler request)]
      (if auth?
        (auth-error)))))

;; Fever API

(s/defn all-feeds-group :- schema/FeverFeedsGroup
  "Return group containing all feeds"
  []
  (let [feedids (->> @*srcs*
                  vals
                  (map :src)
                  (map fever-feed-id))]
    {:group_id (fever-group-id-for-tag :all)
     :feed_ids (string/join "," feedids)}))

(defn- feedids-for-tag
  [tag]
  (try+
    (->> @*srcs*
      vals
      (filter (fn [src] (-> src :tags (contains? tag))))
      (map :src)
      (map fever-feed-id))
    (catch Object _
      [])))

(s/defn tag-feeds-group :- schema/FeverFeedsGroup
  "Return group containing all feed items with tag"
  [tag]
    {:group_id (fever-group-id-for-tag tag)
     :feed_ids (string/join "," (feedids-for-tag tag))})

(s/defn feeds-groups :- schema/FeverFeedsGroups
  "Return feeds_groups array"
  []
  [(all-feeds-group)
   (tag-feeds-group :jobs)
   (tag-feeds-group :personal)])

(s/defn groups  :- schema/FeverGroups
  "Return feed groups"
  []
  {:last_refreshed_on_time 0
   :groups [{:id (fever-group-id-for-tag :all),
             :title "All"}
            {:id (fever-group-id-for-tag :jobs)
             :title "Jobs"}
            {:id (fever-group-id-for-tag :personal)
             :title "Personal"}]
   :feeds_groups (feeds-groups)})

(s/defn feed :- schema/FeverFeed
  "Convert infowarss src to fever feed"
  [src]
  (let [[k {:keys [src state]}] src]
    {:id (fever-feed-id src)
     :favicon_id 1337
     :title (str src) ;; FIXME use stored feed names
     :url (-> src :url str)
     :site_url (-> src :url str)
     :is_spark 0
     :last_updated_on_time (-> state
                             :last-successful-fetch-ts
                             fever-timestamp)}))

(s/defn feeds :- schema/FeverFeeds
  "Return fever feeds"
  []
  (let [feedids
        (->> @*srcs*
          vals
          (map :src)
          (map fever-feed-id))]
  {:last_refreshed_on_time 0,
   :feeds (for [src @*srcs*]
            (feed src))
   :feeds_groups (feeds-groups)}))

(s/defn dummy-favicon :- schema/FeverFavicon
  []
  {:id 1337
   :data (mk-favicon (io/resource "favicon.ico"))})

(s/defn favicons :- schema/FeverFavicons
  "Return favicons for fever"
  []
  {:favicons [(dummy-favicon)]})

(s/defn item :- schema/FeverItem
  "Convert infowarss document to fever feed item"
  [doc]
  (let [contents (get-in doc [:feed-entry :contents])]
  {:id (fever-item-id (:_id doc))
   :feed_id (-> doc :meta :source-name fever-feed-id)
   :title (-> doc :summary :title)
   :author (as-> doc d (get-in d [:feed-entry :authors]) (string/join ", " d))
   :html (or (get contents "text/html") (get contents "text/plain"))
   :url (-> doc :feed-entry :url str)
   :is_saved 0
   :is_read (if (-> doc :meta :tags (contains? :unread)) 1 0)
   :created_on_time (-> doc :summary :ts fever-timestamp)}))

(s/defn items :- schema/FeverItems
  "
  Return feed items for fever. Call without params to get all items
  Limit return values with since, max, with
  "
  [params]
  (let [{:keys [since max with]} params
        ids (->> (couch/all-doc-ids)
              (map (fn [couchid] [(fever-item-id couchid) couchid]))
              (into (sorted-map)))

        filtered-ids (cond->> ids
                       (number? since) (filter (fn [[fid _]] (< since fid )))
                       (number? max) (filter (fn [[fid _]] (>= max fid)))
                       (seq with) (filter (fn [[fid _]] ((set with) fid))))
        ids-to-return (->> filtered-ids
                        (vals)
                        (take 50))]
    {:last_refreshed_on_time 0
     :total_items (count ids)
     :items (for [id ids-to-return
                  :let [doc (couch/get-document-with-attachments id)]]
              (item doc))}))

;; Or the first page (page=1) of Hot links for the past week (range=7)
;; starting now (offset=0).

(s/defn dummy-link :- schema/FeverLink
  []
  {:id 10
   :feed_id 23
   :item_id 1
   :temperature 0.5
   :is_item 1
   :is_local 1
   :is_saved 1
   :title "Example"
   :url "http://example.com"
   :item_ids ""})

(s/defn links :- schema/FeverLinks
  "Return fever links"
  []
  {:links [(dummy-link)]})

(s/defn unread-item-ids :- schema/FeverUnreadItemIds
  "Return ids of unread items"
  []
  {:unread_item_ids
   (string/join ","
     (map fever-item-id (couch/doc-ids-with-tag :unread)))})

(s/defn saved-item-ids :- schema/FeverSavedItemIds
  "Return ids of saved items"
  []
  {:saved_item_ids
   (string/join ","
     (map fever-item-id (couch/doc-ids-with-tag :saved)))})

(defn api-root
  "Return map with data to merge with specific requests on every reply"
  []
  {:api_version 3
   :auth 1})

;; write support:
;; {:as "read",
;;  :id "1275582412",
;;  :mark "item",
;;  :response_required "false"}

(defn find-couchid-by-feverid
  [fid]
  (let [ids (->> (couch/all-doc-ids)
             (map (fn [couchid] [(fever-item-id couchid) couchid]))
             (into (hash-map)))
        couchid (get ids fid)]
    (when (nil? couchid)
      (throw+ {:type ::couchid-not-found}))
    couchid))

(defn- modify-tags
  [id f]
  (let [couchid (find-couchid-by-feverid id)]
    (log/debugf "[Fever API] modify tag of %s/%s"
      id couchid)
    (couch/swap-document! couchid
      (fn [doc] (log/spy doc) (update-in doc [:meta :tags] f)))))

(defn- remove-tag
  [id tag]
  (log/debugf "[Fever API] remove tag %s from %s"
    tag id)
  (modify-tags id (fn [m] (-> m set (disj tag)))))

(defn- set-tag
  [id tag]
  (log/debugf "[Fever API] set tag %s on %s"
    tag id)
  (modify-tags id (fn [m] (-> m (conj tag) set))))

(defn handle-write-op
  [req]
  (try+
    (let [{:keys [mark as id before]} (:params req)
          id (Long/parseLong id)]
      (log/infof "[Fever API] WRITE OP (%s:%s -> %s)"
        mark id as)
      (cond
        (and (= mark "item") (= as "read"))
        (remove-tag id "unread")
        (and (= mark "item") (= as "unread"))
        (set-tag id "unread")
        :else
        (log/error "Unsupported write op")))
    (catch Object _
      (log/error "Unexpected error: " (:throwable &throw-context))
      (log/spy (:params req))))
  {})

(defn extract-op
  "Extract fever api operation from request"
  [req]
  (->> req
    :query-string
    (re-find #"^api&?(\w+)?&?.*$")
    second
    keyword))

(defn handle-request
  "Dispatch fever api operation to handler function"
  [req]
  (let [op (extract-op req)
        params (:params req)]
    (s/with-fn-validation
      (log/infof "[Fever API] OP: %s params: %s" op params)
      (condp = op
        :groups (groups)
        :feeds (feeds)
        :items (items {:since
                       (swallow-exceptions (Long/parseLong (get params :since_id)))
                       :max
                       (swallow-exceptions (Long/parseLong (get params :max_id)))
                       :with
                       (swallow-exceptions (map #(Long/parseLong %) (re-seq #"\d+" (get params "with_ids"))))})
        :links (links)
        :favicons (favicons)
        :unread_item_ids (unread-item-ids)
        :saved_item_ids (saved-item-ids)
        nil (handle-write-op req)
        {}))))


(defn fever-api
  "Fever API ring handler"
  [req]
  (let [resp-body (handle-request req)]
    (log/debugf "[Fever API RESPONSE] %s" resp-body)
    (response/response
      (merge resp-body (api-root)))))
