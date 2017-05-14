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

;;;; Fever compatible API

;; Utility functions

(defmacro swallow-exceptions [& body]
  `(try ~@body (catch Exception e#)))

(defn fever-item-id
  "Convert infowarss feed item id to fever compatible id"
  [id]
  ;; our database keys are to long for the fever api
  ;; shorten to positive java long
  (let [parts [(subs id 0 4)  (subs id 28 32)]]
    (Long/parseLong (string/join parts) 16)))

(defn fever-feed-id
  "Calculate pseudo id from database feed entry"
  [db-feed]
  (let [{:keys [source-name source-key]} db-feed
        source-key (if (keyword? source-key) (name source-key) source-key)]
    (when (or (string/blank? source-name)
            (string/blank? source-key))
      (throw+ {:type ::invalid-input :db-feed db-feed}))

    (let [data (string/join source-name source-key)
          hash (digest/sha-256 data)]
      (Long/parseUnsignedLong (subs hash 0 8) 16))))

(defn fever-group-id-for-tag
  "Convert infowarss feed id to fever compatible id"
  [tag]
  (let [hash (digest/sha-256 (name tag))]
    (Long/parseUnsignedLong (subs hash 0 8) 16)))


(defn fever-timestamp
  "Convert clj-time time object to fever unix timestamp"
  [time]
  (try+
    (-> time
      tc/to-long
      (/ 1000)
      (.longValue)
      (max 0))
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
        response
        (auth-error)))))

;; Fever API

(s/defn all-feeds-group :- schema/FeverFeedsGroup
  "Return group containing all feeds"
  []
  (let [feeds (couch/get-feeds)
        feedids (map fever-feed-id feeds)]
    {:group_id (fever-group-id-for-tag :all)
     :feed_ids (string/join "," feedids)}))

(defn db-feeds-with-config
  []
  (let [dbfs (couch/get-feeds)
        confs *srcs*]
    (into {}
      (for [dbf dbfs]
        [(get dbf :source-key)
         (merge dbf (get confs (get dbf :source-key)))]))))

(defn- feedids-for-tag
  [tag]
  (try+
    (->> (db-feeds-with-config)
      vals
      (filter (fn [src] (->> src :tags (some #(= tag %)))))
      (map fever-feed-id))
    (catch Object _
      [])))

(defn- feedids-for-type
  [type]
  (try+
    (->> (db-feeds-with-config)
      vals
      (filter (fn [feed] (= (get feed :type) type)))
      (map fever-feed-id))
    (catch Object _
      [])))

(defn last-refresh []
  0)
(comment
  (let [feeds (couch/get-feeds)
        ts (map #(-> % :last-fetch-ts fever-timestamp) feeds)]
    (apply max ts)))


(s/defn tag-feeds-group :- schema/FeverFeedsGroup
  "Return group containing all feed items with tag"
  [tag]
    {:group_id (fever-group-id-for-tag tag)
     :feed_ids (string/join "," (feedids-for-tag tag))})

(s/defn type-feeds-group :- schema/FeverFeedsGroup
  "Return group containing all feed items with tag"
  [type]
    {:group_id (fever-group-id-for-tag (keyword (str "type-" (name type))))
     :feed_ids (string/join "," (feedids-for-type type))})

(s/defn special-feeds-group :- schema/FeverFeedsGroup
  "Return group containing all speicla feeds (e.g bookmarks)"
  []
  (let [feeds (db-feeds-with-config)]
    {:group_id (fever-group-id-for-tag :special)
     :feed_ids (string/join ","
                 [(fever-feed-id (:bookmark feeds)) (fever-feed-id (:document feeds))])}))


(s/defn feeds-groups :- schema/FeverFeedsGroups
  "Return feeds_groups array"
  []
  [(all-feeds-group)
   (tag-feeds-group :jobs) (tag-feeds-group :personal) (tag-feeds-group :events) (tag-feeds-group :reddit) (tag-feeds-group :comics)
   (special-feeds-group)
   (type-feeds-group :tweet) (type-feeds-group :link) (type-feeds-group :feed)])

(s/defn groups  :- schema/FeverGroups
  "Return feed groups"
  []
  {:groups [{:id (fever-group-id-for-tag :all),
             :title "All"}
            {:id (fever-group-id-for-tag :jobs)
             :title "Jobs"}
            {:id (fever-group-id-for-tag :personal)
             :title "Personal"}
            {:id (fever-group-id-for-tag :events)
             :title "Events"}
            {:id (fever-group-id-for-tag :reddit)
             :title "Reddit"}
            {:id (fever-group-id-for-tag :comics)
             :title "Comics"}
            {:id (fever-group-id-for-tag :type-tweet)
             :title "Twitter"}
            {:id (fever-group-id-for-tag :type-link)
             :title "Links"}
            {:id (fever-group-id-for-tag :special)
             :title "[Special]"}
            {:id (fever-group-id-for-tag :type-feed)
             :title "Feeds"}]
   :feeds_groups (feeds-groups)})

(s/defn feed :- schema/FeverFeed
  "Convert infowarss src to fever feed"
  [feed]
  (let [{:keys [last-fetch-ts title url]} feed]
    {:id (fever-feed-id feed)
     :favicon_id 1337
     :title title
     :url (str url)
     :site_url (str url)
     :is_spark 0
     :last_updated_on_time (fever-timestamp last-fetch-ts)}))

(s/defn feeds :- schema/FeverFeeds
  "Return fever feeds"
  []
  {:feeds (for [f (couch/get-feeds)] (feed f))
   :feeds_groups (feeds-groups)})

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
  (let [contents (get-in doc [:entry :contents])
        description (get-in doc [:entry :description])
        meta (get doc :meta)
        feed-info {:source-name (get meta :source-name)
                   :source-key (keyword (get meta :source-key))}]
  {:id (fever-item-id (:_id doc))
   :feed_id (fever-feed-id feed-info)
   :title (-> doc :summary :title)
   :author (as-> doc d (get-in d [:entry :authors]) (string/join ", " d))
   :html (or (get contents "text/html")
           (get contents "text/plain")
           (get description "text/html")
           (get description "text/plain")
           "")
   :url (-> doc :entry :url str)
   :is_saved 0
   :is_read (if (some #{"unread"} (get-in doc [:meta :tags])) 0 1)
   :created_on_time (-> doc :summary :ts fever-timestamp)}))

(defn items
  "
  Return feed items for fever. Call without params to get all items
  Limit return values with since, max, with
  "
  [& {:keys [since max with]}]
  (let [ids (->> (couch/all-doc-ids)
              (map (fn [couchid] [(fever-item-id couchid) couchid]))
              (into (sorted-map)))

        filtered-ids (cond->> ids
                       (number? since) (filter (fn [[fid _]] (< since fid )))
                       (number? max) (filter (fn [[fid _]] (>= max fid)))
                       (seq with) (filter (fn [[fid _]] ((set with) fid))))
        ids-to-return (->> filtered-ids
                        (vals)
                        (take 50))]
    (s/validate schema/FeverItems
      {:total_items (count ids)
       :items (for [id ids-to-return
                    :let [doc (couch/get-document-with-attachments id)]]
                (item doc))})))

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
   :last_refreshed_on_time (last-refresh)
   :auth 1})

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
      (fn [doc] (update-in doc [:meta :tags] f)))))

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

(defn modify-tags-for-feed [feed-id tag f]
  (let [couch-ids (couch/doc-ids-with-tag tag)]
    (doseq [id couch-ids
            :let [doc (couch/get-document id)
                  cur-feed-id (fever-feed-id (:meta doc))]]
      (when (= feed-id cur-feed-id)
        (couch/swap-document! id
          (fn [old] (update-in old [:meta :tags] f)))))))

(defn mark-feed-read [feed-id]
  (log/debugf "[Fever API] marking feed %s as read" feed-id)
  (modify-tags-for-feed feed-id "unread" (fn [m] (-> m set (disj "unread")))))

(defn handle-write-op
  [req]
  (try+
    (let [{:keys [mark as id before]} (:params req)
          id (Long/parseLong id)]
      (log/debugf "[Fever API] WRITE OP (%s:%s -> %s)"
        mark id as)
      (cond
        (and (= mark "item") (= as "read"))
        (remove-tag id "unread")
        (and (= mark "item") (= as "unread"))
        (set-tag id "unread")
        (and (= mark "feed") (= as "read"))
        (mark-feed-read id)
        :else
        (log/error "Unsupported write op")))
    (catch Object _
      (log/errorf "Unexpected error (query params: %s): %s"
        (:params req) (:throwable &throw-context))))
  {})

(defn extract-op
  "Extract fever api operation from request"
  [req]
  (some->> req
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
      (log/debugf "[Fever API] OP: %s params: %s" op params)
      (condp = op
        :groups (groups)
        :feeds (feeds)
        :items (items :since
                       (swallow-exceptions (Long/parseLong (get params :since_id)))
                       :max
                       (swallow-exceptions (Long/parseLong (get params :max_id)))
                       :with
                       (swallow-exceptions (map #(Long/parseLong %) (re-seq #"\d+" (get params :with_ids)))))
        :links (links)
        :favicons (favicons)
        :unread_item_ids (unread-item-ids)
        :saved_item_ids (saved-item-ids)
        nil (when (contains? (:params req) :id) (handle-write-op req))
        {}))))


(defn fever-api
  "Fever API ring handler"
  [req]
  (let [resp-body (handle-request req)]
    (response/response
      (merge resp-body (api-root)))))
