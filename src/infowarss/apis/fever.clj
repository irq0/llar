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
  (-> (response/response "Auth error!")
      (response/status 401)))

(defn wrap-auth
  "Check fever api key and set :fever-auth in request
   Overwrite response with (fever-auth-error) if unauthorized"
  [handler]
  (fn [request]
    #spy/p request
    (let [api-key ((request :params) :api_key)
          auth? (auth? api-key)
          request (assoc-in request [:fever-auth] auth?)
          response (handler request)]
      (if auth?
        #spy/p response
        (auth-error)))))


;; Fever API Schema

(def FeverPosInt
  (s/constrained s/Num pos?))

(def FeverPosFloat
  (s/constrained s/Num pos?))

(def FeverBoolInt
  (s/constrained s/Num #{0 1}))

(def FeverUnixTimestamp
  (s/constrained s/Num (partial <= 0)))

(def FeverIntList
  (s/constrained s/Str (partial re-matches #"(\d+(,\d+)*)?")))

(def FeverImageData
  (s/constrained s/Str #(.startsWith % "image/gif;base64;")))

(def FeverURLStr
  (s/constrained s/Str io/as-url))

(def FeverAPIRoot
  {:api_version FeverPosInt
   :auth FeverBoolInt})

(def FeverGroup
  "Fever API: group object"
  {:id FeverPosInt
   :title s/Str})

(def FeverFeedsGroup
  "Fever API: feeds_group object"
  {:group_id FeverPosInt
   :feed_ids FeverIntList})

(def FeverFeedsGroups
  "Fever API: feeds_groups object"
  [FeverFeedsGroup])

(def FeverGroups
  "Fever API: groups object"
  {(s/optional-key :last_refreshed_on_time) FeverUnixTimestamp
   :groups [FeverGroup]
   :feeds_groups [FeverFeedsGroup]})

(def FeverAPIGroups
  "Fever API: groups response"
  (merge FeverAPIRoot FeverGroups))

(def FeverFeed
  "Fever API: feed object"
  {:id FeverPosInt
   :favicon_id FeverPosInt
   :title s/Str
   :url s/Str
   :site_url s/Str
   :is_spark FeverBoolInt
   :last_updated_on_time FeverUnixTimestamp})

(def FeverFeeds
  "Fever API: feeds object"
  {(s/optional-key :last_refreshed_on_time) FeverUnixTimestamp
   :feeds [FeverFeed]
   :feeds_groups [FeverFeedsGroup]})

(def FeverAPIFeeds
  "Fever API: feeds request"
  (merge FeverAPIRoot FeverFeeds))

(def FeverFavicon
  "Fever API: favicon object"
  {:id FeverPosInt
   :data FeverImageData})

(def FeverFavicons
  "Fever API: favicons object"
  {:favicons [FeverFavicon]})

(def FeverAPIFavicons
  "Fever API: favicons request"
  (merge FeverAPIRoot FeverFavicons))

(def FeverItem
  "Fever API: item object"
  {:id FeverPosInt
   :feed_id FeverPosInt
   :title s/Str
   :author s/Str
   :html s/Str
   :url FeverURLStr
   :is_saved FeverBoolInt
   :is_read FeverBoolInt
   :created_on_time FeverUnixTimestamp})

(def FeverItems
  "Fever API: items object"
  {(s/optional-key :last_refreshed_on_time) FeverUnixTimestamp
   :items [FeverItem]
   :total_items (s/constrained s/Num (partial < 0))})

(def FeverAPIItems
  "Fever API: items request"
  (merge FeverAPIRoot FeverItems))

(def FeverLink
  "Fever API: link object"

  {:id FeverPosInt
   :feed_id FeverPosInt
   :item_id FeverPosInt
   :temperature FeverPosFloat
   :is_item FeverBoolInt
   :is_local FeverBoolInt
   :is_saved FeverBoolInt
   :title s/Str
   :url FeverURLStr
   :item_ids FeverIntList})

(def FeverLinks
  "Fever API: links object"
  {:links [FeverLink]})

(def FeverAPILinks
  "Fever API: links request"
  (merge FeverAPIRoot FeverLinks))

(def FeverUnreadItemIds
  "Fever API: unread_item_ids object"
  {:unread_item_ids FeverIntList})

(def FeverAPIUnreadItemIds
  "Fever API: unread_item_ids request"
  (merge FeverAPIRoot FeverUnreadItemIds))

(def FeverSavedItemIds
  "Fever API: saved_item_ids object"
  {:saved_item_ids FeverIntList})

(def FeverAPISavedItemIds
  "Fever API: saved_item_ids request"
  (merge FeverAPIRoot FeverSavedItemIds))


;; Fever API

(s/defn all-feeds-group :- FeverFeedsGroup
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

(s/defn tag-feeds-group :- FeverFeedsGroup
  "Return group containing all feed items with tag"
  [tag]
    {:group_id (fever-group-id-for-tag tag)
     :feed_ids (string/join "," (feedids-for-tag tag))})

(s/defn feeds-groups :- FeverFeedsGroups
  "Return feeds_groups array"
  []
  [(all-feeds-group)
   (tag-feeds-group :jobs)
   (tag-feeds-group :personal)])

(s/defn groups  :- FeverGroups
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

(s/defn feed :- FeverFeed
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

(s/defn feeds :- FeverFeeds
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

(s/defn dummy-favicon :- FeverFavicon
  []
  {:id 1337
   :data (mk-favicon (io/resource "favicon.ico"))})

(s/defn favicons :- FeverFavicons
  "Return favicons for fever"
  []
  {:favicons [(dummy-favicon)]})

(s/defn item :- FeverItem
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

(s/defn items :- FeverItems
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
                       (not (empty? with)) (filter (fn [[fid _]] ((set with) fid))))
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

(s/defn dummy-link :- FeverLink
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

(s/defn links :- FeverLinks
  "Return fever links"
  []
  {:links [(dummy-link)]})

(s/defn unread-item-ids :- FeverUnreadItemIds
  "Return ids of unread items"
  []
  {:unread_item_ids
   (string/join ","
     (map fever-item-id (couch/doc-ids-with-tag :unread)))})

(s/defn saved-item-ids :- FeverSavedItemIds
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
    (let [{:keys [mark as id before]} (-> req :params)
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
      (log/spy (-> req :params))))
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
        params (-> req :params)]
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
      (-> resp-body
        (merge (api-root))))))
