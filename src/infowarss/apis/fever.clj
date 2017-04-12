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

(defmacro swallow-exceptions [& body]
  `(try ~@body (catch Exception e#)))

(defonce ^:dynamic *api-config*
  {:email "infowarss@irq0.org"
   :pass  "foo"
   :version 42})

(defn api-key []
  (let [{:keys [email pass]} *api-config*]
    (digest/md5 (str email ":" pass))))

(defn- auth? [key]
  (= (string/lower-case (str key)) (api-key)))

(defn- auth-error []
  (-> (response/response "Auth error!")
      (response/status 401)))

(defn- fever-item-id [id]
  (Long/parseUnsignedLong (subs id 24 32) 16))

(defn- fever-feed-id [id]
  (let [hash (digest/sha-256 id)]
    (Long/parseUnsignedLong (subs hash 0 8) 16)))


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

(def fever-timestamp
  (fnil tc/to-long 0))

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


(s/defn all-feeds-group :- FeverFeedsGroup [feedids]
  {:group_id 1
    :feed_ids (string/join "," feedids)})

(s/defn groups  :- FeverGroups []
  (let [feedids
        (->> @*srcs*
          vals
          (map #(get-in % [:src :title]))
          (map fever-feed-id))]

  {:last_refreshed_on_time 0
   :groups [{:id 1, :title "All"}]
   :feeds_groups [(all-feeds-group feedids)]}))

(s/defn feed :- FeverFeed [src]
  (let [[k {:keys [src state]}] src]
    {:id (fever-feed-id (:title src))
     :favicon_id 1337
     :title (:title src)
     :url (:url src)
     :site_url (:url src)
     :is_spark 0
     :last_updated_on_time (-> state
                             :last-successful-fetch-ts
                             fever-timestamp)}))


(s/defn feeds :- FeverFeeds []
  (let [feedids
        (->> @*srcs*
          vals
          (map #(get-in % [:src :title]))
          (map fever-feed-id))]
  {:last_refreshed_on_time 0,
   :feeds (for [src @*srcs*]
            (feed src))
   :feeds_groups [(all-feeds-group feedids)]}))

(s/defn dummy-favicon :- FeverFavicon []
  {:id 1337
   :data (mk-favicon (io/resource "favicon.ico"))})

(s/defn favicons :- FeverFavicons []
  {:favicons [(dummy-favicon)]})

(s/defn item :- FeverItem [doc]
  (let [contents (get-in doc [:feed-entry :contents])]
  {:id (fever-item-id (:_id doc))
   :feed_id (-> doc (get-in [:source :title]) (fever-feed-id ))
   :title (get-in doc [:summary :title])
   :author (-> doc (get-in [:feed-entry :authors]) (string/join ", " ))
   :html (or (get contents "text/html") (get contents "text/plain"))
   :url (get-in doc [:feed-entry :link])
   :is_saved 0
   :is_read (if (-> doc (get-in [:meta :tags]) (contains? :unread)) 1 0)
   :created_on_time (-> doc (get-in [:feed :published-date]) fever-timestamp)}))

(defn items
  ([] (do-items {}))
  ([params] (do-items params)))

(s/defn do-items :- FeverItems [params]
  (let [{:keys [since max with]} params
        ids (->> (couch/all-doc-ids)
              (map (fn [couchid] [(fever-feed-id couchid) couchid]))
              (into (sorted-map)))

        filtered-ids (cond->> ids
                       (number? since) (filter (fn [[fid _]] (<= since fid )))
                       (number? max) (filter (fn [[fid _]] (> max fid)))
                       (not (empty? with)) (filter (fn [[fid _]] ((set with) fid))))
        ids-to-return (->> filtered-ids
                        (vals)
                        (take 50))]

    (log/spy (seq ids))
    (log/spy since)
    (log/spy max)
    (log/spy with)
    (log/spy (seq ids-to-return))

    {:last_refreshed_on_time 0
     :total_items (count ids)
     :items (for [id ids-to-return
                  :let [doc (couch/get-document-with-attachments id)]]
              (item doc))}))


;; Or the first page (page=1) of Hot links for the past week (range=7)
;; starting now (offset=0).
(s/defn links :- FeverLinks
  []
  {:links [{:id 10
            :feed_id 23
            :item_id 1
            :temperature 0.5
            :is_item 1
            :is_local 1
            :is_saved 1
            :title "Example"
            :url "http://example.com"
            :item_ids ""}]})

;; return: string/comma-separated list of positive integers)
(s/defn unread-item-ids :- FeverUnreadItemIds
  []
  {:unread_item_ids
   (string/join ","
     (map fever-item-id (couch/doc-ids-with-tag :unread)))})

(s/defn saved-item-ids :- FeverSavedItemIds
  []
  {:saved_item_ids ""})

(defn api-root []
  {:api_version 3
   :auth 1})



;; {:as "read",
;;  :id "1275582412",
;;  :mark "item",
;;  :response_required "false"}

(defn handle-op [op params]
  (s/with-fn-validation
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
      nil {}
      {})))

(defn extract-op [req]
  (->> req
    :query-string
    (re-find #"^api&?(\w+)?&?.*$")
    second
    keyword))

(defn fever-api [req]
  (let [op (extract-op req)
        params (:query-params req)
        resp-body (handle-op op params)]
    (log/debugf "[Fever API REQ ] %s - %s" op params)
    (log/debugf "[Fever API RESP] %s" resp-body)
    (response/response (-> resp-body
                         (merge (api-root)))

                         )))
