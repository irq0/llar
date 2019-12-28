(ns infowarss.apis.fever
  (:require
   [infowarss.core :refer [*srcs*]]
   [infowarss.db :as db]
   [infowarss.schema :as schema]
   [infowarss.update :refer [sources-merge-in-state]]
   [infowarss.logging :refer [log-status]]
   [ring.util.response :as response]
   [digest]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [schema.core :as s]
   [clojure.java.io :as io]))

;;;; Fever compatible API

(defn fever-feed-id [x])

;; Utility functions

(defmacro swallow-exceptions [& body]
  `(try ~@body (catch Exception e#)))

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


(defn last-refresh []
  (->> (db/get-sources)
    db/sources-merge-in-config
    sources-merge-in-state
    vals
    (map :last-successful-fetch-ts)
    sort last))

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
  (let [sources (vals (db/get-sources))
        feedids (map :id sources)]
    {:group_id (fever-group-id-for-tag :all)
     :feed_ids (string/join "," feedids)}))

(defn- feedids-for-tag
  [tag]
  (try+
    (->> (db/get-sources)
      db/sources-merge-in-config
      vals
      (filter (fn [src] (->> src :tags (some #(= tag %)))))
      (map :id))
    (catch Object _
      (log/error (:throwable &throw-context) tag)
      [])))

(defn- feedids-for-type
  [type]
  (try+
    (->>
      (db/get-sources)
      db/sources-merge-in-config
      vals
      (filter (fn [feed] (= (get feed :type) (keyword "item-type" (name type)))))
      (map :id))
    (catch Object _
      [])))

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
  (let [sources (db/get-sources)]
    {:group_id (fever-group-id-for-tag :special)
     :feed_ids (string/join ","
                 [(get-in sources [:bookmark :id])
                  (get-in sources [:document :id])])}))


(s/defn feeds-groups :- schema/FeverFeedsGroups
  "Return feeds_groups array"
  []
  [(all-feeds-group)
   (tag-feeds-group :jobs) (tag-feeds-group :personal) (tag-feeds-group :events)
   (tag-feeds-group :reddit) (tag-feeds-group :comics) (tag-feeds-group :music)
   (special-feeds-group)
   (type-feeds-group :mail) (type-feeds-group :tweet) (type-feeds-group :link) (type-feeds-group :feed)])

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
            {:id (fever-group-id-for-tag :music)
             :title "Music"}
            {:id (fever-group-id-for-tag :type-tweet)
             :title "Twitter"}
            {:id (fever-group-id-for-tag :type-link)
             :title "Links"}
            {:id (fever-group-id-for-tag :type-mail)
             :title "Mails"}
            {:id (fever-group-id-for-tag :special)
             :title "[Special]"}
            ;; {:id (fever-group-id-for-tag :nwords)
            ;;  :title "[Time / Word Count]"}
            {:id (fever-group-id-for-tag :type-feed)
             :title "Feeds"}]
   :feeds_groups (feeds-groups)})

(s/defn feed :- schema/FeverFeed
  "Convert infowarss src to fever feed"
  [source]
  (let [{:keys [last-successful-fetch-ts name title url id]} source]
    {:id id
     :favicon_id 1337
     :title (or title name)
     :url (str url)
     :site_url (str url)
     :is_spark 0
     :last_updated_on_time (fever-timestamp last-successful-fetch-ts)}))

(s/defn feeds :- schema/FeverFeeds
  "Return fever feeds"
  []
  {:feeds (map feed (->> (db/get-sources)
                      (sources-merge-in-state)
                      vals
                      (remove #(nil? (:id %)))))
   :feeds_groups (feeds-groups)})

(s/defn dummy-favicon :- schema/FeverFavicon
  []
  {:id 1337
   :data (mk-favicon (io/resource "favicon.ico"))})

(s/defn favicons :- schema/FeverFavicons
  "Return favicons for fever"
  []
  {:favicons [(dummy-favicon)]})

(defn get-html-content [doc]
  ;; XXX add vew-hints support
  (let [hint (get-in doc [:meta :view-hints :html])
        description (get-in doc [:data :description])
        contents (get-in doc [:data :content])]
    (if-not (nil? hint)
      (let [path (concat (map keyword (butlast hint)) [(last hint)])]
        (get-in doc path))
      (or (get contents "text/html")
           (get contents "text/plain")
           (get description "text/html")
           (get description "text/plain")
           ""))))

;; (s/defn item :- schema/FeverItem
;;   "Convert infowarss document to fever feed item"
;;   [doc]
;;   (let [meta (get doc :meta)
;;         feed-info {:source-name (get meta :source-name)
;;                    :source-key (keyword (get meta :source-key))}]

;;   {:id (fever-item-id (:_id doc))
;;    :feed_id (fever-feed-id feed-info)
;;    :title (str (or (-> doc :summary :title) ""))
;;    :author (as-> doc d (get-in d [:entry :authors]) (string/join ", " d))
;;    :html (get-html-content doc)
;;    :url (-> doc :entry :url str)
;;    :is_saved 0
;;    :is_read (if (some #{"unread"} (get-in doc [:meta :tags])) 0 1)
;;    :created_on_time (-> doc :summary :ts fever-timestamp)}))

(defn items
  "
  Return feed items for fever. Call without params to get all items
  Limit return values with since, max, with
  "
  [& {:keys [since max with]}]
  (let [items (cond
                (number? since)
                (db/get-items-by-id-range-fever since max :limit 100)

                (seq with)
                (db/get-items-by-id-fever with :limit 50)

                :default
                [])
        items-with-html (map (fn [item]
                               (-> item
                                 (assoc :html (get-html-content item))
                                 (dissoc :data)))
                          items)]
    (s/validate schema/FeverItems
      {:total_items (count items)
       :items items-with-html})))

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
   (string/join "," (db/get-item-ids-by-tag :unread))})

(s/defn saved-item-ids :- schema/FeverSavedItemIds
  "Return ids of saved items"
  []
  {:saved_item_ids
   (string/join "," (db/get-item-ids-by-tag :saved))})

(defn api-root
  "Return map with data to merge with specific requests on every reply"
  []
  {:api_version 3
   :last_refreshed_on_time (fever-timestamp (last-refresh))
   :auth 1})

(defn handle-write-op
  [req]
  (let [req-uuid (:req-uuid req)
        {:keys [mark as id before]} (:params req)]
    (try+
      (let [id (Long/parseLong id)]
        (log/debugf "[FEVER/%s] WRITE OP (%s:%s -> %s)"
          req-uuid mark id as)
        (cond
          (and (= mark "item") (= as "read"))
          (db/item-remove-tags id :unread)
          (and (= mark "item") (= as "unread"))
          (db/item-set-tags id :unread)
          (and (= mark "item") (= as "saved"))
          (db/item-set-tags id :saved)
          (and (= mark "item") (= as "unsaved"))
          (db/item-remove-tags :saved)
          (and (= mark "feed") (= as "read"))
          (db/item-set-all-tags-for-source id)
          :else
          (log/errorf "[FEVER/%s] Unsupported write op: " req-uuid (:params req))))
      (catch Object _
        (log/error (:throwable &throw-context)
          (str "[FEVER/" req-uuid "] Unexpected exception, params:")
          (:params req))))
    {}))

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
  (let [req-uuid (java.util.UUID/randomUUID)
        op (extract-op req)
        params (:params req)]
    (log/debugf "[FEVER/%s] OP: %s params: %s query-string: %s"
                req-uuid op params (:query-string req))
    (try+
     (let [response (condp = op
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
                      nil (when (get-in req [:params :id])
                            (handle-write-op (assoc req :req-uuid req-uuid)))
                      {})]
       (if (empty? response)
         (log/tracef "[FEVER/%s] OP: %s empty response" req-uuid op)
         (log/tracef "[FEVER/%s] OP: %s response: %s" req-uuid
                     op (into {} (map (fn [[k v]]
                                        [k (cond
                                             (number? v)
                                             v
                                             (string? v)
                                             (format "type=%s n=%s first_20=%s.."
                                                     (type v) (count v) (subs v 0 (min (count v) 20)))
                                             (sequential? v)

                                             (let [cnt (count v)
                                                   ids (remove nil? (map :id v))
                                                   type (type v)]
                                               (if (some? ids)
                                                 (format "type=%s n=%s id_max=%s id_min=%s ids=%s"
                                                         type cnt
                                                         (when (pos? (count ids)) (apply max ids))
                                                         (when (pos? (count ids)) (apply min ids))
                                                         (pr-str ids))
                                                 (format "type=%s n=%s"
                                                         type cnt)))
                                             :default
                                             (format "type=%s" (type v)))])
                                      response))))
       response)
     (catch [:type :schema.core/error] {:keys [schema value]}
       (log/error (:throwable &throw-context) (str "[FEVER/" req-uuid "] Broken response, schema:" schema))
       (log/trace (str "[FEVER/" req-uuid "] Value of broken response" value)))
     (catch Object _
       (log/error (:throwable &throw-context) (str "[FEVER/" req-uuid "] Broken response for OP: ")
                  op params)))))


(defn fever-api
  "Fever API ring handler"
  [req]
  (let [resp-body (handle-request req)]
    (response/response
      (merge resp-body (api-root)))))
