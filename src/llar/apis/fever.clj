(ns llar.apis.fever
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :refer [ANY routes]]
   [digest]
   [java-time.api :as time]
   [llar.appconfig :as appconfig]
   [llar.config :as config]
   [llar.db.sql :as sql]
   [llar.persistency :as persistency])
  (:import
   [java.nio.charset StandardCharsets]
   [java.security MessageDigest]
   [org.jsoup Jsoup]
   [org.jsoup.nodes Entities]
   [org.jsoup.safety Safelist]))

(def api-version 3)
(def page-size 50)
(def flat-group-id 1)

(defn- datastore [db]
  (or (:datasource db) db))

(defn- config-with-defaults [fever-config]
  (merge {:source-tag :mobile
          :initial-days 30
          :recent-read-days 10
          :max-content-bytes 1048576}
         fever-config))

(defn expected-api-key [username password]
  (digest/md5 (str username ":" password)))

(defn- secure= [a b]
  (and (string? a)
       (string? b)
       (MessageDigest/isEqual (.getBytes a StandardCharsets/UTF_8)
                              (.getBytes b StandardCharsets/UTF_8))))

(defn- credential-password [{:keys [credentials]}]
  (let [credential (appconfig/credentials credentials)]
    (if (map? credential) (:password credential) credential)))

(defn authenticated? [fever-config api-key]
  (let [password (credential-password fever-config)]
    (and (string? password)
         (secure= api-key (expected-api-key (:username fever-config) password)))))

(defn- selected-source-keys [fever-config]
  (let [source-tag (:source-tag fever-config)]
    (->> (config/get-sources)
         (keep (fn [[source-key source]]
                 (when (contains? (:tags source) source-tag)
                   (name source-key))))
         set)))

(defn selected-sources [db fever-config]
  (let [keys (selected-source-keys fever-config)]
    (->> (sql/fever-sources (datastore db))
         (filter #(contains? keys (:key %)))
         vec)))

(defn- source-url [{:keys [key data]}]
  (or (get data :url)
      (str "llar://source/" key)))

(defn- epoch-millis [timestamp]
  (if timestamp
    (time/to-millis-from-epoch timestamp)
    0))

(defn- seconds [timestamp]
  (quot (epoch-millis timestamp) 1000))

(defn- feed-response [source]
  {:id (:id source)
   :favicon_id 0
   :title (:name source)
   :url (source-url source)
   :site_url (source-url source)
   :is_spark 0
   :last_updated_on_time (seconds (:updated_ts source))})

(defn- feeds-groups [sources]
  [{:group_id flat-group-id
    :feed_ids (string/join "," (map :id sources))}])

(defn- byte-truncate [s max-bytes]
  (if (<= (alength (.getBytes s StandardCharsets/UTF_8)) max-bytes)
    s
    (loop [low 0 high (count s)]
      (if (>= low high)
        (str (subs s 0 low) "\n<p><em>Content truncated by LLAR.</em></p>")
        (let [mid (quot (inc (+ low high)) 2)
              size (alength (.getBytes (subs s 0 mid) StandardCharsets/UTF_8))]
          (if (<= size max-bytes)
            (recur mid high)
            (recur low (dec mid))))))))

(defn sanitize-content [content html? base-url max-bytes]
  (let [html (if html?
               (or content "")
               (str "<p>" (Entities/escape (or content "")) "</p>"))
        safelist (doto (Safelist/relaxed)
                   (.addTags (into-array String ["video" "audio" "source"]))
                   (.addAttributes "a" (into-array String ["target"]))
                   (.addProtocols "img" "src" (into-array String ["http" "https" "data"])))
        cleaned (Jsoup/clean html (or base-url "") safelist)]
    (-> cleaned
        (byte-truncate max-bytes)
        (Jsoup/parseBodyFragment (or base-url ""))
        .body
        .html)))

(defn- item-response [item fever-config]
  {:id (:id item)
   :feed_id (:feed_id item)
   :title (:title item)
   :author (:author item)
   :html (sanitize-content (:content item)
                           (:content_is_html item)
                           (:url item)
                           (:max-content-bytes fever-config))
   :url (or (:url item) "")
   :is_saved (if (:is_saved item) 1 0)
   :is_read (if (:is_read item) 1 0)
   :created_on_time (seconds (:ts item))})

(defn- parse-positive-long [value]
  (when (and (string? value) (re-matches #"[0-9]+" value))
    (let [n (parse-long value)]
      (when (pos? n) n))))

(defn- parse-id-list [value]
  (when (string? value)
    (let [ids (mapv parse-positive-long (string/split value #","))]
      (when (and (<= (count ids) page-size) (every? some? ids)) ids))))

(defn- working-set-bounds [fever-config now]
  {:unread-after (time/minus now (time/days (:initial-days fever-config)))
   :read-after (time/minus now (time/days (:recent-read-days fever-config)))})

(defn- item-query [source-ids fever-config now params]
  (let [since-id (parse-positive-long (get params "since_id"))
        raw-max-id (get params "max_id")
        max-id (parse-positive-long raw-max-id)
        with-ids (parse-id-list (get params "with_ids"))]
    (when (and (contains? params "with_ids") (nil? with-ids))
      (throw (ex-info "Invalid with_ids" {:type ::bad-request})))
    (cond-> (merge {:source-ids source-ids
                    :descending? (and (nil? since-id) (nil? with-ids))
                    :limit page-size}
                   (working-set-bounds fever-config now))
      since-id (assoc :since-id since-id)
      (and raw-max-id (not= raw-max-id "0") max-id) (assoc :max-id max-id)
      with-ids (assoc :with-ids with-ids))))

(defn- state-query [source-ids fever-config now state]
  (merge {:source-ids source-ids
          :state-query state}
         (working-set-bounds fever-config now)))

(defn- comma-ids [rows]
  (string/join "," (map :id rows)))

(defn- selected-item? [db source-ids item-id]
  (boolean (:selected (sql/fever-item-selected (datastore db)
                                               {:source-ids source-ids
                                                :item-id item-id}))))

(defn- apply-item-action! [db source-ids params]
  (let [item-id (parse-positive-long (get params "id"))
        action (get params "as")]
    (when-not (and item-id (selected-item? db source-ids item-id))
      (throw (ex-info "Unknown item" {:type ::bad-request})))
    (case action
      "read" (persistency/item-remove-tags! db item-id [:unread])
      "saved" (persistency/item-set-tags! db item-id [:saved])
      "unsaved" (persistency/item-remove-tags! db item-id [:saved])
      (throw (ex-info "Unsupported item action" {:type ::bad-request})))))

(defn- before-timestamp [value]
  (when-let [epoch (parse-positive-long value)]
    (time/instant epoch 0)))

(defn- apply-bulk-action! [db source-ids params]
  (let [mark (get params "mark")
        id (or (parse-long (or (get params "id") "")) 0)
        before (before-timestamp (get params "before"))]
    (when-not (and (= "read" (get params "as")) before)
      (throw (ex-info "Invalid bulk action" {:type ::bad-request})))
    (case mark
      "feed" (when-not (and (pos? id) (contains? (set source-ids) id))
               (throw (ex-info "Unknown feed" {:type ::bad-request})))
      "group" (when-not (#{0 flat-group-id} id)
                (throw (ex-info "Unknown group" {:type ::bad-request})))
      (throw (ex-info "Unsupported bulk action" {:type ::bad-request})))
    (sql/fever-mark-read (datastore db)
                         (cond-> {:source-ids source-ids :before before}
                           (= mark "feed") (assoc :feed-id id)))))

(defn- base-response [sources]
  {:api_version api-version
   :auth 1
   :last_refreshed_on_time (or (some->> sources
                                        (keep :updated_ts)
                                        seq
                                        (apply max-key epoch-millis)
                                        seconds)
                               0)})

(defn fever-response [db fever-config request]
  (let [fever-config (config-with-defaults fever-config)
        params (:params request)]
    (if-not (authenticated? fever-config (get params "api_key"))
      {:status 200 :body {:api_version api-version :auth 0}}
      (let [sources (selected-sources db fever-config)
            source-ids (mapv :id sources)
            now (time/zoned-date-time)
            base (base-response sources)]
        (if (empty? source-ids)
          {:status 200
           :body (cond-> base
                   (contains? params "groups") (assoc :groups [] :feeds_groups [])
                   (contains? params "feeds") (assoc :feeds [] :feeds_groups [])
                   (contains? params "items") (assoc :items [] :total_items 0)
                   (contains? params "unread_item_ids") (assoc :unread_item_ids "")
                   (contains? params "saved_item_ids") (assoc :saved_item_ids ""))}
          (do
            (when-let [mark (get params "mark")]
              (if (= mark "item")
                (apply-item-action! db source-ids params)
                (apply-bulk-action! db source-ids params)))
            {:status 200
             :body (cond-> base
                     (contains? params "groups")
                     (assoc :groups [{:id flat-group-id :title "LLAR"}]
                            :feeds_groups (feeds-groups sources))

                     (contains? params "feeds")
                     (assoc :feeds (mapv feed-response sources)
                            :feeds_groups (feeds-groups sources))

                     (contains? params "favicons")
                     (assoc :favicons [])

                     (contains? params "items")
                     (assoc :items (mapv #(item-response % fever-config)
                                         (sql/fever-items (datastore db)
                                                          (item-query source-ids fever-config now params)))
                            :total_items (:total (sql/fever-total-items
                                                  (datastore db)
                                                  (merge {:source-ids source-ids}
                                                         (working-set-bounds fever-config now)))))

                     (or (contains? params "unread_item_ids")
                         (= "read" (get params "as")))
                     (assoc :unread_item_ids
                            (comma-ids (sql/fever-item-state-ids
                                        (datastore db)
                                        (state-query source-ids fever-config now "0"))))

                     (or (contains? params "saved_item_ids")
                         (#{"saved" "unsaved"} (get params "as")))
                     (assoc :saved_item_ids
                            (comma-ids (sql/fever-item-state-ids
                                        (datastore db)
                                        (state-query source-ids fever-config now "1")))))}))))))

(defn handler [db fever-config]
  (routes
   (ANY "/" request
     (try
       (fever-response db fever-config request)
       (catch clojure.lang.ExceptionInfo e
         (if (= ::bad-request (:type (ex-data e)))
           (let [params (:params request)]
             (log/warnf "Rejected Fever request: %s (mark=%s as=%s)"
                        (ex-message e)
                        (get params "mark")
                        (get params "as"))
             {:status 400
              :body {:api_version api-version :auth 1 :error (ex-message e)}})
           (throw e)))))))
