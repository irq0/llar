(ns llar.fetch.reddit
  (:require
   [llar.appconfig :as appconfig]
   [llar.fetch :as fetch]
   [llar.specs]
   [llar.persistency :as persistency]
   [llar.postproc :as postproc]
   [llar.analysis :as analysis]
   [llar.item]
   [llar.http :as llar-http :refer [with-http-exception-handler]]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [digest]
   [hiccup2.core :refer [html]]
   [clj-http.client :as http]
   [java-time.api :as time]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.tools.logging :as log]
   [org.bovinegenius [exploding-fish :as uri]]))

;; Reddit shut down the unauthenticated .json endpoints - they return HTTP 403 to
;; every client regardless of user agent. All requests go through the OAuth2 Data
;; API, which is free for non-commercial use at 100 queries per minute per client id.
;; https://support.reddithelp.com/hc/en-us/articles/16160319875092-Reddit-Data-API-Wiki
(def +reddit-api-url+ "https://oauth.reddit.com")
(def +reddit-token-url+ "https://www.reddit.com/api/v1/access_token")

;; Reddit honors the t (timeframe) parameter on these listings only. Sending it
;; elsewhere is silently ignored.
(def +reddit-timeframe-listings+ #{"top" "controversial"})

(defn- reddit-credentials []
  (let [{:keys [app-id secret] :as credentials} (appconfig/credentials :reddit)]
    (when (or (string/blank? app-id) (string/blank? secret))
      (throw+ {:type ::missing-credentials
               :message (str "reddit needs :app-id and :secret in credentials.edn. "
                             "Create a script app at https://www.reddit.com/prefs/apps")}))
    credentials))

;; Reddit requires the format <platform>:<app ID>:<version string> (by /u/<username>)
;; and drastically limits clients sending a generic or malformed user agent.
(defn reddit-user-agent []
  (format "java:llar:v%s (by /u/%s)"
          (get-in appconfig/appconfig [:version :version])
          (:username (reddit-credentials))))

(defrecord RedditItem
           [meta
            summary
            hash
            entry]
  Object
  (toString [item] (fetch/item-to-string item)))

(defn make-reddit-item [meta summary hash entry]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->RedditItem meta summary hash entry))

(extend-protocol postproc/ItemProcessor
  RedditItem
  (post-process-item [item _src _state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (update item :entry merge (:entry item) nlp)))
  (filter-item [_ _ _] false))

(extend-protocol persistency/CouchItem
  RedditItem
  (to-couch [item]
    (-> item
        (assoc :type :link)
        (dissoc :raw)
        (dissoc :body)
        (assoc-in [:meta :source :args] nil))))

;; Bearer tokens last 24h. The client_credentials grant returns no refresh token,
;; so renewal is just another POST.
(defonce ^:private token-cache (atom nil))
(defonce ^:private token-refresh-lock (Object.))

(def ^:private +token-refresh-margin+ (time/minutes 5))

(defn- request-token! []
  (let [{:keys [app-id secret]} (reddit-credentials)
        user-agent (reddit-user-agent)
        resp (with-http-exception-handler
               {:url +reddit-token-url+
                :user-agent user-agent
                :request ::request-token}
               (http/post +reddit-token-url+
                          (merge
                           (appconfig/http-request-timeouts)
                           {:basic-auth [app-id secret]
                            :form-params {:grant_type "client_credentials"}
                            :accept :json
                            :as :json
                            :headers {:user-agent user-agent}})))
        {:keys [access_token expires_in]} (:body resp)]
    (when (string/blank? access_token)
      (throw+ {:type ::token-request-failed
               :message "reddit access token request returned no access_token"}))
    (log/debugf "reddit: new access token, expires in %ss" expires_in)
    {:token access_token
     :expires-at (time/plus (time/zoned-date-time) (time/seconds (or expires_in 3600)))}))

(defn- token-fresh? [{:keys [expires-at]}]
  (and (some? expires-at)
       (time/after? expires-at (time/plus (time/zoned-date-time) +token-refresh-margin+))))

(defn access-token
  "Cached bearer token for the Reddit Data API. Pass true to force renewal.
  Public so tests can stub it instead of hitting the network."
  ([] (access-token false))
  ([force-refresh?]
   (let [cached @token-cache]
     (if (and (not force-refresh?) (token-fresh? cached))
       (:token cached)
       (locking token-refresh-lock
         ;; Another caller may have renewed while this one waited for the lock.
         ;; Forced refreshes still renew because the cached token was rejected.
         (let [cached @token-cache]
           (if (and (not force-refresh?) (token-fresh? cached))
             (:token cached)
             (:token (reset! token-cache (request-token!))))))))))

(defn- log-rate-limit! [headers]
  (let [remaining (get headers "x-ratelimit-remaining")
        reset (get headers "x-ratelimit-reset")]
    (when (some? remaining)
      (log/debugf "reddit rate limit: used %s, remaining %s, reset in %ss"
                  (get headers "x-ratelimit-used") remaining reset)
      ;; reddit reports remaining as a float, e.g. "999.0"
      (when (some-> remaining parse-double (< 10))
        (log/warnf "reddit rate limit nearly exhausted: %s remaining, resets in %ss"
                   remaining reset)))))

(defn reddit-get
  "GET a Reddit Data API path, e.g. \"/r/clojure/top\". Returns the parsed body."
  [path params]
  (let [url (str +reddit-api-url+ path)
        user-agent (reddit-user-agent)
        get-with (fn [token]
                   (with-http-exception-handler
                     {:url url
                      :user-agent user-agent
                      :request ::reddit-get}
                     (http/get url (merge
                                    (appconfig/http-request-timeouts)
                                    {:query-params params
                                     :accept :json
                                     :as :json
                                     :headers {:user-agent user-agent
                                               :authorization (str "bearer " token)}}))))
        resp (try+
              (get-with (access-token))
              (catch [:type :llar.http/request-error :code 401] _
                (log/info "reddit: access token rejected, renewing and retrying once")
                (get-with (access-token true))))]
    (log-rate-limit! (:headers resp))
    (:body resp)))

(defn listing-path [src]
  (if (= "best" (:listing src))
    ;; /best is the (user-less) front page. It is not subreddit scoped - reddit has
    ;; no /r/<sub>/best - so the source's subreddit cannot be honored here.
    (do (log/warnf "reddit listing :best is not subreddit scoped, ignoring r/%s"
                   (:subreddit src))
        "/best")
    (format "/r/%s/%s" (:subreddit src) (:listing src))))

(defn listing-params [src]
  (cond-> {:limit 100
           ;; without raw_json reddit HTML-escapes & < > in titles and selftext
           :raw_json 1}
    (contains? +reddit-timeframe-listings+ (:listing src)) (assoc :t (:timeframe src))))

;; Scores from the most recent fetch per source, so the dynamic cutoff in
;; llar.fetchutils can be computed without a second request for the same listing.
(defonce last-listing-scores (atom {}))

(defn reddit-ts-to-zoned-date-time [t]
  (when (number? t)
    (time/zoned-date-time (time/instant (* 1000 (long t))) (time/zone-id "UTC"))))

(defn reddit-html-summary [c]
  (str
   (html
    [:h1 (:title c)]
    [:div {:class "summary"}
     [:ul
      (when (some? (:subreddit_name_prefixed c))
        [:li {:class "item-key-subreddit"}
         [:span {:class "key"} "Subreddit: "] (:subreddit_name_prefixed c)])
      [:li {:class "item-key-score"} [:span {:class "key"} "Score: "] (:score c)]
      [:li {:class "item-key-timestamp"}
       [:span {:class "key"} "Time: "] (reddit-ts-to-zoned-date-time (:created_utc c))]
      [:li {:class "item-key-url"} [:a {:href (:url c)} "URL"]]
      [:li {:class "item-key-comments-url"}
       [:a {:href (str "https://www.reddit.com" (:permalink c))} "Comments"]]]]
    [:p {:style "white-space: pre-line"} (:selftext c)])))

(defn make-reddit-entry [c]
  {:url (llar-http/absolutify-url (:url c) "https://www.reddit.com")
   :comments-url (uri/uri (str "https://www.reddit.com" (:permalink c)))
   :thumbnail (:thumbnail c)
   :pub-ts (reddit-ts-to-zoned-date-time (:created_utc c))
   :title (:title c)
   :authors [(:author c)]
   :id (:id c)
   :score (:score c)
   :contents {"text/plain" (:selftext c)
              "text/html" (reddit-html-summary c)}
   :descriptions {"text/plain" (:selftext c)}})

(s/def :irq0-src-reddit/title string?)
(s/def :irq0-src-reddit/created_utc number?)
(s/def :irq0-src-reddit/url :irq0/url-str)
(s/def :irq0-src-reddit/permalink string?)
(s/def :irq0-src-reddit/thumbnail string?)
(s/def :irq0-src-reddit/author string?)
(s/def :irq0-src-reddit/id string?)
(s/def :irq0-src-reddit/score number?)
(s/def :irq0-src-reddit/selftext string?)
(s/def :irq0-src-reddit/item (s/keys :req-un [:irq0-src-reddit/title
                                              :irq0-src-reddit/created_utc
                                              :irq0-src-reddit/url
                                              :irq0-src-reddit/permalink
                                              :irq0-src-reddit/author
                                              :irq0-src-reddit/id
                                              :irq0-src-reddit/score
                                              :irq0-src-reddit/selftext]
                                     :opt-un [:irq0-src-reddit/thumbnail]))

(extend-protocol fetch/FetchSource
  llar.src.Reddit
  (fetch-source [src _conditional-tokens]
    (let [reddit (reddit-get (listing-path src) (listing-params src))
          children (map :data (get-in reddit [:data :children]))]
      (swap! last-listing-scores assoc src (into [] (keep :score) children))
      ;; keep, not for/if: an item failing the spec must be dropped, not turned
      ;; into a nil that flows on into postprocessing and the store
      (keep (fn [item]
              (if (s/invalid? (s/conform :irq0-src-reddit/item item))
                (log/warn "invalid reddit item:"
                          (s/explain-str :irq0-src-reddit/item item))
                (make-reddit-item
                 (fetch/make-meta src)
                 {:ts (reddit-ts-to-zoned-date-time (:created_utc item)) :title (:title item)}
                 (fetch/make-item-hash (str (:id item)))
                 (make-reddit-entry item))))
            children))))
