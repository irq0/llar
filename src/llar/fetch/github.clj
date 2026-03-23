(ns llar.fetch.github
  (:require
   [llar.fetch :as fetch]
   [llar.specs]
   [llar.src]
   [llar.item]
   [llar.persistency :as persistency]
   [llar.postproc :as postproc]
   [llar.analysis :as analysis]
   [llar.http :as llar-http :refer [with-http-exception-handler]]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [hiccup2.core :refer [html]]
   [clj-http.client :as http]
   [java-time.api :as time]))

;; GitHub Search API
;; https://docs.github.com/en/rest/search/search

(def +github-search-issues-url+ "https://api.github.com/search/issues")
(def +github-search-repos-url+ "https://api.github.com/search/repositories")
(def +github-user-agent+ "java:llar:23 (https://github.com/irq0/llar)")

;;;; Date token expansion

(defn- utc-today [] (time/local-date (time/zone-id "UTC")))

(def +date-tokens+
  {"{{today}}"        #(time/format "yyyy-MM-dd" (utc-today))
   "{{yesterday}}"    #(time/format "yyyy-MM-dd" (time/minus (utc-today) (time/days 1)))
   "{{last-week}}"    #(time/format "yyyy-MM-dd" (time/minus (utc-today) (time/days 7)))
   "{{last-2-weeks}}" #(time/format "yyyy-MM-dd" (time/minus (utc-today) (time/days 14)))
   "{{last-month}}"   #(time/format "yyyy-MM-dd" (time/minus (utc-today) (time/days 30)))})

(defn expand-date-tokens [query-str]
  (reduce-kv (fn [q token resolver]
               (string/replace q token (resolver)))
             query-str
             +date-tokens+))

;;;; API

(defn- github-search-api [search-type query args]
  (let [url (case search-type
              :issues +github-search-issues-url+
              :repos  +github-search-repos-url+)
        expanded-query (expand-date-tokens query)
        params (cond-> {"q" expanded-query
                        "per_page" (or (:per-page args) 30)}
                 (:sort args)  (assoc "sort" (name (:sort args)))
                 (:order args) (assoc "order" (name (:order args))))]
    (log/debugf "GitHub search (%s): %s params=%s" search-type expanded-query params)
    (with-http-exception-handler
      {:url url :user-agent +github-user-agent+ :request ::github-search}
      (let [resp (http/get url
                           {:query-params params
                            :accept :json
                            :as :json
                            :headers {"User-Agent" +github-user-agent+
                                      "Accept" "application/vnd.github.v3+json"}})]
        (log/infof "GitHub search returned %d items (total: %d)"
                   (count (get-in resp [:body :items]))
                   (get-in resp [:body :total_count]))
        (get-in resp [:body :items])))))

;;;; Item record

(defrecord GitHubSearchItem
           [meta
            summary
            hash
            entry]
  Object
  (toString [item] (fetch/item-to-string item)))

(defn make-github-item [meta summary hash entry]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->GitHubSearchItem meta summary hash entry))

;;;; Protocol extensions

(extend-protocol postproc/ItemProcessor
  GitHubSearchItem
  (post-process-item [item _src _state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (update item :entry merge (:entry item) nlp)))
  (filter-item [_ _ _] false))

(extend-protocol persistency/CouchItem
  GitHubSearchItem
  (to-couch [item]
    (-> item
        (assoc :type :link)
        (dissoc :raw)
        (assoc-in [:meta :source :args] nil))))

;;;; HTML summary generators

(defn- format-duration-between
  "Human-readable duration between two ISO timestamps, e.g. '3d 5h' or '2h 15m'"
  [from-str to-str]
  (when (and from-str to-str)
    (let [from (time/instant from-str)
          to (time/instant to-str)
          dur (time/duration from to)
          days (.toDays dur)
          hours (mod (.toHours dur) 24)
          minutes (mod (.toMinutes dur) 60)]
      (cond
        (>= days 1) (str days "d " hours "h")
        (>= hours 1) (str hours "h " minutes "m")
        :else (str minutes "m")))))

(defn- repo-from-url
  "Extract 'owner/repo' from a GitHub html_url"
  [html-url]
  (when html-url
    (let [parts (string/split html-url #"/")]
      (when (>= (count parts) 5)
        (str (nth parts 3) "/" (nth parts 4))))))

(defn- issue-html-summary [hit]
  (let [{:keys [html_url user labels state pull_request
                comments reactions body draft
                created_at closed_at assignees]} hit
        is-pr? (some? pull_request)
        merged? (and is-pr? (some? (:merged_at pull_request)))
        merged-at (:merged_at pull_request)
        diff-url (:diff_url pull_request)
        resolved-at (or merged-at closed_at)
        open-duration (format-duration-between created_at resolved-at)
        repo (repo-from-url html_url)]
    (str
     (html
      [:div {:class "summary"}
       [:p (when repo [:code repo])
        (when repo " ")
        [:strong (:login user)]
        " · " (if is-pr? "PR" "Issue")
        " · " (cond draft "draft" merged? "merged" :else state)
        (when resolved-at
          (str " · " (subs resolved-at 0 10)))
        (when open-duration
          (str " (open " open-duration ")"))]
       (when (seq assignees)
         [:p "Assignees: "
          (interpose ", " (map #(vector :strong (:login %)) assignees))])
       (when (seq labels)
         [:p (interpose " " (map #(vector :code (:name %)) labels))])
       (when-not (string/blank? body)
         [:pre body])
       [:p (str comments " comments")
        (when reactions (str " · " (:total_count reactions) " reactions"))
        (when diff-url (list " · " [:a {:href diff-url} "Diff"]))]]))))

(defn- repo-html-summary [hit]
  (let [{:keys [description stargazers_count
                forks_count language topics homepage]} hit
        license-name (get-in hit [:license :spdx_id])]
    (str
     (html
      [:div {:class "summary"}
       (when-not (string/blank? description)
         [:p description])
       [:p (str stargazers_count " stars · " forks_count " forks")
        (when language (str " · " language))
        (when (and license-name (not= license-name "NOASSERTION"))
          (str " · " license-name))]
       (when (seq topics)
         [:p (interpose " " (map #(vector :code %) topics))])
       (when-not (string/blank? homepage)
         [:p [:a {:href homepage} "Homepage"]])]))))

;;;; Timestamp parsing

(defn- parse-github-timestamp
  "Parse GitHub API timestamp (ISO 8601 with Z suffix) to ZonedDateTime"
  [s]
  (when s
    (time/zoned-date-time (time/instant s) (time/zone-id "UTC"))))

;;;; Entry builders

(defn- make-issue-entry [hit]
  (let [{:keys [html_url title user body created_at number
                labels state pull_request comments reactions]} hit]
    {:url (llar-http/absolutify-url html_url)
     :title (str "#" number " " title)
     :authors [(:login user)]
     :pub-ts (parse-github-timestamp created_at)
     :id (str (:id hit))
     :score (or (:total_count reactions) 0)
     :labels (mapv :name labels)
     :state state
     :is-pr (some? pull_request)
     :merged (and (some? pull_request) (some? (:merged_at pull_request)))
     :num-comments comments
     :contents {"text/plain" (or body "")
                "text/html" (issue-html-summary hit)}}))

(defn- make-repo-entry [hit]
  (let [{:keys [html_url full_name description stargazers_count
                forks_count language topics owner created_at]} hit]
    {:url (llar-http/absolutify-url html_url)
     :title full_name
     :authors [(:login owner)]
     :pub-ts (parse-github-timestamp created_at)
     :id (str (:id hit))
     :score stargazers_count
     :language language
     :topics (or topics [])
     :num-forks forks_count
     :contents {"text/plain" (or description "")
                "text/html" (repo-html-summary hit)}}))

;;;; FetchSource

(extend-protocol fetch/FetchSource
  llar.src.GitHubSearch
  (fetch-source [src _conditional-tokens]
    (let [{:keys [search-type query args]} src
          hits (github-search-api search-type query args)
          make-entry (case search-type
                       :issues make-issue-entry
                       :repos  make-repo-entry)]
      (for [hit hits
            :let [entry (make-entry hit)]]
        (make-github-item
         (fetch/make-meta src)
         {:ts (or (:pub-ts entry) (time/zoned-date-time))
          :title (:title entry)}
         (fetch/make-item-hash (str "github-" (name search-type) "-" (:id entry)))
         entry)))))
