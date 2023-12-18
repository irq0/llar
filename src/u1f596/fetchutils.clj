(ns u1f596.fetchutils
  (:require
   [u1f596.fetch :refer [make-item-hash] :as fetch]
   [u1f596.src :as src]
   [clojure.tools.logging :as log]
   [u1f596.converter :as converter]
   [u1f596.notifier :as notifier]
   [java-time :as time]
   [clojure.set :refer [intersection]]
   [hiccup.core :refer [html]]
   [hickory.select :as S]
   [slingshot.slingshot :refer [try+ throw+]]
   [clj-http.client :as http-client]
   [clj-http.cookies :as http-cookies]
   [hickory.core :as hick]
   [hickory.render :refer [hickory-to-html]]

   [u1f596.postproc :as proc]
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [org.bovinegenius [exploding-fish :as uri]]))

(defn parse-date-to-zoned-data-time [fmt s]
  (time/zoned-date-time
   (time/local-date (time/formatter fmt) s) 0 (time/zone-id "UTC")))

(defn parse-date-time-to-zoned-data-time [fmt s]
  (time/zoned-date-time
   (time/local-date-time (time/formatter fmt) s) (time/zone-id "UTC")))

(def +mercury-site-blacklist+
  #"www\.washingtonpost\.com|semiaccurate\.com|gitlab\.com|youtube|vimeo|reddit|redd\.it|open\.spotify\.com|news\.ycombinator\.com|www\.amazon\.com")

(defn- replace-contents-with-mercury [item keep-orig?]
  (let [url (get-in item [:entry :url])
        src (src/mercury (str url))
        mercu (proc/process-feedless-item src (first (fetch/fetch-source src)))
        html (if keep-orig?
               (str "<div class=\"orig-content\">" (get-in item [:entry :contents "text/html"]) "</div>"
                    "<div class=\"mercury\">" (get-in mercu [:entry :contents "text/html"]) "</div>")
               (get-in mercu [:entry :contents "text/html"]))
        text (if keep-orig?
               (str (get-in item [:entry :contents "text/plain"])
                    "\n"
                    (get-in mercu [:entry :contents "text/plain"]))
               (get-in mercu [:entry :contents "text/plain"]))]
    (cond-> (-> item
                (assoc-in [:entry :nlp] (get-in mercu [:entry :nlp]))
                (assoc-in [:entry :descriptions "text/plain"] (get-in mercu [:entry :descriptions "text/plain"]))
                (assoc-in [:entry :contents "text/plain"] text)
                (assoc-in [:entry :lead-image-url] (get-in mercu [:entry :lead-image-url]))
                (assoc-in [:entry :contents "text/html"] html))
      (empty? (get-in item [:entry :authors]))
      (assoc-in [:entry :authors] (get-in mercu [:entry :authors])))))

(defn mercury-contents
  [& {:keys [keep-orig?]
      :or {keep-orig? false}}]
  (fn [item]
    (let [site (some-> item :entry :url uri/host)
          path (or (some-> item :entry :url uri/path) "")]
      (cond
        ;; images
        (or (re-find #"i\.imgur\.com|i\.redd\.it|twimg\.com" site)
            (re-find #"\.(jpg|jpeg|gif|png|pdf)$" path))
        (update-in item [:entry :contents "text/html"]
                   str "<img src=\"" (get-in item [:entry :url]) "\"/>")

        ;; blacklisted sites
        (re-find +mercury-site-blacklist+ site)
        item

        ;; rest: replace with mercury
        :else
        (try+
         (replace-contents-with-mercury item keep-orig?)
         (catch [:type :u1f596.fetch.mercury/not-parsable] _
           (log/error (str item) "Mercury Error. Not replacing content with mercury")
           item))))))

(defn make-hacker-news-filter [min-score min-score-match]
  (fn [item]
    (let [site (some-> item :entry :url uri/host)
          score (get-in item [:entry :score])
          title (some-> (get-in item [:summary :title]) string/lower-case)
          type (get-in item [:entry :type])]
      (not
       (or
        (and (= :story type)
             (>= score min-score))
        (and (= :story type)
             (re-find #"clojure|lisp|book|alan kay|futurism|rob pike|germany|file system|quobyte|storage" title)
             (>= score min-score-match))
        (and
         (some? site)
         (re-find #"nautil\.us|theatlantic|medium|youtube|theguardian|washingtonpost|99percentinvisible|theverge|phys\.org|bbc\.com"
                  site)
         (>= score min-score-match)))))))

(defn make-category-filter-deny [blacklist]
  (fn [item]
    (let [categories (set (get-in item [:entry :categories]))]
      (>= (count (intersection categories (set blacklist))) 1))))

(defonce reddit-scores (atom {}))

(defn- update-reddit-cutoff-score! [k src]
  (let [s (u1f596.fetch.reddit/reddit-get-scores src)
        next {:top-n-score (nth (sort s) (- (count s) (num (* (count s) 0.05))))
              :update-ts (time/zoned-date-time)}]
    (swap! reddit-scores assoc k next)
    next))

(defn get-reddit-cutoff-score [k src]
  (if-let [{:keys [top-n-score update-ts]} (get @reddit-scores k)]
    (if (time/before? update-ts (time/minus (time/zoned-date-time) (time/hours 12)))
      (:top-n-score (update-reddit-cutoff-score! k src))
      top-n-score)
    (:top-n-score (update-reddit-cutoff-score! k src))))

(defn make-reddit-proc [k src options]
  (let [{:keys [min-score dynamic?]
         :or {min-score 0
              dynamic? false}} options]
    (proc/make
     :filter (fn [item]
               (let [dynamic-min-score (if dynamic? (get-reddit-cutoff-score k src) min-score)
                     min-score (max dynamic-min-score min-score)
                     score (get-in item [:entry :score])]
                 (<= score dynamic-min-score min-score)))
     :post [(fn [item]
              (let [site (some-> item :entry :url uri/host)
                    path (some-> item :entry :url uri/path)]
                (cond
                  (or (re-find #"i\.imgur\.com|i\.redd\.it|twimg\.com" site)
                      (re-find #"\.(jpg|jpeg|gif|png)$" path))
                  (update-in item [:entry :contents "text/html"]
                             str "<img src=\"" (get-in item [:entry :url]) "\"/>")
                  (re-find #"youtube|vimeo|reddit|redd\.it|open\.spotify\.com" site) item
                  :else ((mercury-contents :keep-orig? true) item))))])))
