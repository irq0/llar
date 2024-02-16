(ns llar.fetchutils
  (:require
   [clojure.set :refer [intersection]]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]
   [java-time.api :as time]
   [java-time.format :as time-format]
   [org.bovinegenius [exploding-fish :as uri]]
   [slingshot.slingshot :refer [try+ throw+]]
   [hickory.core :as hick]
   [hickory.render :as hick-r]
   [llar.fetch :as fetch]
   [llar.fetch.reddit :as reddit]
   [llar.http :as http]
   [llar.postproc :as proc]
   [llar.src :as src]))

(defmacro with-datetime-exception-handler [context & body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo ex#
       (let [java-ex# (ex-cause ex#)
             arguments# (:arguments (ex-data ex#))]
         (cond
           (instance? java.time.format.DateTimeParseException java-ex#)
           (do (log/debugf "Unparsable timestamp: %s" (ex-message java-ex#))
               (throw+ (merge {:type :datetime-parse-exception
                               :arguments arguments#}
                              ~context)
                       nil
                       (ex-message java-ex#)))
           (and (instance? java.time.DateTimeException java-ex#)
                (re-find #"LocalDateTime from TemporalAccessor" (ex-message java-ex#)))
           (throw+ (merge {:type :datetime-unable-to-find-time
                           :arguments arguments#}

                          ~context)
                   nil
                   (ex-message java-ex#))
           (and (instance? java.time.DateTimeException java-ex#)
                (re-find #"ZonedDateTime from TemporalAccessor" (ex-message java-ex#)))
           (throw+ (merge {:type :datetime-no-timezone
                           :arguments arguments#}
                          ~context)
                   nil
                   (ex-message java-ex#))
           (instance? java.time.DateTimeException java-ex#)
           (throw+ (merge {:type :datetime-exception
                           :arguments arguments#}
                          ~context)
                   nil
                   (ex-message java-ex#)))))))

(defn- parse-datetime-with-timezone [fmt s]
  (with-datetime-exception-handler {:format fmt :string s}
    (time/zoned-date-time (time/formatter fmt) s)))

(defn- parse-datetime-force-utc [fmt s]
  (with-datetime-exception-handler {:format fmt :string s}
    (time/zoned-date-time
     (time/local-date-time (time/formatter fmt) s) 0 (time/zone-id "UTC"))))

(defn- parse-date-force-utc [fmt s]
  (with-datetime-exception-handler {:format fmt :string s}
    (time/zoned-date-time
     (time/local-date (time/formatter fmt) s) 0 (time/zone-id "UTC"))))

(def predefined-timestamp-formats (->>
                                   time-format/predefined-formatters
                                   keys
                                   (map keyword)
                                   (into #{})))

(s/def :irq0/predefined-timestamp-format #(contains? predefined-timestamp-formats %))

(defn parse-timestamp [fmt s]
  {:pre [(s/valid? (s/or :format-string (s/and string?)
                         :predefined (s/and keyword? :irq0/predefined-timestamp-format))
                   fmt)
         (s/valid? string? s)]}
  (try+
   (parse-datetime-with-timezone fmt s)
   (catch [:type :datetime-no-timezone] _
     (try+
      (parse-datetime-force-utc fmt s)
      (catch [:type :datetime-unable-to-find-time] _
        (parse-date-force-utc fmt s))))
   (catch [:type :datetime-unable-to-find-time] _
     (parse-date-force-utc fmt s))))

(defn process-html-contents [base-url contents]
  (if-let [html (get-in contents ["text/html"])]
    (assoc contents "text/html"
           (-> html
               hick/parse
               hick/as-hickory
               (http/absolutify-links-in-hick base-url)
               http/sanitize
               http/blobify
               (hick-r/hickory-to-html)))
    contents))

(def +mercury-site-denylist+
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

        ;; denylisted sites
        (re-find +mercury-site-denylist+ site)
        item

        ;; rest: replace with mercury
        :else
        (try+
         (replace-contents-with-mercury item keep-orig?)
         (catch [:type :llar.fetch.mercury/not-parsable] _
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

(defn make-category-filter-deny [denylist]
  (fn [item]
    (let [categories (set (get-in item [:entry :categories]))]
      (>= (count (intersection categories (set denylist))) 1))))

(defonce reddit-scores (atom {}))

(defn- update-reddit-cutoff-score! [k src]
  (let [s (reddit/reddit-get-scores src)
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

(defn add-tag [tag]
  (fn [item]
    (update-in item [:meta :tags] conj tag)))

(defn add-tag-filter [tag fltr]
  (fn [item]
    (if (fltr item)
      (update-in item [:meta :tags] conj tag)
      item)))

(defn exchange [src dst]
  (fn [item]
    (let [src-val (get-in item src)
          dst-val (get-in item dst)]
      (-> item
          (assoc-in dst src-val)
          (assoc-in src dst-val)))))

(defn html-to-hickory [raw-html]
  (some-> raw-html
          hick/parse
          hick/as-hickory))

(defn hickory-sanitize-blobify [h]
  (some-> h
          (http/sanitize :remove-css? true)
          (http/blobify)))
