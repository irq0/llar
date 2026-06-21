(ns llar.metrics
  (:require
   [clojure.string :as string]
   [iapetos.core :as prometheus]
   [iapetos.collector.jvm :as jvm]
   [iapetos.collector.ring :as ring]))

(defonce prom-registry
  (-> (prometheus/collector-registry)
      (jvm/initialize)
      (ring/initialize)
      (prometheus/register
       (prometheus/gauge :llar-sched/last-run
                         {:description "Last time the schedume was run"
                          :labels [:schedule]})
       (prometheus/histogram
        :llar-ui/compile-sources
        {:description "UI step: Time to get configured sources, sources in DB and combine them (:sources)"})
       (prometheus/histogram :llar-ui/active-sources
                             {:description "UI: Time to find and merge in tag/item stats (e.g total, today, $tags like unread) for all sources in current view (:active-sources)"})
       (prometheus/histogram :llar-ui/tag-list
                             {:description "UI: Time to get names of all item tags (:item-tags)"})
       (prometheus/histogram :llar-ui/render-html
                             {:description "UI: Time to render hiccup into HTML"})
       (prometheus/histogram :llar-ui/render-download
                             {:description "UI: Time to render raw download page"})
       (prometheus/histogram :llar-ui/items-current-view
                             {:description "UI: Time to get items with data for current view (:items)"})
       (prometheus/counter :llar/degraded-item-exceptions-total
                           {:description "Item-level processor exceptions that were caught and did not fail the source update."
                            :labels [:source :source_type :step :reason_class :exception_class]}))))

(defn label-value [x]
  (-> (name (or x :unknown))
      (string/replace "-" "_")))

(defn source-type-label [source]
  (if source
    (-> (.getSimpleName (class source))
        (string/replace #"([a-z0-9])([A-Z])" "$1_$2")
        string/lower-case)
    "unknown"))

(defn exception-class-label [throwable]
  (if throwable
    (.getName (class throwable))
    "none"))

(defn http-status-reason-class [code]
  (cond
    (= 429 code) :rate-limited
    (= 408 code) :timeout
    (and (number? code) (<= 400 code 499)) :http-4xx
    (and (number? code) (<= 500 code 599)) :http-5xx))

(defn reason-class-from-data [data]
  (or (:reason-class data)
      (http-status-reason-class (:code data))
      (case (:type data)
        :llar.fetch.feed/rome-failure :parse
        :llar.fetch.readability/not-parsable :parse
        :llar.postproc/postproc-temp-fail :parse
        :llar.update/store-error :store
        :llar.http/unexpected-error :unexpected
        :unknown)))

(defn exception-reason-class [throwable]
  (reason-class-from-data (ex-data throwable)))

(defmacro with-log-exec-time-named [metric & body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elapsed# (- fin# start#)
         elapsed-sec# (/ elapsed# 1000000)]
     (log/debugf "[EXECTIME] %s: %.2fms" '~metric (float elapsed-sec#))
     result#))
