(ns llar.metrics
  (:require
   [mount.core :refer [defstate]]
   [iapetos.core :as prometheus]
   [iapetos.collector.jvm :as jvm]
   [iapetos.collector.ring :as ring]))

(defstate prom-registry
  :start (-> (prometheus/collector-registry)
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
                                    {:description "UI: Time to get items with data for current view (:items)"})))
  :stop (prometheus/clear prom-registry))

(defmacro with-log-exec-time-named [metric & body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elapsed# (- fin# start#)
         elapsed-sec# (/ elapsed# 1000000)]
     (log/debugf "[EXECTIME] %s: %.2fms" '~metric (float elapsed-sec#))
     result#))
