(ns llar.metrics
  (:require
   [iapetos.core :as prometheus]
   [iapetos.collector.jvm :as jvm]
   [iapetos.collector.ring :as ring]
   [mount.core :refer [defstate]]))

(defstate prom-registry
  :start (-> (prometheus/collector-registry)
             (jvm/initialize)
             (ring/initialize)
             (prometheus/register
              (prometheus/gauge :llar/update-duration-millis
                                {:description "Time it took to fetch and process a source"
                                 :labels [:source]})
              (prometheus/histogram :llar-ui/compile-sources)
              (prometheus/histogram :llar-ui/active-sources)
              (prometheus/histogram :llar-ui/tag-list)
              (prometheus/histogram :llar-ui/render-html)
              (prometheus/histogram :llar-ui/render-download)
              (prometheus/histogram :llar-ui/items-current-view))))

(defmacro with-log-exec-time-named [metric & body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elapsed# (- fin# start#)
         elapsed-sec# (/ elapsed# 1000000)]
     (log/debugf "[EXECTIME] %s: %.2fms" '~metric (float elapsed-sec#))
     result#))
