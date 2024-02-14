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
              (prometheus/histogram :llar/compile-sources)
              (prometheus/histogram :llar/download-item-content)
              (prometheus/histogram :llar/active-sources)
              (prometheus/histogram :llar/blobstore-get)
              (prometheus/histogram :llar/tag-list)
              (prometheus/histogram :llar/render-html)
              (prometheus/histogram :llar/render-download)
              (prometheus/histogram :llar/items-current-view))))

(defmacro with-log-exec-time [& body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elapsed# (- fin# start#)
         elapsed-sec# (/ elapsed# 1000000)]
     (log/debugf "[EXECTIME] form %s: %.2fms" (quote ~@body) (float elapsed-sec#))
     result#))

(defmacro with-log-exec-time-named [metric & body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elapsed# (- fin# start#)
         elapsed-sec# (/ elapsed# 1000000)]
     (log/debugf "[EXECTIME] %s: %.2fms" '~metric (float elapsed-sec#))
     result#))

(defmacro with-prom-exec-time [metric & body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elapsed# (- fin# start#)]
     (log/debugf "[EXECTIME] %s: %.2fms" (name ~metric) (float (/ elapsed# 1000000)))
     (prometheus/observe prom-registry ~metric (float (/ elapsed# 1000000000)))
     result#))
