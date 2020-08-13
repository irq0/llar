(ns infowarss.metrics
  (:require
   [iapetos.core :as prometheus]
   [mount.core :refer [defstate]]))

(defstate prom-registry
  :start (-> (prometheus/collector-registry)
             (prometheus/register
              (prometheus/histogram :compile-sources)
              (prometheus/histogram :download-item-content)
              (prometheus/histogram :active-sources)
              (prometheus/histogram :blobstore-get)
              (prometheus/histogram :tag-list)
              (prometheus/histogram :render-html)
              (prometheus/histogram :render-download)
              (prometheus/histogram :items-current-view))))

(defmacro with-log-exec-time [& body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elasped# (- fin# start#)
         elasped-sec# (/ elasped# 1000000)]
     (log/debugf "%s: %.2fms" (quote ~@body) (float elasped-sec#))
     result#))

(defmacro with-prom-exec-time [metric & body]
  `(let [start# (java.lang.System/nanoTime)
         result# (do ~@body)
         fin# (java.lang.System/nanoTime)
         elasped# (- fin# start#)]
     (log/debugf "[EXECTIME] %s: %.2fms" (name ~metric) (float (/ elasped# 1000000)))
     (prometheus/observe prom-registry ~metric (float (/ elasped# 1000000000)))
     result#))
