(ns llar.throttle
  "Semaphores that report.

  The bare `Semaphore`s these replace were invisible: with `streaming` holding a single
  permit and `av-downloader` two, a source waiting on one looked exactly like a source
  that was merely slow. Now each throttle publishes its permits, its waiters and how
  long acquiring took.

  Wait time is the one measure that cannot be sampled at scrape time - a snapshot of a
  semaphore says nothing about how long anyone waited - so it is observed here, at
  acquire."
  (:require
   [clojure.tools.logging :as log]
   [iapetos.core :as prometheus]
   [llar.metrics :as metrics]
   [llar.metrics.resources :as resources])
  (:import
   [java.util.concurrent Semaphore]))

(defonce prom-registry
  (-> metrics/prom-registry
      (prometheus/register
       (prometheus/histogram :llar-resource/wait-seconds
                             {:description "Time spent waiting to acquire a bounded resource."
                              :labels [:resource]
                              :buckets [0.001 0.01 0.1 0.5 1 5 30 60 300 1800]}))))

(defrecord Throttle [name ^Semaphore semaphore limit])

(defn sample
  "Live saturation, in the shape `llar.metrics.resources` expects.

  `:queued` comes from `Semaphore.getQueueLength`, which the JDK documents as an
  estimate intended for exactly this kind of monitoring."
  [{:keys [^Semaphore semaphore limit]}]
  {:in-use (- limit (.availablePermits semaphore))
   :limit limit
   :queued (.getQueueLength semaphore)})

(defn make-throttle
  "A throttle of `limit` permits, registered for /metrics until shut down."
  [throttle-name limit]
  (let [throttle (->Throttle throttle-name (Semaphore. limit) limit)]
    (resources/register! resources/resources throttle-name :throttle #(sample throttle))
    (log/debugf "throttle %s created with %d permits" throttle-name limit)
    throttle))

(defn shutdown! [{:keys [name]}]
  (resources/unregister! resources/resources name)
  true)

(defn observe-wait!
  "Record how long an acquire took. Public only so `with-throttle` can expand into it."
  [throttle-name elapsed-nanos]
  (prometheus/observe prom-registry :llar-resource/wait-seconds
                      {:resource (metrics/label-value throttle-name)}
                      (/ elapsed-nanos 1e9)))

(defmacro with-throttle
  "Run `body` holding one of `throttle`'s permits, timing the acquire."
  [throttle & body]
  `(let [throttle# ~throttle
         ^Semaphore sem# (:semaphore throttle#)
         start# (System/nanoTime)]
     (.acquire sem#)
     (observe-wait! (:name throttle#) (- (System/nanoTime) start#))
     (try
       ~@body
       (finally
         (.release sem#)))))
