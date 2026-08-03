(ns llar.work
  "Registry of work currently in flight.

  Per-source-per-item detail is exactly the cardinality Prometheus is built to reject, so
  the detail lives in an atom the dashboard reads directly and only aggregates are
  exported. That split is deliberate: `llar_work_oldest_age_seconds` is the alertable
  signal, while \"which item of which source is stuck, and on what\" is a live question
  best answered by looking.

  `*work-id*` is dynamic so a unit can self-report what it is blocked on without
  threading an id through every call. The pools dispatch through `bound-fn*`, so an item
  task inherits its source's id until it opens its own `with-work`."
  (:require
   [iapetos.registry :as registry]
   [java-time.api :as time]
   [llar.metrics :as metrics]
   [llar.metrics.collector :as collector])
  (:import
   [java.util.concurrent.atomic AtomicLong]))

(defonce work
  (atom {}))

(def ^:private next-id (AtomicLong. 0))

(def ^:dynamic *work-id* nil)

(defn reset-work! []
  (reset! work {}))

(defn register!
  "Add a unit of work. `attrs` carries at least :kind, usually :source and :stage."
  [attrs]
  (let [id (.incrementAndGet next-id)]
    (swap! work assoc id
           (merge {:waiting-on nil}
                  attrs
                  {:id id
                   :thread (.getName (Thread/currentThread))
                   :started-at (time/zoned-date-time)}))
    id))

(defn unregister! [id]
  (swap! work dissoc id)
  nil)

(defn- update-current! [f & args]
  (when-let [id *work-id*]
    (swap! work (fn [w] (if (contains? w id)
                          (apply update w id f args)
                          w))))
  nil)

(defn stage!
  "Advance the current unit's pipeline stage, e.g. :fetch -> :postproc -> :store."
  [stage]
  (update-current! assoc :stage stage))

(defn waiting-on!
  "Record what the current unit is blocked on, or nil once it is running again.
  This is what distinguishes \"slow\" from \"queued behind a permit\"."
  [reason]
  (update-current! assoc :waiting-on reason))

(defmacro with-work
  "Track `body` as one unit of in-flight work, cleaning up even when it throws."
  [attrs & body]
  `(let [id# (register! ~attrs)]
     (binding [*work-id* id#]
       (try
         ~@body
         (finally
           (unregister! id#))))))

(defn- age-seconds [now started-at]
  (/ (.toMillis (time/duration started-at now)) 1000.0))

(defn in-flight
  "Work currently in flight, oldest first, each with :age-seconds."
  []
  (let [now (time/zoned-date-time)]
    (->> (vals @work)
         (map (fn [entry] (assoc entry :age-seconds (age-seconds now (:started-at entry)))))
         (sort-by :age-seconds >)
         vec)))

;;; Aggregate export. Deliberately no :source label - see the namespace docstring.

(def ^:private +in-flight-metric+ "llar_work_in_flight")
(def ^:private +oldest-metric+ "llar_work_oldest_age_seconds")

(defn- in-flight-family [entries]
  (collector/family
   +in-flight-metric+ collector/gauge
   "Units of work currently in flight."
   (map (fn [[[kind stage] units]]
          (collector/sample +in-flight-metric+ ["kind" "stage"]
                            [(metrics/label-value kind) (metrics/label-value stage)]
                            (count units)))
        (group-by (juxt :kind :stage) entries))))

(defn- oldest-family [entries]
  (collector/family
   +oldest-metric+ collector/gauge
   "Age of the oldest in-flight unit of work, by kind."
   (map (fn [[kind units]]
          (collector/sample +oldest-metric+ ["kind"]
                            [(metrics/label-value kind)]
                            (apply max (map :age-seconds units))))
        (group-by :kind entries))))

(defonce collector
  (doto (collector/sampling-collector
         (fn []
           (let [entries (in-flight)]
             [(in-flight-family entries)
              (oldest-family entries)])))
    (.register (registry/raw metrics/prom-registry))))
