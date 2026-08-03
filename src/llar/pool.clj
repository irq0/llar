(ns llar.pool
  "Named, bounded, observable executors for the fetch pipeline.

  Replaces `clojure.core/pmap`.

  Source updates and item post-processing get SEPARATE pools. One shared pool
  deadlocks: source tasks block waiting on item tasks that can never get a thread.

  The two levels also need opposite failure semantics, hence two map helpers:
  `pmap-isolated` for sources (one bad source must not discard the batch) and
  `pmap-failfast` for items (a postproc temp-fail dooms the whole source anyway)."
  (:require
   [clojure.tools.logging :as log]
   [llar.metrics.resources :as resources]
   [llar.rc :as rc]
   [mount.core :refer [defstate]])
  (:import
   [java.util.concurrent Callable ExecutionException ExecutorService Future
    LinkedBlockingQueue ThreadFactory ThreadPoolExecutor TimeUnit]))

(def ^:private +shutdown-grace-seconds+ 30)

(defrecord Pool [name ^ThreadPoolExecutor executor])

(defn resolve-size
  "Resolve a configured concurrency value. `:auto` follows the CPU count."
  [size]
  (if (= :auto size)
    (.availableProcessors (Runtime/getRuntime))
    size))

(defn- thread-factory [pool-name]
  (let [counter (atom 0)
        prefix (str "llar-" (name pool-name))]
    (reify ThreadFactory
      (newThread [_ runnable]
        (doto (Thread. runnable (format "%s-%d" prefix (swap! counter inc)))
          (.setDaemon true))))))

(defn sample
  "Live saturation of the pool, in the shape `llar.metrics.resources` expects."
  [{:keys [^ThreadPoolExecutor executor]}]
  {:in-use (.getActiveCount executor)
   :limit (.getCorePoolSize executor)
   :queued (.size (.getQueue executor))
   :completed (.getCompletedTaskCount executor)})

(defn make-pool
  "A fixed-size pool named `pool-name`, registered for /metrics until shut down."
  [pool-name size]
  (let [n (resolve-size size)
        executor (ThreadPoolExecutor. n n 0 TimeUnit/MILLISECONDS
                                      (LinkedBlockingQueue.)
                                      (thread-factory pool-name))
        pool (->Pool pool-name executor)]
    (resources/register! resources/resources pool-name :executor #(sample pool))
    (log/debugf "pool %s started with %d threads" pool-name n)
    pool))

(defn resize!
  "Change a running pool's size. Core and max must never cross, so the order of the
  two setters depends on the direction of the change."
  [{:keys [^ThreadPoolExecutor executor]} size]
  (let [n (resolve-size size)]
    (if (> n (.getMaximumPoolSize executor))
      (doto executor (.setMaximumPoolSize n) (.setCorePoolSize n))
      (doto executor (.setCorePoolSize n) (.setMaximumPoolSize n)))
    n))

(defn follow-runtime-config!
  "Keep this pool's size in step with the runtime config at `path`.

  Pools are built during Mount startup, which happens before `.llar` files are loaded, so
  without this the configured size is stale the moment a config file sets it."
  [{:keys [name] :as pool} path]
  (rc/on-change! name #(resize! pool (rc/rc path)))
  pool)

(defn shutdown!
  "Stop accepting work, drain for a grace period, then interrupt what is left.
  Returns true when the pool terminated cleanly."
  [{:keys [name ^ThreadPoolExecutor executor]}]
  (rc/remove-on-change! name)
  (resources/unregister! resources/resources name)
  (.shutdown executor)
  (or (.awaitTermination executor +shutdown-grace-seconds+ TimeUnit/SECONDS)
      (do (log/warnf "pool %s did not drain in %ds, interrupting"
                     name +shutdown-grace-seconds+)
          (.shutdownNow executor)
          false)))

(defn- submit-all
  "Submit every element up front so workers pull independently. `bound-fn*` keeps
  `pmap`'s binding conveyance, which a raw Callable would drop."
  [{:keys [^ExecutorService executor]} f coll]
  (mapv (fn [x]
          (.submit executor ^Callable (bound-fn* (fn [] (f x)))))
        coll))

(defn- unwrap [^ExecutionException e]
  (or (.getCause e) e))

(defn- cancel-all! [futures]
  (run! (fn [^Future fut] (.cancel fut true)) futures))

(defn- abandon!
  "The caller is being torn down mid-wait. Drop the work it will never collect and
  re-arm the interrupt, which `.get` cleared when it threw, so callers up the stack
  still see it."
  [futures ^InterruptedException e]
  (cancel-all! futures)
  (.interrupt (Thread/currentThread))
  (throw e))

(defn call-on
  "Run `f` on `pool` and wait for its value, rethrowing the original cause on failure.

  Bounds a single call the same way the map helpers bound a batch. Callers must not
  already be running on `pool`, or the wait would occupy a slot while needing another."
  [pool f]
  (let [^Future fut (first (submit-all pool (fn [_] (f)) [nil]))]
    (try
      (.get fut)
      (catch ExecutionException e
        (throw (unwrap e))))))

(defn pmap-failfast
  "Map `f` over `coll` on `pool`, returning results in input order.

  On the first failure (in input order) the remaining tasks are cancelled and the
  original cause is rethrown, so slingshot selectors still match. Use for item-level
  work, where one failure already dooms the enclosing source."
  [pool f coll]
  (let [futures (submit-all pool f coll)]
    (try
      (mapv (fn [^Future fut] (.get fut)) futures)
      (catch ExecutionException e
        (cancel-all! futures)
        (throw (unwrap e)))
      (catch InterruptedException e
        (abandon! futures e)))))

(defn pmap-isolated
  "Map `f` over `coll` on `pool`, returning one outcome per element in input order:
  `{:ok? true :value v}` or `{:ok? false :error throwable}`.

  Task failures never propagate - one bad source must not discard everything else's
  results. Caller interruption does propagate: that is the caller going away, not a task
  failing, and reporting it as an outcome left the remaining `.get` calls blocking again."
  [pool f coll]
  (let [futures (submit-all pool f coll)]
    (try
      (mapv (fn [^Future fut]
              (try
                {:ok? true :value (.get fut)}
                (catch ExecutionException e
                  {:ok? false :error (unwrap e)})))
            futures)
      (catch InterruptedException e
        (abandon! futures e)))))

(def ^:private +source-size-path+ [:throttle :source-update-max-concurrent])
(def ^:private +item-size-path+ [:throttle :item-postproc-max-concurrent])

(defstate source-pool
  :start (-> (make-pool :source-update (rc/rc +source-size-path+))
             (follow-runtime-config! +source-size-path+))
  :stop (shutdown! source-pool))

(defstate item-pool
  :start (-> (make-pool :item-postproc (rc/rc +item-size-path+))
             (follow-runtime-config! +item-size-path+))
  :stop (shutdown! item-pool))
