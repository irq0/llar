(ns llar.shutdown
  "Order application and logging shutdown from one JVM hook."
  (:require
   [clojure.tools.logging :as log]
   [llar.work :as work]
   [mount.core :as mount])
  (:import
   [java.io FileDescriptor FileOutputStream]
   [java.nio.charset StandardCharsets]
   [org.apache.logging.log4j LogManager]))

(defonce ^:private shutdown-started? (atom false))
(defonce ^:private direct-stdout (FileOutputStream. FileDescriptor/out))

(defn- elapsed-ms [started-nanos]
  (quot (- (System/nanoTime) started-nanos) 1000000))

(defn- work-snapshot []
  (->> (work/in-flight)
       (mapv #(select-keys % [:id :kind :source :stage :waiting-on :thread]))))

(defn- error-text [error]
  (str (.getName (class error)) ": " (or (ex-message error) "")))

(defn- shutdown-message [status elapsed states remaining-work error]
  (str "LLAR shutdown " (name status) " in " elapsed "ms: "
       "remaining-states=" (count states)
       " remaining-work=" (count remaining-work)
       (when (seq states)
         (str " states=" (pr-str states)))
       (when (seq remaining-work)
         (str " work=" (pr-str remaining-work)))
       (when error
         (str " error=" (error-text error)))))

(defn- stdout! [text]
  ;; Bypass a replaced or closed System.out; journald reads file descriptor 1.
  (locking direct-stdout
    (.write direct-stdout (.getBytes (str text "\n") StandardCharsets/UTF_8))
    (.flush direct-stdout)))

(defn- emit-log! [status error text]
  (case status
    :finished (log/info text)
    :incomplete (log/warn text)
    :failed (log/error error text)))

(defn- report! [status error text]
  (try
    (emit-log! status error text)
    (catch Throwable logging-error
      (try
        (stdout! (str text " logging-error=" (error-text logging-error)))
        (catch Throwable _ nil)))))

(defn- stop-logging! []
  (try
    ;; Synchronous: flushes appenders before returning.
    (LogManager/shutdown)
    (catch Throwable logging-error
      (try
        (stdout! (str "LLAR logging shutdown failed: " (error-text logging-error)))
        (catch Throwable _ nil)))))

(defn- run-shutdown! []
  (let [started-nanos (System/nanoTime)]
    (try
      ;; A broken logger must never prevent resource teardown.
      (try
        (log/info "shutting down...")
        (catch Throwable _ nil))
      (let [stop-error (try
                         (mount/stop)
                         nil
                         (catch Throwable error
                           error))
            states (-> (mount/running-states) sort vec)
            remaining-work (work-snapshot)
            status (cond
                     stop-error :failed
                     (or (seq states) (seq remaining-work)) :incomplete
                     :else :finished)]
        (report! status stop-error
                 (shutdown-message status (elapsed-ms started-nanos)
                                   states remaining-work stop-error)))
      (catch Throwable error
        (report! :failed error
                 (str "LLAR shutdown failed after " (elapsed-ms started-nanos)
                      "ms before teardown could be verified: "
                      (error-text error))))
      (finally
        (stop-logging!)))))

(defn shutdown! []
  (when (compare-and-set! shutdown-started? false true)
    (run-shutdown!)))

(defonce ^:private shutdown-hook
  (delay
    (let [hook (Thread. ^Runnable shutdown! "llar-shutdown")]
      (.addShutdownHook (Runtime/getRuntime) hook)
      hook)))

(defn install-hook! []
  (force shutdown-hook))
