(ns llar.throttle-test
  (:require
   [clojure.test :refer [deftest is]]
   [iapetos.registry :as registry]
   [llar.metrics :as metrics]
   [llar.metrics.resources :as resources]
   [llar.rc :as rc]
   [llar.throttle :as uut]
   [llar.work :as work])
  (:import
   [java.util.concurrent CountDownLatch TimeUnit]))

(defn- wait-observations [resource]
  (.getSampleValue (registry/raw metrics/prom-registry)
                   "llar_resource_wait_seconds_count"
                   (into-array String ["resource"])
                   (into-array String [resource])))

(defn- wait-until
  "Poll pred for up to ~2s. Returns its last value, so callers can assert on it."
  [pred]
  (loop [n 0]
    (let [v (pred)]
      (if (or v (> n 100))
        v
        (do (Thread/sleep 20) (recur (inc n)))))))

(deftest throttle-reports-its-saturation
  (let [t (uut/make-throttle :sat-test 3)]
    (try
      (is (= {:in-use 0 :limit 3 :queued 0} (uut/sample t)))
      (uut/with-throttle t
        (is (= 1 (:in-use (uut/sample t)))))
      (is (= 0 (:in-use (uut/sample t))))
      (finally
        (uut/shutdown! t)))))

(deftest permits-are-released-even-when-the-body-throws
  (let [t (uut/make-throttle :throw-test 1)]
    (try
      (is (thrown? clojure.lang.ExceptionInfo
                   (uut/with-throttle t (throw (ex-info "boom" {})))))
      (is (= 0 (:in-use (uut/sample t))) "a throwing body must not leak a permit")
      (finally
        (uut/shutdown! t)))))

(deftest with-throttle-returns-the-body-value
  (let [t (uut/make-throttle :value-test 1)]
    (try
      (is (= :the-value (uut/with-throttle t :the-value)))
      (finally
        (uut/shutdown! t)))))

(deftest throttle-caps-concurrency-at-its-limit
  (let [t (uut/make-throttle :cap-test 2)
        in-flight (atom 0)
        peak (atom 0)
        futs (doall (for [_ (range 20)]
                      (future (uut/with-throttle t
                                (swap! peak max (swap! in-flight inc))
                                (Thread/sleep 5)
                                (swap! in-flight dec)))))]
    (try
      (run! deref futs)
      (is (<= @peak 2))
      (finally
        (uut/shutdown! t)))))

(deftest queued-waiters-are-visible
  ;; the whole point: "slow" and "queued behind a permit" must look different
  (let [t (uut/make-throttle :queued-test 1)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        holder (future (uut/with-throttle t
                         (.countDown entered)
                         (.await release 30 TimeUnit/SECONDS)))]
    (try
      (is (.await entered 30 TimeUnit/SECONDS))
      (let [waiter (future (uut/with-throttle t :got-it))]
        (is (wait-until #(= 1 (:queued (uut/sample t))))
            "a blocked acquire must show up as queued")
        (is (= 1 (:in-use (uut/sample t))))
        (.countDown release)
        (is (= :got-it (deref waiter 30000 :timed-out)))
        (is (wait-until #(zero? (:queued (uut/sample t))))))
      (finally
        (.countDown release)
        (deref holder 30000 nil)
        (uut/shutdown! t)))))

(deftest throttles-register-and-unregister-with-the-metrics-registry
  (let [t (uut/make-throttle :registry-test 2)]
    (is (contains? @resources/resources :registry-test))
    (is (= {:in-use 0 :limit 2 :queued 0}
           ((:sample-fn (get @resources/resources :registry-test)))))
    (uut/shutdown! t)
    (is (not (contains? @resources/resources :registry-test)))))

(deftest every-acquire-is-timed
  ;; wait time is the one measure that cannot be sampled from a snapshot, so it has to
  ;; be observed at acquire - including the uncontended case, which should be ~0
  (let [t (uut/make-throttle :timed-test 2)]
    (try
      (is (nil? (wait-observations "timed_test")) "no observations before any acquire")
      (dotimes [_ 3]
        (uut/with-throttle t :ok))
      (is (= 3.0 (wait-observations "timed_test")))
      (finally
        (uut/shutdown! t)))))

(deftest a-blocked-acquire-reports-what-it-is-waiting-on
  ;; the payoff of the whole design: "slow" and "queued behind a permit" must be
  ;; distinguishable from the outside
  (work/reset-work!)
  (let [t (uut/make-throttle :waiting-test 1)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        holder (future (uut/with-throttle t
                         (.countDown entered)
                         (.await release 30 TimeUnit/SECONDS)))]
    (try
      (is (.await entered 30 TimeUnit/SECONDS))
      (let [waiter (future (work/with-work {:kind :item :source :s :stage :postproc}
                             (uut/with-throttle t :done)))]
        (is (wait-until #(= :waiting-test (:waiting-on (first (work/in-flight)))))
            "a unit blocked on a permit must say so")
        (.countDown release)
        (is (= :done (deref waiter 30000 :timed-out)))
        (is (wait-until #(empty? (work/in-flight)))))
      (finally
        (.countDown release)
        (deref holder 30000 nil)
        (uut/shutdown! t)
        (work/reset-work!)))))

(deftest throttle-resizes-in-both-directions
  (let [t (uut/make-throttle :resize-test 2)]
    (try
      (is (= 2 (:limit (uut/sample t))))
      (uut/resize! t 5)
      (is (= {:in-use 0 :limit 5 :queued 0} (uut/sample t)))
      (uut/resize! t 1)
      (is (= {:in-use 0 :limit 1 :queued 0} (uut/sample t)))
      (finally
        (uut/shutdown! t)))))

(deftest shrinking-does-not-revoke-a-permit-already-held
  ;; a smaller limit must gate new work without yanking a permit from work in progress
  (let [t (uut/make-throttle :resize-held 2)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        holder (future (uut/with-throttle t
                         (.countDown entered)
                         (.await release 30 TimeUnit/SECONDS)))]
    (try
      (is (.await entered 30 TimeUnit/SECONDS))
      (uut/resize! t 1)
      (is (= 1 (:limit (uut/sample t))))
      (is (= 1 (:in-use (uut/sample t))) "the running holder still counts as in use")
      (.countDown release)
      ;; with-throttle returns the body value, and await-with-timeout returns a boolean
      (is (true? (deref holder 30000 :timed-out)) "the holder finishes once released")
      (is (= 0 (:in-use (uut/sample t))))
      (finally
        (.countDown release)
        (deref holder 30000 nil)
        (uut/shutdown! t)))))

(deftest throttles-follow-runtime-config-changes
  (let [t (uut/follow-runtime-config!
           (uut/make-throttle :rc-follow 2)
           [:throttle :command-max-concurrent])]
    (try
      (is (= 2 (:limit (uut/sample t))))
      (rc/rc [:throttle :command-max-concurrent] 9)
      (is (= 9 (:limit (uut/sample t))) "an .llar rc write must reach the live throttle")
      (finally
        (rc/reset-rc!)
        (uut/shutdown! t)))))
