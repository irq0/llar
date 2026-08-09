(ns llar.pool-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.metrics.resources :as resources]
   [llar.pool :as uut]
   [llar.rc :as rc]
   [slingshot.slingshot :refer [throw+ try+]])
  (:import
   [java.util.concurrent Callable CountDownLatch ExecutionException ExecutorService TimeUnit]))

(def ^:dynamic *conveyed* :unbound)

(deftest slow-element-does-not-starve-the-pool
  ;; The defect this whole change exists to fix. clojure.core/pmap advances its
  ;; cores+2 window only as results are consumed IN INPUT ORDER, so a blocked
  ;; element 0 stops everything past the window from even being submitted. The
  ;; collection is deliberately far larger than any plausible pmap window.
  (let [n 128
        release (CountDownLatch. 1)
        done (CountDownLatch. (dec n))
        pool (uut/make-pool :hol-test 2)]
    (try
      (let [result (future
                     (uut/pmap-failfast pool
                                        (fn [i]
                                          (if (zero? i)
                                            (do (.await release) :blocked)
                                            (do (.countDown done) i)))
                                        (range n)))]
        (is (.await done 30 TimeUnit/SECONDS)
            "every element except the blocked one must finish while it is still blocked")
        (.countDown release)
        (is (= n (count @result))))
      (finally
        (.countDown release)
        (uut/shutdown! pool)))))

(deftest pool-never-exceeds-its-configured-size
  (let [size 3
        in-flight (atom 0)
        peak (atom 0)
        pool (uut/make-pool :cap-test size)]
    (try
      (uut/pmap-failfast pool
                         (fn [_]
                           (swap! peak max (swap! in-flight inc))
                           (Thread/sleep 5)
                           (swap! in-flight dec)
                           :ok)
                         (range 40))
      (is (<= @peak size))
      (finally
        (uut/shutdown! pool)))))

(deftest nested-pools-do-not-deadlock
  ;; Source tasks block on item tasks. With one shared pool this starves and hangs
  ;; forever, which is why source-pool and item-pool must stay separate.
  (let [sources (uut/make-pool :nested-source 1)
        items (uut/make-pool :nested-item 1)]
    (try
      (is (= [[0 1] [0 1]]
             (uut/pmap-failfast sources
                                (fn [_] (uut/pmap-failfast items identity (range 2)))
                                (range 2))))
      (finally
        (uut/shutdown! sources)
        (uut/shutdown! items)))))

(deftest failfast-rethrows-the-original-cause-not-the-execution-exception
  (let [pool (uut/make-pool :ff-cause 2)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"boom"
           (uut/pmap-failfast pool
                              (fn [i] (if (= i 1) (throw (ex-info "boom" {})) i))
                              (range 4))))
      (finally
        (uut/shutdown! pool)))))

(deftest failfast-preserves-slingshot-selectors
  ;; postproc.clj classifies :temp-fail vs :perm-fail off exactly these selectors;
  ;; losing them would silently turn retryable failures into permanent ones
  (let [pool (uut/make-pool :ff-slingshot 2)]
    (try
      (is (= :retry-later
             (try+
              (uut/pmap-failfast pool
                                 (fn [i]
                                   (if (= i 1)
                                     (throw+ {:type :llar.http/client-error-retry-later})
                                     i))
                                 (range 4))
              (catch [:type :llar.http/client-error-retry-later] _ :retry-later))))
      (finally
        (uut/shutdown! pool)))))

(deftest failfast-cancels-work-behind-the-failure
  ;; a postproc temp-fail dooms the whole source, so running the remaining items is waste.
  ;; Every element after the first blocks on a gate that is only opened during teardown,
  ;; so any increment would mean that element was left running rather than cancelled.
  ;; (Trivial non-blocking tasks cannot express this: on a fast machine the worker drains
  ;; them before the cancel loop is even reached.)
  (let [n 32
        ran (atom 0)
        gate (CountDownLatch. 1)
        pool (uut/make-pool :ff-cancel 1)]
    (try
      (is (thrown? clojure.lang.ExceptionInfo
                   (uut/pmap-failfast pool
                                      (fn [i]
                                        (if (zero? i)
                                          (throw (ex-info "boom" {}))
                                          (do (.await gate 30 TimeUnit/SECONDS)
                                              (swap! ran inc))))
                                      (range n))))
      (is (zero? @ran) "elements behind the failure must be cancelled, not merely queued")
      (finally
        (.countDown gate)
        (uut/shutdown! pool)))))

(deftest isolated-reports-an-outcome-per-element
  (let [pool (uut/make-pool :iso-outcomes 2)]
    (try
      (let [results (uut/pmap-isolated pool
                                       (fn [i]
                                         (if (= i 1)
                                           (throw (ex-info "boom" {}))
                                           (* i 10)))
                                       (range 4))]
        (is (= [true false true true] (mapv :ok? results)))
        (is (= [0 nil 20 30] (mapv :value results)))
        (is (= "boom" (ex-message (:error (second results))))))
      (finally
        (uut/shutdown! pool)))))

(deftest isolated-never-lets-one-failure-discard-the-batch
  (let [pool (uut/make-pool :iso-isolation 2)]
    (try
      (let [results (uut/pmap-isolated pool
                                       (fn [i] (if (even? i) (throw (ex-info "boom" {})) i))
                                       (range 10))]
        (is (= 10 (count results)))
        (is (= 5 (count (filter :ok? results)))))
      (finally
        (uut/shutdown! pool)))))

(deftest dynamic-bindings-reach-pool-tasks
  ;; pmap dispatches through future, which conveys bindings; a raw Callable does not
  (let [pool (uut/make-pool :binding-test 2)]
    (try
      (binding [*conveyed* :bound]
        (is (= [:bound :bound]
               (uut/pmap-failfast pool (fn [_] *conveyed*) (range 2))))
        (is (= [:bound :bound]
               (mapv :value (uut/pmap-isolated pool (fn [_] *conveyed*) (range 2))))))
      (finally
        (uut/shutdown! pool)))))

(deftest results-keep-input-order-regardless-of-completion-order
  (let [pool (uut/make-pool :order-test 4)]
    (try
      (is (= (vec (range 20))
             (uut/pmap-failfast pool (fn [i] (Thread/sleep (- 20 i)) i) (range 20))))
      (is (= (vec (range 20))
             (mapv :value
                   (uut/pmap-isolated pool (fn [i] (Thread/sleep (- 20 i)) i) (range 20)))))
      (finally
        (uut/shutdown! pool)))))

(deftest empty-collections-are-not-an-error
  (let [pool (uut/make-pool :empty-test 2)]
    (try
      (is (= [] (uut/pmap-failfast pool identity [])))
      (is (= [] (uut/pmap-isolated pool identity [])))
      (finally
        (uut/shutdown! pool)))))

(deftest auto-size-resolves-to-the-cpu-count
  (is (= (.availableProcessors (Runtime/getRuntime)) (uut/resolve-size :auto)))
  (is (= 4 (uut/resolve-size 4))))

(deftest pools-register-and-unregister-with-the-metrics-registry
  (let [pool (uut/make-pool :registry-test 3)]
    (is (contains? @resources/resources :registry-test))
    (is (= {:in-use 0 :limit 3 :queued 0 :completed 0} (uut/sample pool)))
    (is (true? (uut/shutdown! pool)))
    (is (.isTerminated ^ExecutorService (:executor pool)))
    (is (not (contains? @resources/resources :registry-test)))))

(deftest shutdown-cancels-the-queue-and-only-waits-for-active-work
  (let [pool (uut/make-pool :fast-shutdown-test 1)
        executor ^ExecutorService (:executor pool)
        entered (CountDownLatch. 1)
        interrupted (CountDownLatch. 1)
        queued-ran? (atom false)
        active (.submit executor
                        ^Callable
                        (reify Callable
                          (call [_]
                            (.countDown entered)
                            (try
                              (Thread/sleep 30000)
                              (catch InterruptedException _
                                (.countDown interrupted))))))
        queued (.submit executor
                        ^Callable
                        (reify Callable
                          (call [_]
                            (reset! queued-ran? true))))]
    (is (.await entered 10 TimeUnit/SECONDS))
    (let [started (System/nanoTime)]
      (is (false? (uut/shutdown! pool 50)))
      (is (< (/ (- (System/nanoTime) started) 1000000.0) 1000)))
    (is (.await interrupted 10 TimeUnit/SECONDS))
    (is (.isDone active))
    (is (.isCancelled queued))
    (is (false? @queued-ran?))))

(deftest resize-applies-in-both-directions
  (let [pool (uut/make-pool :resize-test 2)]
    (try
      (uut/resize! pool 6)
      (is (= 6 (:limit (uut/sample pool))))
      (uut/resize! pool 1)
      (is (= 1 (:limit (uut/sample pool))))
      (finally
        (uut/shutdown! pool)))))

(deftest call-on-runs-a-single-call-on-the-pool
  (let [pool (uut/make-pool :call-on-test 2)]
    (try
      (is (= :value (uut/call-on pool (constantly :value))))
      (is (re-find #"^llar-call-on-test-"
                   (uut/call-on pool #(.getName (Thread/currentThread)))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                            (uut/call-on pool #(throw (ex-info "boom" {})))))
      (finally
        (uut/shutdown! pool)))))

(deftest pools-follow-runtime-config-changes
  ;; pools are constructed at Mount start, before .llar files load, so the configured
  ;; size has to be reconciled when overrides arrive
  (let [pool (uut/follow-runtime-config!
              (uut/make-pool :rc-follow 2)
              [:throttle :source-update-max-concurrent])]
    (try
      (is (= 2 (:limit (uut/sample pool))))
      (rc/rc [:throttle :source-update-max-concurrent] 9)
      (is (= 9 (:limit (uut/sample pool))))
      (finally
        (rc/reset-rc!)
        (uut/shutdown! pool)))))

(deftest failfast-cancels-queued-work-when-the-caller-is-interrupted
  ;; shutdown! interrupts workers once its grace period expires. A source task blocked
  ;; in here must not leave its item tasks running behind it.
  (let [pool (uut/make-pool :ff-interrupt 1)
        caller (promise)
        gate (CountDownLatch. 1)
        ran (atom 0)
        ;; only the head blocks; the rest would increment the moment a worker frees up,
        ;; so releasing the gate after the interrupt gives un-cancelled work its chance
        result (future
                 (deliver caller (Thread/currentThread))
                 (uut/pmap-failfast pool
                                    (fn [i]
                                      (if (zero? i)
                                        (.await gate 30 TimeUnit/SECONDS)
                                        (swap! ran inc)))
                                    (range 8)))]
    (try
      (is (some? (deref caller 10000 nil)))
      (Thread/sleep 200)
      (.interrupt ^Thread @caller)
      (let [thrown (try @result nil (catch ExecutionException e e))]
        (is (instance? InterruptedException (some-> thrown .getCause))
            "interruption must propagate, not be swallowed"))
      (.countDown gate)
      (Thread/sleep 500)
      (is (zero? @ran) "queued tasks must be cancelled, not left to run later")
      (finally
        (.countDown gate)
        (uut/shutdown! pool)))))

(deftest isolated-propagates-caller-interruption-instead-of-reporting-outcomes
  ;; "never propagates" is about task failure, not about the caller being torn down:
  ;; swallowing the interrupt here left the remaining .get calls blocking again, because
  ;; .get clears the interrupt flag when it throws
  (let [pool (uut/make-pool :iso-interrupt 1)
        caller (promise)
        gate (CountDownLatch. 1)
        result (future
                 (deliver caller (Thread/currentThread))
                 (uut/pmap-isolated pool
                                    (fn [_] (.await gate 30 TimeUnit/SECONDS))
                                    (range 8)))]
    (try
      (is (some? (deref caller 10000 nil)))
      (Thread/sleep 200)
      (.interrupt ^Thread @caller)
      (let [thrown (try (deref result 20000 ::timeout) nil
                        (catch ExecutionException e e))]
        (is (instance? InterruptedException (some-> thrown .getCause))))
      (finally
        (.countDown gate)
        (uut/shutdown! pool)))))
