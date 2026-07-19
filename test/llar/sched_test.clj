(ns llar.sched-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.tools.logging :as log]
   [java-time.api :as time]
   [llar.appconfig :as appconfig]
   [llar.db.core :as db]
   [llar.db.modify]
   [llar.db.sql :as sql]
   [llar.metrics :as metrics]
   [llar.persistency :as persistency]
   [llar.podcast :as podcast]
   [llar.store :as store]
   [llar.update :as update]
   [iapetos.core :as prometheus]
   [mount.core :as mount]
   [llar.sched :as uut])
  (:import
   [java.io Closeable]))

(defn- test-schedule [opts]
  (uut/make-schedule
   (merge {:key :test
           :mount-state "#'llar.sched-test/test"
           :sched-name "test"
           :sched-type :test
           :run-fn (constantly :ok)}
          opts)))

(deftest schedule-closes-underlying-closeable
  (let [closed? (atom false)
        closeable (reify Closeable
                    (close [_] (reset! closed? true)))
        schedule (uut/attach-closeable! (test-schedule {}) closeable)]
    (.close schedule)
    (is @closed?)))

(deftest run-schedule-records-success
  (let [schedule (test-schedule {:run-fn (constantly {:count 1})})
        result (uut/run-schedule! schedule :manual)
        state (uut/snapshot schedule)]
    (is (= {:count 1} result))
    (is (false? (:running? state)))
    (is (= :manual (:last-trigger state)))
    (is (:last-started-at state))
    (is (:last-finished-at state))
    (is (:last-duration state))
    (is (= {:count 1} (:last-result state)))
    (is (nil? (:last-exception state)))))

(deftest run-schedule-publishes-next-run-and-expected-interval
  (let [base (time/zoned-date-time 2026 7 19 12 0 0 0 "UTC")
        next-run (time/plus base (time/minutes 10))
        following-run (time/plus base (time/minutes 30))
        schedule-name "metrics-test"
        schedule (test-schedule {:sched-name schedule-name
                                 :schedule-times [next-run following-run]})]
    (with-redefs [time/zoned-date-time (constantly base)]
      (uut/run-schedule! schedule :scheduled))
    (is (= (/ (time/to-millis-from-epoch next-run) 1000.0)
           (prometheus/value metrics/prom-registry
                             :llar-sched/next-run-unixtime
                             {:schedule schedule-name})))
    (is (= 1200.0
           (prometheus/value metrics/prom-registry
                             :llar-sched/expected-interval-seconds
                             {:schedule schedule-name})))))

(deftest schedule-timing-advances-across-irregular-runs
  (let [base (time/zoned-date-time 2026 7 19 12 0 0 0 "UTC")
        first-run (time/plus base (time/minutes 5))
        second-run (time/plus base (time/minutes 15))
        third-run (time/plus base (time/minutes 45))
        schedule-name "irregular-metrics-test"
        schedule (test-schedule {:sched-name schedule-name
                                 :schedule-times [first-run second-run third-run]})]
    (with-redefs [time/zoned-date-time (constantly base)]
      (#'uut/observe-schedule-timing! schedule))
    (is (= 600.0
           (prometheus/value metrics/prom-registry
                             :llar-sched/expected-interval-seconds
                             {:schedule schedule-name})))
    (with-redefs [time/zoned-date-time (constantly (time/plus base (time/minutes 6)))]
      (#'uut/observe-schedule-timing! schedule))
    (is (= (/ (time/to-millis-from-epoch second-run) 1000.0)
           (prometheus/value metrics/prom-registry
                             :llar-sched/next-run-unixtime
                             {:schedule schedule-name})))
    (is (= 1800.0
           (prometheus/value metrics/prom-registry
                             :llar-sched/expected-interval-seconds
                             {:schedule schedule-name})))))

(deftest snapshot-includes-next-run-at
  (let [past (time/minus (time/zoned-date-time) (time/minutes 5))
        future (time/plus (time/zoned-date-time) (time/minutes 5))
        schedule (test-schedule {:schedule-times [past future]})]
    (is (= future (:next-run-at (uut/snapshot schedule))))))

(deftest canned-schedule-metadata-is-doc-safe
  (let [metadata (uut/canned-schedule-metadata)]
    (is (= [:during-daytime
            :sundays
            :early-morning
            :now-and-early-morning
            :now-and-hourly
            :hourly
            :now-and-every-5-minutes]
           (keys metadata)))
    (is (= (keys uut/canned-schedules) (keys metadata)))
    (is (every? fn? (map :schedule-times (vals uut/canned-schedules))))
    (is (every? string? (map :description (vals metadata))))
    (is (not-any? :schedule-times (vals metadata)))))

(deftest resolves-canned-schedule-keywords
  (is (seq (take 1 (uut/resolve-chime-times :hourly))))
  (try
    (uut/resolve-chime-times :not-a-schedule)
    (is false "Expected unknown canned schedule to throw")
    (catch clojure.lang.ExceptionInfo ex
      (is (= :llar.sched/unknown-canned-sched (:type (ex-data ex))))
      (is (= :not-a-schedule (:keyword (ex-data ex))))
      (is (= (keys (uut/canned-schedule-metadata)) (:supported (ex-data ex)))))))

(deftest now-and-canned-schedules-are-ordered
  (doseq [schedule-key [:now-and-early-morning
                        :now-and-hourly
                        :now-and-every-5-minutes]
          :let [times (take 5 (uut/resolve-chime-times schedule-key))]]
    (is (apply <= (map #(time/to-millis-from-epoch %) times))
        (str schedule-key " should return ascending times"))))

(deftest now-and-schedule-times-skips-startup-window
  (let [base (time/zoned-date-time)
        near-cadence (time/plus base (time/seconds 60))
        later-cadence (time/plus base (time/seconds 180))]
    (with-redefs [clojure.core/rand-int (constantly 30)]
      (let [[startup next-cadence] (take 2 (#'uut/now-and-schedule-times
                                            [near-cadence later-cadence]))]
        (is (time/before? startup near-cadence))
        (is (= later-cadence next-cadence))))))

(deftest run-schedule-records-failure
  (let [ex (ex-info "boom" {:type :test/boom})
        schedule (test-schedule {:run-fn (fn [] (throw ex))})]
    (is (thrown? clojure.lang.ExceptionInfo
                 (uut/run-schedule! schedule :scheduled)))
    (let [state (uut/snapshot schedule)]
      (is (false? (:running? state)))
      (is (= :scheduled (:last-trigger state)))
      (is (:last-started-at state))
      (is (:last-finished-at state))
      (is (:last-duration state))
      (is (= "boom" (get-in state [:last-exception :message])))
      (is (= {:type :test/boom} (get-in state [:last-exception :data]))))))

(deftest trigger-schedule-skips-duplicate-run
  (let [started (promise)
        release (promise)
        schedule (test-schedule {:run-fn (fn []
                                           (deliver started true)
                                           @release
                                           :done)})]
    (is (= :triggered (uut/trigger-schedule! schedule)))
    (is (= true (deref started 1000 false)))
    (is (= :already-running (uut/trigger-schedule! schedule)))
    (let [fut (:current-future (uut/snapshot schedule))]
      (is (future? fut))
      (deliver release true)
      (is (= :done @fut)))
    (is (nil? (:current-future (uut/snapshot schedule))))))

(deftest find-schedules-uses-mount-current-states
  (let [schedule (test-schedule {})]
    (with-redefs [mount/find-all-states (constantly ["#'llar.sched-test/test"
                                                     "#'llar.sched-test/other"])
                  mount/current-state (fn [state-name]
                                        (if (= "#'llar.sched-test/test" state-name)
                                          schedule
                                          :not-a-schedule))]
      (is (= [schedule] (vec (uut/find-schedules)))))))

(deftest podcast-schedules-use-schedule-runtime
  (mount/in-clj-mode)
  (mount/swap {#'appconfig/appconfig {:podcast nil}})
  (try
    (mount/start #'appconfig/appconfig
                 #'podcast/podcast-scanner
                 #'podcast/podcast-retention-enforcer)
    (let [names (set (map (comp :sched-name uut/snapshot) (uut/find-schedules)))]
      (is (contains? names "podcast-scanner"))
      (is (contains? names "podcast-retention-enforcer"))
      (is (uut/schedule? (mount/current-state "#'llar.podcast/podcast-scanner")))
      (is (uut/schedule? (mount/current-state "#'llar.podcast/podcast-retention-enforcer"))))
    (finally
      (mount/stop #'podcast/podcast-retention-enforcer
                  #'podcast/podcast-scanner
                  #'appconfig/appconfig))))

(deftest podcast-scanner-tick-returns-summary-map
  (reset! podcast/download-state {})
  (reset! (deref #'podcast/state-rebuilt?) false)
  (with-redefs-fn {#'podcast/rebuild-state-from-index! (fn []
                                                         (swap! podcast/download-state assoc
                                                                1 {:status :complete}))
                   #'podcast/scan-podcast-items! (fn []
                                                   (swap! podcast/download-state assoc
                                                          2 {:status :pending}))
                   #'podcast/process-pending-downloads! (fn []
                                                          {:item-id 2
                                                           :result :complete})}
    (fn []
      (let [result (#'podcast/podcast-scanner-tick!)]
        (is (= true (:rebuilt? result)))
        (is (= {:complete 1 :pending 1} (:after-scan result)))
        (is (= {:pending 1} (select-keys (:after-process result) [:pending])))
        (is (= {:item-id 2 :result :complete} (:processed result)))))))

(deftest remove-unread-for-empty-source-keys-is-noop
  (let [remove-called? (atom false)]
    (with-redefs [sql/resolve-source-keys-to-ids (constantly [])
                  sql/remove-tags (fn [& _]
                                    (reset! remove-called? true))]
      (is (= [] (persistency/remove-unread-for-items-of-source-older-then!
                 (db/->PostgresqlDataStore nil) [] (time/zoned-date-time))))
      (is (false? @remove-called?)))))

(deftest remove-unread-for-missing-tag-is-noop
  (let [remove-called? (atom false)]
    (with-redefs [sql/get-tags (constantly [])
                  sql/remove-tags (fn [& _]
                                    (reset! remove-called? true))]
      (is (= [] (persistency/remove-unread-for-items-with-tag!
                 (db/->PostgresqlDataStore nil) :missing-digest-issue)))
      (is (false? @remove-called?)))))

(deftest remove-unread-autoread-rule-skips-empty-sources
  (let [remove-called? (atom false)]
    (with-redefs [update/updateable-sources (constantly {})
                  store/backend-db :db
                  persistency/remove-unread-for-items-of-source-older-then! (fn [& _]
                                                                              (reset! remove-called? true))]
      (let [result (update/remove-unread-for-autoread-sched!
                    :reddit-4
                    {:period (time/days 28)
                     :pred (constantly false)})]
        (is (= {:name :reddit-4
                :status :skipped
                :reason :no-matching-sources
                :period (time/days 28)
                :source-count 0
                :sources []}
               result))
        (is (false? @remove-called?))))))

(deftest remove-unread-autoread-rule-includes-printable-predicate
  (with-redefs [update/updateable-sources (constantly {})
                store/backend-db :db]
    (let [predicate '(some #{:reddit} $TAGS)
          result (update/remove-unread-for-autoread-sched!
                  :reddit-4
                  {:period (time/days 28)
                   :pred (constantly false)
                   :predicate predicate})]
      (is (= predicate (:predicate result)))
      (is (not (contains? result :pred))))))

(deftest remove-unread-autoread-rule-validates-config
  (let [missing-period (update/remove-unread-for-autoread-sched!
                        :bad-period
                        {:pred (constantly true)})
        bad-predicate (update/remove-unread-for-autoread-sched!
                       :bad-predicate
                       {:period (time/days 1)
                        :pred nil})]
    (is (= :error (:status missing-period)))
    (is (= :invalid-period (:reason missing-period)))
    (is (= :error (:status bad-predicate)))
    (is (= :invalid-predicate (:reason bad-predicate)))))

(deftest remove-unread-autoread-rule-catches-predicate-errors
  (let [ex (ex-info "bad pred" {:reason :test})
        result (with-redefs [update/updateable-sources (constantly {:x {}})
                             log/error (fn [& _])]
                 (update/remove-unread-for-autoread-sched!
                  :bad-pred
                  {:period (time/days 1)
                   :pred (fn [_] (throw ex))}))]
    (is (= :error (:status result)))
    (is (= "bad pred" (get-in result [:error :message])))
    (is (= {:reason :test} (get-in result [:error :data])))))

(deftest remove-unread-tags-status-is-error-when-child-errors
  (is (= :ok (update/remove-unread-tags-status
              [{:status :ok} {:status :skipped}]
              nil)))
  (is (= :error (update/remove-unread-tags-status
                 [{:status :error}]
                 nil)))
  (is (= :error (update/remove-unread-tags-status
                 [{:status :ok}]
                 {:status :error}))))
