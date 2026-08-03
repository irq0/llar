(ns llar.update-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [llar.config :as config]
   [llar.fetch.feed]
   [llar.http :as http]
   [llar.pool :as pool]
   [llar.src :as src]
   [llar.update :as uut]
   [mount.core :as mount]
   [slingshot.slingshot :refer [throw+]])
  (:import
   [java.util.concurrent CountDownLatch TimeUnit]))

;; batch updates run on the real bounded source pool
(use-fixtures :once
  (fn [f]
    (mount/start #'pool/source-pool #'pool/item-pool)
    (try
      (f)
      (finally
        (mount/stop #'pool/source-pool #'pool/item-pool)))))

(use-fixtures :each
  (fn [f]
    (uut/reset-in-flight!)
    (f)
    (uut/reset-in-flight!)))

(defn- source-state [source-key status]
  {source-key (merge uut/src-state-template {:key source-key :status status})})

(deftest conditional-feed-update-reuses-and-preserves-etag
  (let [source-key :hardcoresoftware
        source (src/feed "https://example.com/feed.xml")
        conditional-tokens {:etag "W/\"ec627-nOlkMvp2fc86N4xolHdhQgksVmY\""}
        source-state (merge uut/src-state-template
                            {:key source-key
                             :status :ok
                             :fetch-meta {:conditional-tokens conditional-tokens}})
        requested-conditionals (atom nil)]
    (with-redefs [uut/state (atom {source-key source-state})
                  config/get-source (constantly {:src source})
                  http/fetch
                  (fn [_url & {:keys [conditionals]}]
                    (reset! requested-conditionals conditionals)
                    {:status :not-modified
                     :conditional-tokens conditionals})]
      (is (= :ok (uut/update! source-key :skip-proc true :skip-store true)))
      (is (= conditional-tokens @requested-conditionals))
      (is (= {:conditional-tokens conditional-tokens}
             (get-in @uut/state [source-key :fetch-meta])))
      (is (= {:fetched 0 :processed 0 :db 0}
             (get-in @uut/state [source-key :stats]))))))

(deftest conditional-feed-timeout-becomes-temp-failure
  (let [source-key :hardcoresoftware
        source (src/feed "https://example.com/feed.xml")
        conditional-tokens {:etag "W/\"ec627-nOlkMvp2fc86N4xolHdhQgksVmY\""}
        source-state (merge uut/src-state-template
                            {:key source-key
                             :status :ok
                             :fetch-meta {:conditional-tokens conditional-tokens}})]
    (with-redefs [uut/state (atom {source-key source-state})
                  config/get-source (constantly {:src source})
                  http/fetch (fn [& _]
                               (throw+ {:type :llar.http/server-error-retry-later
                                        :reason-class :timeout}))]
      (is (= :temp-fail (uut/update! source-key :skip-proc true :skip-store true)))
      (is (= 1 (get-in @uut/state [source-key :retry-count])))
      (is (= {:conditional-tokens conditional-tokens}
             (get-in @uut/state [source-key :fetch-meta])))
      (is (= :timeout
             (get-in @uut/state
                     [source-key :last-exception :data :reason-class]))))))

(deftest concurrent-updates-of-one-source-fetch-once
  ;; update! read the status and then wrote :updating non-atomically, so two callers
  ;; could both see :ok and both fetch. :force made it unconditional: it skipped the
  ;; policy check entirely, and the dashboard forces every manual update.
  (let [source-key :single-flight
        source (src/feed "https://example.com/feed.xml")
        fetches (atom 0)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)]
    (with-redefs [uut/state (atom (source-state source-key :ok))
                  config/get-source (constantly {:src source})
                  http/fetch (fn [& _]
                               (swap! fetches inc)
                               (.countDown entered)
                               (.await release 30 TimeUnit/SECONDS)
                               {:status :not-modified :conditional-tokens nil})]
      (let [first-run (future (uut/update! source-key :skip-proc true :skip-store true))]
        (is (.await entered 30 TimeUnit/SECONDS) "first update should reach the fetch")
        (is (= :updating (uut/update! source-key :skip-proc true :skip-store true))
            "a concurrent plain update must not start a second fetch")
        (is (= :updating (uut/update! source-key :force true :skip-proc true :skip-store true))
            "force may bypass state policy but never single-flight")
        (.countDown release)
        (is (= :ok @first-run))
        (is (= 1 @fetches))))))

(deftest duplicate-update-reports-already-in-flight
  (let [source-key :single-flight-outcome
        source (src/feed "https://example.com/feed.xml")
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)]
    (with-redefs [uut/state (atom (source-state source-key :ok))
                  config/get-source (constantly {:src source})
                  http/fetch (fn [& _]
                               (.countDown entered)
                               (.await release 30 TimeUnit/SECONDS)
                               {:status :not-modified :conditional-tokens nil})]
      (let [first-run (future (uut/update! source-key :skip-proc true :skip-store true))]
        (is (.await entered 30 TimeUnit/SECONDS))
        (is (= {:source-key source-key
                :outcome :skipped
                :reason :already-in-flight
                :status :updating}
               (uut/update-outcome! source-key :skip-proc true :skip-store true)))
        (.countDown release)
        @first-run))))

(deftest update-outcome-reports-completion-and-policy-skips
  (let [source-key :outcome-shapes
        source (src/feed "https://example.com/feed.xml")
        fetches (atom 0)]
    (with-redefs [uut/state (atom (source-state source-key :perm-fail))
                  config/get-source (constantly {:src source})
                  http/fetch (fn [& _]
                               (swap! fetches inc)
                               {:status :not-modified :conditional-tokens nil})]
      (is (= {:source-key source-key
              :outcome :skipped
              :reason :state-policy
              :status :perm-fail}
             (uut/update-outcome! source-key :skip-proc true :skip-store true)))
      (is (zero? @fetches) "a perm-failed source must not be fetched without force")
      (is (= {:source-key source-key :outcome :completed :status :ok}
             (uut/update-outcome! source-key :force true :skip-proc true :skip-store true)))
      (is (= 1 @fetches) "force must still get a failed source moving"))))

(deftest update-sources-returns-ordered-outcomes
  (let [source (src/feed "https://example.com/feed.xml")
        keys [:batch-a :batch-b :batch-c]]
    (with-redefs [uut/state (atom (merge (source-state :batch-a :ok)
                                         (source-state :batch-b :perm-fail)
                                         (source-state :batch-c :ok)))
                  config/get-source (constantly {:src source})
                  http/fetch (constantly {:status :not-modified :conditional-tokens nil})]
      (is (= [{:source-key :batch-a :outcome :completed :status :ok}
              {:source-key :batch-b :outcome :skipped :reason :state-policy :status :perm-fail}
              {:source-key :batch-c :outcome :completed :status :ok}]
             (uut/update-sources! keys :skip-proc true :skip-store true))))))

(deftest one-broken-source-does-not-discard-the-batch
  ;; pmap + doall used to propagate the first exception, throwing away every other
  ;; source's result and surfacing only as the schedule's :last-exception
  (let [source (src/feed "https://example.com/feed.xml")]
    (with-redefs [uut/state (atom (merge (source-state :ok-before :ok)
                                         (source-state :ok-after :ok)))
                  config/get-source (fn [k]
                                      (if (= k :exploding)
                                        (throw (ex-info "config blew up" {:key k}))
                                        {:src source}))
                  http/fetch (constantly {:status :not-modified :conditional-tokens nil})]
      (let [results (uut/update-sources! [:ok-before :exploding :ok-after]
                                         :skip-proc true :skip-store true)]
        (is (= [:completed :error :completed] (mapv :outcome results)))
        (is (= "config blew up" (get-in results [1 :error :message])))
        (is (= :exploding (get-in results [1 :error :data :key])))))))
