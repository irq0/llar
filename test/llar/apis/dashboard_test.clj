(ns llar.apis.dashboard-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [hiccup2.core :as h]
   [java-time.api :as time]
   [mount.core :as mount]
   [llar.appconfig :as appconfig]
   [llar.apis.podcast :as podcast-api]
   [llar.config :as config]
   [llar.config-lab :as config-lab]
   [llar.db.bookmark-capture :as capture-db]
   [llar.persistency :as persistency]
   [llar.podcast :as podcast]
   [llar.rc :as rc]
   [llar.sched :as sched]
   [llar.update :as update]
   [llar.metrics.resources :as resources]
   [llar.work :as work]
   [llar.apis.dashboard :as uut]))

(deftest bookmarks-tab-shows-queue-health-and-recovery-actions
  (with-redefs [capture-db/dashboard-counts
                (constantly {:ready 1 :processing 0 :retry-wait 2 :failed 1 :complete 3})
                capture-db/list-captures
                (constantly [{:id 42
                              :url "https://example.com/failed"
                              :title "Failed capture"
                              :status :failed
                              :attempt-count 5
                              :submitted-by "iphone"
                              :last-attempt-ts (time/zoned-date-time 2026 8 13 12 0 0 0 "UTC")
                              :failure-class "fetch"
                              :last-error "network failed"
                              :item-id 314}])
                appconfig/reader
                (fn [key]
                  (when (= key :base-url)
                    "https://reader.example/"))]
    (let [body (str (h/html (uut/bookmarks-tab)))]
      (is (string/includes? body "1 Ready"))
      (is (string/includes? body "id=\"bookmarks-datatable\""))
      (is (string/includes? body "data-order=\"1786622400000\""))
      (is (string/includes? body "Failed capture"))
      (is (string/includes? body "network failed"))
      (is (string/includes? body "href=\"https://reader.example/reader/item/by-id/314\""))
      (is (string/includes? body "Open in Reader"))
      (is (string/includes? body "/api/bookmark-captures/42/retry"))
      (is (string/includes? body "/api/bookmark-captures/42/dismiss")))))

(deftest bookmark-recovery-routes-use-explicit-state-transitions
  (with-redefs [capture-db/retry! (fn [_ id] {:id id :status :pending})
                capture-db/dismiss! (fn [_ id] {:id id :status :dismissed})]
    (let [retry-response (uut/app {:request-method :post
                                   :uri "/api/bookmark-captures/42/retry"})
          dismiss-response (uut/app {:request-method :post
                                     :uri "/api/bookmark-captures/42/dismiss"})]
      (is (= 200 (:status retry-response)))
      (is (= {:capture-id 42 :status :pending} (:body retry-response)))
      (is (= 200 (:status dismiss-response)))
      (is (= {:capture-id 42 :status :dismissed} (:body dismiss-response)))))
  (is (= 400 (:status (uut/bookmark-capture-retry "not-an-id")))))

(deftest status-index-renders-non-source-tabs-lazily
  (testing "initial dashboard render does not execute heavy hidden tabs"
    (with-redefs [uut/tabs {:overview (fn [] [:div "overview summary"])
                            :sources (fn [] (throw (ex-info "should not render sources" {})))
                            :heavy (fn [] (throw (ex-info "should not render" {})))}]
      (let [body (uut/status-index)]
        (is (string/includes? body "overview summary"))
        (is (string/includes? body "data-tab-name=\"sources\""))
        (is (string/includes? body "data-tab-name=\"heavy\""))
        (is (string/includes? body "Click tab to load..."))
        (is (not (string/includes? body "should not render sources")))
        (is (not (string/includes? body "should not render")))))))

(deftest dashboard-tab-renders-known-non-source-tabs
  (with-redefs [uut/tabs {:overview (fn [] [:div "overview summary"])
                          :sources (fn [] [:div "sources table"])
                          :memory (fn [] [:div "memory tab"])}]
    (testing "known non-source tab renders HTML"
      (let [response (uut/dashboard-tab "memory")]
        (is (= 200 (:status response)))
        (is (= "text/html; charset=utf-8" (get-in response [:headers "Content-Type"])))
        (is (string/includes? (:body response) "memory tab"))))

    (testing "sources is lazy-loadable"
      (let [response (uut/dashboard-tab "sources")]
        (is (= 200 (:status response)))
        (is (string/includes? (:body response) "sources table"))))

    (testing "overview is reloadable"
      (let [response (uut/dashboard-tab "overview")]
        (is (= 200 (:status response)))
        (is (string/includes? (:body response) "overview summary"))))

    (testing "unknown tab returns 404"
      (is (= 404 (:status (uut/dashboard-tab "unknown")))))))

(deftest dashboard-tab-route-renders-known-tab
  (with-redefs [uut/tabs {:overview (fn [] [:div "overview summary"])
                          :sources (fn [] [:div "sources table"])
                          :memory (fn [] [:div "memory tab"])}]
    (let [response (uut/app {:request-method :get
                             :uri "/tab/memory"})]
      (is (= 200 (:status response)))
      (is (string/includes? (:body response) "memory tab")))))

(deftest source-details-lazily-exposes-reader-state
  (is (not (string/includes? (str (h/html (uut/source-tab)))
                             "btn-mark-source-read")))
  (with-redefs [config/get-source (constantly {:src :feed})
                uut/get-state (constantly {:status :ok})
                uut/source-unread-count (constantly 146)]
    (let [body (uut/source-details "example")]
      (is (string/includes? body "Reader state"))
      (is (string/includes? body "Unread: <strong>146</strong>"))
      (is (string/includes? body "btn-mark-source-read"))
      (is (string/includes? body "data-source-key=\"example\""))
      (is (not (re-find #"btn-mark-source-read[^>]*disabled" body)))))
  (with-redefs [config/get-source (constantly {:src :feed})
                uut/get-state (constantly {})
                uut/source-unread-count (constantly 0)]
    (let [body (uut/source-details "empty")]
      (is (string/includes? body "Unread: <strong>0</strong>"))
      (is (re-find #"btn-mark-source-read[^>]*disabled" body)))))

(deftest source-unread-count-reuses-source-tag-counts
  (with-redefs [config/get-sources (constantly {:example {:src :feed}})
                persistency/get-sources
                (fn [_ config-sources]
                  {:example (assoc (:example config-sources) :id 7)})
                persistency/sources-merge-in-tags-counts
                (fn [_ sources]
                  (map #(assoc % :item-tags {:unread 12 :total 30}) sources))]
    (is (= 12 (uut/source-unread-count :example)))
    (is (= 0 (uut/source-unread-count :missing)))))

(deftest source-reader-state-actions-report-exact-results
  (with-redefs [config/get-source (constantly {:src :feed})
                persistency/remove-unread-for-items-of-source-older-then!
                (constantly [{:id 7} {:id 9}])
                uut/source-unread-count (constantly 0)]
    (is (= {:source-key :example
            :affected-count 2
            :unread-count 0}
           (:body (uut/mark-source-read "example")))))
  (with-redefs [config/get-source (constantly nil)]
    (is (= 404 (:status (uut/mark-source-read "missing"))))))

(deftest source-reader-state-dashboard-script-confirms-the-action
  (let [javascript (slurp (io/resource "status/llar-status.js"))]
    (is (string/includes? javascript ".btn-mark-source-read"))
    (is (string/includes? javascript "Saved items and reading progress will be preserved."))
    (is (not (string/includes? javascript "/read/undo")))
    (is (not (string/includes? javascript ".btn-undo-source-read")))))

(deftest config-lab-tab-is-opt-in
  (with-redefs [config-lab/enabled? (constantly false)]
    (is (= 404 (:status (uut/dashboard-tab "config-lab")))))
  (with-redefs [config-lab/enabled? (constantly true)]
    (let [response (uut/dashboard-tab "config-lab")]
      (is (= 200 (:status response)))
      (is (string/includes? (:body response) "Ephemeral lab"))
      (is (string/includes? (:body response) "config-lab-run"))
      (is (string/includes? (:body response) "Run a source to begin"))
      (is (string/includes? (:body response) "config-lab-preview-frame"))
      (is (string/includes? (:body response) "Export configuration"))
      (is (string/includes? (:body response) "Extraction"))
      (is (string/includes? (:body response) "config-lab-data-tree"))
      (is (string/includes? (:body response) "Clojure readable notation"))
      (is (not (string/includes? (:body response) "Copy JSON"))))))

(deftest docs-tab-renders-config-docs
  (let [response (uut/dashboard-tab "docs")]
    (is (= 200 (:status response)))
    (is (string/includes? (:body response) "LLAR Configuration"))
    (is (string/includes? (:body response) "sched-fetch"))
    (is (string/includes? (:body response) ":now-and-hourly"))))

(deftest config-tab-renders-system-config-and-rc
  (with-redefs [appconfig/appconfig {:postgresql {:frontend {:password "secret"}
                                                  :backend {:password "secret"}}
                                     :ui {:favorites [[:appconfig :source-tag]]
                                          :default-list-view {:appconfig :gallery}}
                                     :ranking {:highlight-boost-hours 12
                                               :rarity-boost-cap-hours 24}}]
    (rc/reset-rc!)
    (rc/rc [:reader :ranking :highlight-boost-hours] 96)
    (let [response (uut/dashboard-tab "config")]
      (is (= 200 (:status response)))
      (is (string/includes? (:body response) "System config"))
      (is (string/includes? (:body response) "Runtime config (rc)"))
      (is (string/includes? (:body response) "Effective"))
      (is (string/includes? (:body response) "Runtime overrides (.llar)"))
      (is (string/includes? (:body response) "Baseline (defaults + appconfig)"))
      (is (string/includes? (:body response) "Supported rc paths"))
      (is (string/includes? (:body response) "[:reader :favorites]"))
      (is (string/includes? (:body response) "[:ui :favorites]"))
      (is (string/includes? (:body response) "96"))
      (is (string/includes? (:body response) "--secret removed--")))
    (rc/reset-rc!)))

(deftest metrics-endpoint-has-prometheus-content-type
  (testing "/metrics returns a non-blank Prometheus text format content type"
    (let [response (uut/app {:request-method :get
                             :uri "/metrics"})]
      (is (= 200 (:status response)))
      (is (string/starts-with? (get-in response [:headers "Content-Type"])
                               "text/plain; version=0.0.4"))
      (is (string/includes? (:body response) "# HELP")))))

(deftest schedule-tab-renders-runtime-state
  (let [schedule (sched/make-schedule
                  {:key :daily
                   :mount-state "#'llar.test/daily"
                   :sched-name "daily"
                   :sched-type :defsched
                   :chime-times :hourly
                   :schedule-times [(time/plus (time/zoned-date-time) (time/minutes 5))]
                   :pred '(do :work)
                   :run-fn (constantly :ok)})]
    (swap! (:state* schedule) assoc
           :running? false
           :last-duration (time/seconds 90)
           :last-trigger :manual
           :last-result {:count 1})
    (with-redefs [sched/find-schedules (constantly [schedule])]
      (let [body (str (h/html (uut/schedule-tab)))]
        (is (string/includes? body "daily"))
        (is (string/includes? body "Next Run"))
        (is (string/includes? body "in "))
        (is (string/includes? body "1 m 30 s"))
        (is (string/includes? body "llar.test/daily"))
        (is (string/includes? body "manual"))
        (is (string/includes? body ":count"))
        (is (string/includes? body "data-clojure-inspector-variant=\"compact\""))
        (is (string/includes? body "table-responsive"))
        (is (string/includes? body "id=\"schedules-datatable\""))
        (is (string/includes? body "class=\"schedule-row\""))
        (is (string/includes? body "class=\"schedule-detail-row\""))
        (is (string/includes? body "data-order=\"90000\""))
        (is (string/includes? body "bg-body-tertiary"))
        (is (string/includes? body "colspan=\"11\""))
        (is (not (string/includes? body ">Inspect<")))
        (is (string/includes? body "btn-run-schedule"))))))

(deftest ranking-chart-is-server-rendered
  (let [body (str (h/html (#'uut/ranking-bar-chart
                           "Rarity"
                           [{:source_key :one :rarity_boost_hours 10.0}
                            {:source_key :two :rarity_boost_hours 5.0}]
                           :rarity_boost_hours
                           #(format "%.1fh" %)
                           "bg-primary")))
        footer (apply str (map #(str (h/html %)) (uut/html-footer)))]
    (is (string/includes? body "dashboard-ranking-chart"))
    (is (string/includes? body "progress-bar bg-primary"))
    (is (string/includes? body "width:100.00%"))
    (is (string/includes? body "width:50.00%"))
    (is (string/includes? body "10.0h"))
    (is (not (string/includes? footer "chartjs")))))

(deftest ranking-sources-are-grouped-by-equal-boost
  (let [groups (#'uut/rarity-boost-groups
                [{:source_key :rare-a :rarity_boost_hours 168.0 :items_7d 2 :highlight_count 1}
                 {:source_key :rare-b :rarity_boost_hours 168.0 :items_7d 3 :highlight_count 0}
                 {:source_key :daily :rarity_boost_hours 24.0 :items_7d 7 :highlight_count 2}]
                168.0)]
    (is (= [168.0 24.0] (mapv :rarity_boost_hours groups)))
    (is (= 2 (:source_count (first groups))))
    (is (= ["rare-a" "rare-b"] (:sources (first groups))))
    (is (= 5 (:items_7d (first groups))))
    (is (true? (:capped? (first groups))))))

(deftest ranking-preview-explains-the-score
  (let [body (str (h/html (#'uut/ranking-preview-item
                           {:title "An item"
                            :source_key :daily
                            :age_hours 30.0
                            :rarity_boost_hours 24.0
                            :highlight_boost_hours 0.0
                            :effective_age_hours 6.0})))]
    (is (string/includes? body "30.0h age − 24.0h rarity − 0.0h highlight = 6.0h effective age"))))

(deftest state-tab-renders-values-as-full-width-details
  (with-redefs [mount/find-all-states (constantly ["#'llar.test/example"])
                mount/running-states (constantly ["#'llar.test/example"])
                mount/current-state (constantly {:answer 42})]
    (let [body (str (h/html (uut/state-tab)))]
      (is (string/includes? body "id=\"states-datatable\""))
      (is (string/includes? body "data-order=\"1\""))
      (is (string/includes? body "class=\"state-row\""))
      (is (string/includes? body "class=\"state-detail-row\""))
      (is (string/includes? body "colspan=\"3\""))
      (is (string/includes? body "data-clojure-inspector-variant=\"compact\""))
      (is (string/includes? body ":answer"))
      (is (not (string/includes? body "Inspect value"))))))

(deftest dashboard-loads-the-shipped-datatables-runtime
  (let [header (str (h/html (uut/html-header)))
        body (apply str (map #(str (h/html %)) (uut/html-footer)))
        javascript (slurp (io/resource "status/llar-status.js"))]
    (is (string/includes? header "/static/datatables/dataTables.bootstrap5.min.css?v=3.0.1"))
    (is (string/includes? body "/static/jquery/jquery.min.js?v=4.0.0"))
    (is (string/includes? body "/static/datatables/dataTables.min.js?v=3.0.1"))
    (is (string/includes? body "/static/datatables/dataTables.bootstrap5.min.js?v=3.0.1"))
    (is (not (string/includes? body "/static/datatables/jquery.dataTables.min.js")))
    (is (string/includes? body "/static/llar-status.js?v=dashboard-tables-1"))
    (is (string/includes? javascript "DataTable.isDataTable"))
    (is (string/includes? javascript "new DataTable"))
    (is (not (re-find #"[.$]DataTable\(" javascript)))))

(deftest source-staleness-follows-the-advertised-schedule-deadline
  (let [now (time/zoned-date-time 2026 8 8 12 0 0 0 "UTC")
        expected (time/minus now (time/hours 2))
        schedule {:sched-name "hourly"
                  :expected-next-run-at expected
                  :expected-interval (time/hours 1)}]
    (testing "a success predating a missed run is stale after one interval of grace"
      (is (#'uut/source-stale? now {:status :ok
                                    :last-success (time/minus now (time/hours 3))
                                    :schedules [schedule]})))
    (testing "a source updated after that advertised run is still fresh"
      (is (not (#'uut/source-stale? now {:status :ok
                                         :last-success (time/minus now (time/minutes 30))
                                         :schedules [schedule]}))))
    (testing "failures retain their more specific dashboard classification"
      (is (not (#'uut/source-stale? now {:status :temp-fail
                                         :last-success (time/minus now (time/hours 3))
                                         :schedules [schedule]}))))
    (testing "the schedule gets a full expected interval of grace"
      (is (not (#'uut/source-stale? now {:status :ok
                                         :last-success (time/minus now (time/hours 3))
                                         :schedules [(assoc schedule
                                                            :expected-next-run-at
                                                            (time/minus now (time/minutes 30)))]}))))))

(deftest overview-distinguishes-scheduled-new-and-unscheduled-sources
  (let [now (time/zoned-date-time 2026 8 8 12 0 0 0 "UTC")
        schedule (sched/make-schedule
                  {:sched-name "hourly"
                   :sched-type :update-feed-by-filter
                   :source-keys-fn (constantly [:awaiting :also-awaiting])
                   :run-fn (constantly nil)})]
    (swap! (:state* schedule) assoc
           :expected-next-run-at (time/plus now (time/minutes 30))
           :expected-interval (time/hours 1))
    (with-redefs [time/zoned-date-time (constantly now)
                  config/get-sources (constantly {:awaiting {:src :one}
                                                  :also-awaiting {:src :three}
                                                  :orphan {:src :two}
                                                  :also-orphan {:src :four}
                                                  :bookmark {:tags #{:bookmark}}})
                  uut/get-state (constantly nil)
                  sched/find-schedules (constantly [schedule])]
      (let [body (str (h/html (uut/overview-tab)))]
        (is (string/includes? body "2 Unscheduled"))
        (is (string/includes? body "No matching fetch schedule"))
        (is (string/includes? body "hourly"))
        (is (string/includes? body "Expected in"))
        (is (string/includes? body "list-unstyled d-flex flex-wrap"))
        (is (string/includes? body "dashboard-overview-grid"))
        (is (string/includes? body "table-responsive dashboard-overview-table"))))))

(deftest run-schedule-route-handles-resolution-results
  (testing "unknown schedule"
    (with-redefs [sched/find-schedule (constantly {:error :not-found})]
      (is (= 404 (:status (uut/run-schedule "missing"))))))

  (testing "ambiguous schedule"
    (with-redefs [sched/find-schedule (constantly {:error :ambiguous
                                                   :matches [{:key :x}]})]
      (is (= 409 (:status (uut/run-schedule "x"))))))

  (testing "known schedule"
    (with-redefs [sched/find-schedule (constantly {:schedule :schedule})
                  sched/trigger-schedule! (constantly :triggered)]
      (let [response (uut/app {:request-method :post
                               :uri "/api/schedule/daily/run"})]
        (is (= 200 (:status response)))
        (is (= :triggered (get-in response [:body :result])))))))

(deftest podcast-tab-offers-untag-action
  (reset! podcast/download-state
          {42 {:status :perm-failed
               :media-url "https://example.com/episode"
               :metadata {:duration 90}
               :error "video unavailable"}})
  (try
    (with-redefs [podcast/podcast-disk-stats (constantly nil)
                  podcast-api/format-duration (constantly nil)]
      (let [body (str (h/html (uut/podcast-tab)))]
        (is (string/includes? body "Perm Failed"))
        (is (string/includes? body "id=\"podcasts-datatable\""))
        (is (string/includes? body "data-order=\"90\""))
        (is (string/includes? body ">Untag<"))
        (is (string/includes? body "/api/podcast/42/tag"))))
    (finally
      (reset! podcast/download-state {}))))

(deftest tiny-dashboard-summaries-do-not-render-datatables
  (testing "memory"
    (let [body (str (uut/memory-tab))]
      (is (string/includes? body "Runtime Total"))
      (is (not (string/includes? body "datatable")))))
  (testing "word count groups"
    (with-redefs [persistency/get-word-count-groups (constantly [[0 2]])
                  persistency/get-tag-stats (constantly [])
                  persistency/get-type-stats (constantly [])]
      (let [body (str (uut/database-tab))]
        (is (string/includes? body
                              "<h5>Word Count Groups</h5><table class=\"table\">"))))))

(deftest podcast-untag-removes-tag-and-tracking-state
  (reset! podcast/download-state
          {42 {:status :perm-failed
               :media-url "https://example.com/episode"}})
  (let [removed (atom nil)]
    (try
      (with-redefs [persistency/item-remove-tags!
                    (fn [_ item-id tags]
                      (reset! removed [item-id tags]))]
        (let [response (uut/podcast-untag "42")]
          (is (= 200 (:status response)))
          (is (= 42 (get-in response [:body :item-id])))
          (is (true? (get-in response [:body :untagged])))
          (is (= [42 [:podcast]] @removed))
          (is (not (contains? @podcast/download-state 42)))))
      (finally
        (reset! podcast/download-state {})))))

(deftest podcast-untag-keeps-blob-but-removes-unique-index-entry
  (reset! podcast/download-state
          {42 {:status :complete
               :blob-hash "blob-hash"}})
  (let [removed-index (atom nil)]
    (try
      (with-redefs [persistency/item-remove-tags! (fn [& _])
                    podcast/remove-from-podcast-index! (fn [blob-hash]
                                                         (reset! removed-index blob-hash))]
        (let [response (uut/podcast-untag "42")]
          (is (= 200 (:status response)))
          (is (true? (get-in response [:body :blob-kept])))
          (is (true? (get-in response [:body :index-removed])))
          (is (= "blob-hash" @removed-index))
          (is (not (contains? @podcast/download-state 42)))))
      (finally
        (reset! podcast/download-state {})))))

(deftest podcast-untag-route-handles-conflict-and-missing-item
  (try
    (reset! podcast/download-state {42 {:status :downloading}})
    (let [response (uut/app {:request-method :delete
                             :uri "/api/podcast/42/tag"})]
      (is (= 409 (:status response)))
      (is (= :download-in-progress (get-in response [:body :error])))
      (is (contains? @podcast/download-state 42)))

    (reset! podcast/download-state {})
    (is (= 404 (:status (uut/app {:request-method :delete
                                  :uri "/api/podcast/99/tag"}))))
    (finally
      (reset! podcast/download-state {}))))

(deftest update-source-defers-to-single-flight-admission
  ;; the old guard only knew about dashboard-initiated futures, so clicking update
  ;; during a scheduled fetch queued a duplicate forced update
  (with-redefs [config/get-sources (constantly {:held {:src :a-source}})
                update/in-flight-sources (constantly #{:held})]
    (let [{:keys [status body]} (uut/update-source "held" false)]
      (is (= 200 status))
      (is (= :already-updating (:status body)))))
  (with-redefs [config/get-sources (constantly {:held {:src :a-source}})]
    (is (= 404 (:status (uut/update-source "nope" false))))))

(deftest source-status-survives-a-source-with-no-dashboard-future
  ;; reachable as soon as update-source reports :already-updating for a scheduled run:
  ;; there is no future to poll, and future-done? would NPE on nil
  (with-redefs [uut/get-state (constantly {:status :ok})
                config/get-sources (constantly {:held {:src :a-source}})
                update/in-flight-sources (constantly #{:held})]
    (let [{:keys [status body]} (uut/source-status "held")]
      (is (= 200 status))
      (is (false? (get-in body [:update-status :done])))))
  (with-redefs [uut/get-state (constantly {:status :ok})
                config/get-sources (constantly {:idle {:src :a-source}})
                update/in-flight-sources (constantly #{})]
    (is (true? (get-in (uut/source-status "idle") [:body :update-status :done])))))

(def ^:private +activity-samples+
  {"llar_resource_in_use" [{:labels {"resource" "item_postproc" "kind" "executor"} :value 32.0}
                           {:labels {"resource" "streaming" "kind" "throttle"} :value 0.0}]
   "llar_resource_limit" [{:labels {"resource" "item_postproc" "kind" "executor"} :value 32.0}
                          {:labels {"resource" "streaming" "kind" "throttle"} :value 1.0}]
   "llar_resource_queued" [{:labels {"resource" "item_postproc" "kind" "executor"} :value 7.0}
                           {:labels {"resource" "streaming" "kind" "throttle"} :value 0.0}]
   "llar_resource_wait_seconds_count" [{:labels {"resource" "streaming"} :value 4.0}]
   "llar_resource_wait_seconds_sum" [{:labels {"resource" "streaming"} :value 2.0}]
   "hikaricp_active_connections" [{:labels {"pool" "backend"} :value 3.0}]
   "hikaricp_max_connections" [{:labels {"pool" "backend"} :value 10.0}]
   "hikaricp_pending_threads" [{:labels {"pool" "backend"} :value 0.0}]})

(deftest meter-rows-describes-every-bounded-resource
  (let [by-name (into {} (map (juxt :name identity)) (#'uut/meter-rows +activity-samples+))]
    (is (= #{"item_postproc" "streaming" "backend"} (set (keys by-name))))
    (is (= {:in-use 32.0 :limit 32.0 :queued 7.0} (select-keys (by-name "item_postproc")
                                                               [:in-use :limit :queued])))
    ;; hikaricp uses different metric names and a different label, same row shape
    (is (= {:in-use 3.0 :limit 10.0 :queued 0.0} (select-keys (by-name "backend")
                                                              [:in-use :limit :queued])))
    (is (= 0.5 (:wait-mean (by-name "streaming"))) "mean wait is sum/count")
    ;; every row needs a kind, but only llar_resource_* carries one as a label -
    ;; hikaricp samples are labelled by pool alone
    (is (= {"item_postproc" "executor"
            "streaming" "throttle"
            "backend" "db-pool"}
           (into {} (map (juxt :name :kind)) (vals by-name))))))

(deftest meter-rows-flags-and-promotes-saturated-resources
  (let [rows (#'uut/meter-rows +activity-samples+)
        by-name (into {} (map (juxt :name identity)) rows)]
    (is (true? (:saturated? (by-name "item_postproc"))) "32 of 32 is pinned")
    (is (false? (:saturated? (by-name "streaming"))))
    (is (false? (:saturated? (by-name "backend"))))
    ;; a pinned resource is the thing you came to the page to find
    (is (= "item_postproc" (:name (first (#'uut/sort-meters rows)))))))

(deftest threads-are-bucketed-by-name
  (is (= "Work pools" (#'uut/thread-bucket "llar-item-postproc-3")))
  (is (= "HTTP server" (#'uut/thread-bucket "qtp1234567-42")))
  (is (= "Schedulers" (#'uut/thread-bucket "chime-7")))
  (is (= "Database" (#'uut/thread-bucket "HikariPool-1 housekeeper")))
  (is (= "JVM & other" (#'uut/thread-bucket "Reference Handler"))))

(deftest thread-rows-show-the-complete-top-frame
  (let [frame (StackTraceElement. "sun.nio.ch.Net" "poll" "Net.java" -2)
        stack (into-array StackTraceElement [frame])
        html (str (h/html (#'uut/thread-rows {(Thread/currentThread) stack})))]
    (is (string/includes? html "thread-top-frame"))
    (is (string/includes? html "thread-top-frame-code"))
    (is (string/includes? html "sun.nio.ch.Net.poll(Native Method)"))
    (is (not (string/includes? html "sun.nio.ch.Net.poll…")))))

(deftest activity-tab-renders-saturation-inflight-and-census
  (let [work-id (work/register! {:kind :source :source :a-stuck-feed
                                 :stage :store :waiting-on :av-download})]
    (resources/register! resources/resources :pinned-pool :executor
                         (constantly {:in-use 4 :limit 4 :queued 9}))
    (try
      (let [html (str (h/html (uut/activity-tab)))]
        ;; saturation strip, driven by whatever happens to be registered
        (is (string/includes? html "pinned_pool"))
        (is (string/includes? html "table-danger") "a pinned resource is highlighted")
        ;; in-flight detail - exactly what prometheus deliberately does not carry
        (is (string/includes? html "a-stuck-feed"))
        (is (string/includes? html "store"))
        (is (string/includes? html "av-download"))
        ;; the raw dump is demoted but keeps the id the stacktrace handler binds to
        (is (string/includes? html "All threads and stacks"))
        (is (string/includes? html "threads-datatable")))
      (finally
        (work/unregister! work-id)
        (resources/unregister! resources/resources :pinned-pool)))))
