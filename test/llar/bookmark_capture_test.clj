(ns llar.bookmark-capture-test
  (:require
   [clojure.test :refer [deftest is]]
   [java-time.api :as time]
   [llar.appconfig :as appconfig]
   [llar.bookmark-capture :as uut]
   [llar.db.bookmark-capture :as capture-db]
   [llar.fetch :as fetch]
   [llar.fetch.bookmark :as bookmark]
   [llar.persistency :as persistency]
   [llar.work :as work]))

(deftest processing-schedule-comes-from-static-appconfig
  (let [configured-schedule (ns-resolve 'llar.bookmark-capture 'configured-schedule)]
    (with-redefs [appconfig/capture (fn [_] :hourly)]
      (is (= :hourly (configured-schedule))))
    (with-redefs [appconfig/capture (constantly nil)]
      (is (= :now-and-every-minute (configured-schedule))))))

(deftest capture-url-normalization-is-bounded-and-deterministic
  (is (= "https://example.com/article?a=1#section"
         (uut/normalize-url
          "  https://example.com/article?utm_source=share&a=1&fbclid=x#section  ")))
  (doseq [url [nil "" "/relative" "ftp://example.com/file" "https:///missing-host"]]
    (is (thrown? clojure.lang.ExceptionInfo (uut/normalize-url url)))))

(deftest enqueue-persists-the-normalized-identity-and-audit-name
  (with-redefs [capture-db/enqueue! (fn [_ capture] capture)]
    (let [capture (uut/enqueue! :db
                                "https://example.com/story?utm_source=share&a=1"
                                "  Story  " "iphone")]
      (is (= "https://example.com/story?a=1" (:url capture)))
      (is (= (fetch/make-item-hash (:url capture)) (:url-fingerprint capture)))
      (is (= "Story" (:title capture)))
      (is (= "iphone" (:submitted-by capture)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (uut/enqueue! :db "https://example.com" 42 "iphone"))))

(deftest raw-capture-is-a-normal-bookmark-with-saved-unread-state
  (let [captured-ts (time/zoned-date-time 2026 8 9 12 0 0 0 "UTC")
        raw (bookmark/make-raw-bookmark
             "https://example.com/story" "Story" captured-ts)
        stored (persistency/to-couch raw)]
    (is (= #{:saved :unread} (get-in raw [:meta :tags])))
    (is (= :bookmark (:type stored)))
    (is (= (str captured-ts) (get-in stored [:entry :captured-ts])))
    (is (= "https://example.com/story" (get-in stored [:entry :url])))))

(deftest fetched-capture-keeps-the-original-url-identity
  (let [captured-ts (time/zoned-date-time)
        feed (bookmark/make-readability-bookmark-feed
              "https://example.com/original" captured-ts)
        post-fn (first (get-in feed [:proc :post]))
        item {:hash "SHA-256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
              :meta {:tags #{} :source-key :other :source-name "Other"}
              :entry {:url "https://example.com/redirected"}}
        processed (post-fn item)]
    (is (= (bookmark/bookmark-identity "https://example.com/original")
           (select-keys
            {:url "https://example.com/original"
             :hash (:hash processed)
             :source-key (get-in processed [:meta :source-key])
             :source-name (get-in processed [:meta :source-name])}
            [:url :hash :source-key :source-name])))
    (is (= #{:saved :unread} (get-in processed [:meta :tags])))))

(deftest processing-completes-only-with-the-claimed-lease-version
  (let [completed (atom nil)
        capture {:id 7 :url "https://example.com" :submitted-by "test"
                 :attempt-count 1 :lease-version 12}]
    (with-redefs-fn
      {(ns-resolve 'llar.bookmark-capture 'ensure-raw-item!) (constantly 42)
       (ns-resolve 'llar.bookmark-capture 'item-with-content) (constantly {:content true})
       #'capture-db/complete! (fn [& args]
                                (reset! completed args)
                                {:status :complete})}
      #(is (= {:status :complete} (uut/process-capture! :db capture))))
    (is (= [:db 7 12 42] @completed))
    (is (empty? (work/in-flight)))))

(deftest processing-failure-uses-fixed-backoff-and-the-claimed-lease-version
  (let [rescheduled (atom nil)
        capture {:id 8 :url "https://example.com" :submitted-by "test"
                 :attempt-count 1 :lease-version 13}]
    (with-redefs-fn
      {(ns-resolve 'llar.bookmark-capture 'ensure-raw-item!)
       (fn [& _] (throw (ex-info "fetch failed" {:reason-class :fetch})))
       #'capture-db/reschedule! (fn [& args]
                                  (reset! rescheduled args)
                                  {:status :pending})}
      #(is (= {:status :pending} (uut/process-capture! :db capture))))
    (is (= [:db 8 13 60 "fetch" "fetch failed"] @rescheduled))
    (is (empty? (work/in-flight)))))
