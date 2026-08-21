(ns llar.db.bookmark-capture-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [java-time.api :as time]
   [llar.db.bookmark-capture :as capture-db]
   [llar.db.test-fixtures :refer [*test-db* create-test-item
                                  with-clean-db-fixture with-test-db-fixture]]
   [llar.persistency :as persistency]
   [next.jdbc :as jdbc]))

(use-fixtures :once with-test-db-fixture)
(use-fixtures :each with-clean-db-fixture)

(defn- enqueue!
  ([fingerprint]
   (enqueue! fingerprint {}))
  ([fingerprint overrides]
   (capture-db/enqueue!
    *test-db*
    (merge {:url (str "https://example.com/" fingerprint)
            :url-fingerprint fingerprint
            :title "Captured title"
            :submitted-by "test"}
           overrides))))

(deftest enqueue-is-durable-and-does-not-mutate-duplicates
  (let [fingerprint "SHA-256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        first-capture (enqueue! fingerprint)
        duplicate (enqueue! fingerprint {:title "Replacement title"})]
    (is (true? (:inserted first-capture)))
    (is (false? (:inserted duplicate)))
    (is (= (:id first-capture) (:id duplicate)))
    (is (= "Captured title" (:title duplicate)))
    (is (= :pending (:status duplicate)))
    (is (some? (:created-ts duplicate)))))

(deftest claims-are-single-owner-and-completable
  (let [one (enqueue! "SHA-256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
        two (enqueue! "SHA-256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")
        claimed-one (capture-db/claim-next! *test-db* 1200)
        claimed-two (capture-db/claim-next! *test-db* 1200)]
    (is (= #{(:id one) (:id two)} #{(:id claimed-one) (:id claimed-two)}))
    (is (every? #(= :processing (:status %)) [claimed-one claimed-two]))
    (is (= 1 (:attempt-count claimed-one)))
    (is (nil? (capture-db/claim-next! *test-db* 1200)))
    (let [item (:id (create-test-item *test-db* :hash "capture-complete"))
          completed (capture-db/complete! *test-db* (:id claimed-one)
                                          (:lease-version claimed-one) item)]
      (is (= :complete (:status completed)))
      (is (= item (:item-id completed)))
      (is (some? (:completed-ts completed))))))

(deftest expired-leases-are-reclaimed
  (let [capture (enqueue! "SHA-256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd")
        claimed (capture-db/claim-next! *test-db* 1200)]
    (jdbc/execute! *test-db*
                   ["UPDATE bookmark_capture_queue SET lease_expires_ts = now() - interval '1 minute' WHERE id = ?"
                    (:id capture)])
    (let [reclaimed (capture-db/claim-next! *test-db* 1200)]
      (is (= (:id claimed) (:id reclaimed)))
      (is (= 2 (:attempt-count reclaimed)))
      (is (> (:lease-version reclaimed) (:lease-version claimed)))
      (is (nil? (capture-db/fail! *test-db* (:id claimed)
                                  (:lease-version claimed) "stale" "stale owner")))
      (is (= :processing (:status (capture-db/by-id *test-db* (:id capture))))))))

(deftest retry-and-dismiss-are-explicit-dashboard-actions
  (let [capture (enqueue! "SHA-256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")
        claimed (capture-db/claim-next! *test-db* 1200)
        failed (capture-db/fail! *test-db* (:id claimed) (:lease-version claimed)
                                 "fetch" "network failed")]
    (is (= :failed (:status failed)))
    (is (= "network failed" (:last-error failed)))
    (let [retried (capture-db/retry! *test-db* (:id capture))]
      (is (= :pending (:status retried)))
      (is (zero? (:attempt-count retried)))
      (is (= :dismissed (:status (capture-db/dismiss! *test-db* (:id capture))))))))

(deftest dashboard-capture-timestamps-preserve-the-database-instant
  (enqueue! "SHA-256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
  (capture-db/claim-next! *test-db* 1200)
  (let [last-attempt (:last-attempt-ts (first (capture-db/list-captures *test-db* 1)))
        now (time/zoned-date-time)]
    (is (time/after? last-attempt (time/minus now (time/minutes 1))))
    (is (time/before? last-attempt (time/plus now (time/minutes 1))))))

(deftest reader-activity-is-reader-scoped-recent-and-bounded
  (let [completed-capture (enqueue!
                           "SHA-256:1111111111111111111111111111111111111111111111111111111111111111"
                           {:submitted-by "reader"
                            :url "https://example.com/ready"})
        completed-claim (capture-db/claim-next! *test-db* 1200)
        item-id (:id (create-test-item *test-db*
                                       :hash "reader-activity-ready"
                                       :title "Ready title"))]
    (capture-db/complete! *test-db* (:id completed-capture)
                          (:lease-version completed-claim) item-id)
    (let [failed-capture (enqueue!
                          "SHA-256:2222222222222222222222222222222222222222222222222222222222222222"
                          {:submitted-by "reader"})
          failed-claim (capture-db/claim-next! *test-db* 1200)]
      (capture-db/fail! *test-db* (:id failed-capture)
                        (:lease-version failed-claim) "fetch" "no response"))
    (enqueue! "SHA-256:3333333333333333333333333333333333333333333333333333333333333333"
              {:submitted-by "reader"})
    (enqueue! "SHA-256:4444444444444444444444444444444444444444444444444444444444444444"
              {:submitted-by "external"})

    (is (= {:active 1 :recent-complete 1 :recent-failed 1}
           (capture-db/reader-activity-counts *test-db*)))
    (is (= [{:id (:id completed-capture)
             :item-id item-id
             :url "https://example.com/ready"
             :item-title "Ready title"}]
           (mapv #(select-keys % [:id :item-id :url :item-title])
                 (capture-db/reader-recent-complete *test-db* 3))))

    (jdbc/execute! *test-db*
                   ["UPDATE bookmark_capture_queue
                     SET completed_ts = now() - interval '61 minutes',
                         updated_ts = now() - interval '61 minutes'
                     WHERE submitted_by = 'reader' AND status IN ('complete', 'failed')"])
    (is (= {:active 1 :recent-complete 0 :recent-failed 0}
           (capture-db/reader-activity-counts *test-db*)))
    (is (empty? (capture-db/reader-recent-complete *test-db* 3)))))

(deftest reading-queue-has-no-bookmark-only-membership
  (let [unread-bookmark (:id (create-test-item *test-db*
                                               :hash "unread-bookmark-only"
                                               :type :item-type/bookmark
                                               :tags #{:unread}))
        saved-bookmark (:id (create-test-item *test-db*
                                              :hash "saved-bookmark"
                                              :type :item-type/bookmark
                                              :tags #{:unread :saved}))
        queue-ids (set (map :id (persistency/get-reading-queue-items *test-db* {})))]
    (is (not (contains? queue-ids unread-bookmark)))
    (is (contains? queue-ids saved-bookmark))))
