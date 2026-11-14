(ns llar.reprocess-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as time]
            [llar.config :as config]
            [llar.persistency :as persistency]
            [llar.pool :as pool]
            [llar.update :as uut]))

(use-fixtures :each
  (fn [f]
    (uut/reset-in-flight!)
    (reset! uut/reprocess-state {:status :idle})
    (try (f) (finally (uut/reset-in-flight!)))))

(def stored
  {:id 7 :hash "same-hash" :source-key "newsletter" :type :item-type/mail
   :title "Newsletter" :ts (time/zoned-date-time) :tags ["saved" "has-annotations"]
   :nwords 2 :names ["Ada"] :top-words {:words {"article" 1}}
   :entry {:authors ["Newsletter author"]}
   :data {:source-snapshot {"text/html" "<p>Original email</p>"}
          :content {"text/html" "<p>Old extraction</p>"}}})

(deftest reprocessing-uses-stored-content-for-every-item-type
  (doseq [type [:item-type/mail :item-type/feed :item-type/website :item-type/bookmark]]
    (let [writes (atom [])
          processor (fn [item]
                      (is (= (get-in stored [:data :source-snapshot]) (get-in item [:entry :source-snapshot])))
                      (is (= (get-in stored [:data :content]) (get-in item [:entry :contents])))
                      (is (= 2 (get-in item [:entry :nlp :nwords])))
                      (assoc-in item [:entry :contents "text/html"] "<p>New extraction</p>"))]
      (with-redefs [pool/call-on (fn [_ f] (f))
                    persistency/get-item-by-id (fn [_ id] (is (= 7 id)) (assoc stored :type type))
                    config/get-source (fn [key] (is (= :newsletter key)) {:proc {:post [processor]}})
                    persistency/replace-item-content! (fn [_ id item] (swap! writes conj [id item]) {:id id})]
        (is (= {:id 7} (uut/reprocess-item! 7))))
      (let [[id item] (first @writes)]
        (is (= 1 (count @writes)))
        (is (= 7 id))
        (is (= "same-hash" (:hash item)))
        (is (= type (:type item)))
        (is (= #{:saved :has-annotations} (get-in item [:meta :tags])))
        (is (= "<p>New extraction</p>" (get-in item [:entry :contents "text/html"])))))))

(deftest failed-or-invalid-rules-never-write
  (with-redefs [pool/call-on (fn [_ f] (f))
                persistency/get-item-by-id (constantly stored)
                persistency/replace-item-content! (fn [& _] (throw (AssertionError. "Must not write")))]
    (doseq [post [[] [(constantly nil)] [(fn [item] [item])]
                  [#(assoc % :hash "another-item")]
                  [(fn [_] (throw (ex-info "Broken rule" {})))]]]
      (with-redefs [config/get-source (constantly {:proc {:post post}})]
        (is (thrown? clojure.lang.ExceptionInfo (uut/reprocess-item! 7)))))))

(deftest plan-is-bounded-and-validates-before-querying
  (with-redefs [config/get-source (constantly {:proc {:post [identity]}})
                persistency/get-reprocess-item-ids
                (fn [_ source since limit]
                  (is (= :newsletter source))
                  (is (time/before? since (time/minus (time/zoned-date-time) (time/days 29))))
                  (is (= 3 limit))
                  [3 2 1])]
    (is (= {:source-key :newsletter :days 30 :limit 2 :item-ids [3 2] :total 2 :limited? true}
           (uut/reprocess-plan :newsletter 30 2))))
  (with-redefs [persistency/get-reprocess-item-ids (fn [& _] (throw (AssertionError. "Must not query")))]
    (doseq [[days limit] [[0 100] [366 100] [30 0] [30 501] [nil 100] [30 1.5]]]
      (is (thrown? clojure.lang.ExceptionInfo (uut/reprocess-plan :newsletter days limit))))))

(deftest batch-holds-source-lock-and-stops-on-first-error
  (let [entered (promise) proceed (promise) finished (promise) seen (atom [])
        release @#'uut/release!]
    (with-redefs [uut/reprocess-plan (fn [source days limit]
                                       {:source-key source :days days :limit limit :item-ids [3 2 1] :total 3})
                  uut/reprocess-stored-item! (fn [id source-key]
                                               (is (= :newsletter source-key))
                                               (swap! seen conj id)
                                               (when (= id 3) (deliver entered true) @proceed)
                                               (when (= id 2) (throw (AssertionError. "Broken rule"))))
                  uut/release! (fn [source token] (release source token) (deliver finished true))]
      (try
        (is (= :running (:status (uut/start-reprocess! :newsletter 30 100))))
        (is (= true (deref entered 1000 :timeout)))
        (is (= #{:newsletter} (uut/in-flight-sources)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already running"
                              (uut/start-reprocess! :other 30 100)))
        (finally (deliver proceed true)))
      (is (= true (deref finished 1000 :timeout)))
      (is (= [3 2] @seen))
      (is (= {:status :failed :completed 1 :item-id 2}
             (select-keys @uut/reprocess-state [:status :completed :item-id])))
      (is (empty? (uut/in-flight-sources))))))

(deftest batch-does-not-overlap-an-existing-source-update
  (let [token (#'uut/reserve! :newsletter)]
    (try
      (with-redefs [uut/reprocess-plan (constantly {:item-ids [] :total 0})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"currently updating"
                              (uut/start-reprocess! :newsletter 30 100)))
        (is (= :idle (:status @uut/reprocess-state))))
      (finally (#'uut/release! :newsletter token)))))

(deftest reprocessing-protects-an-existing-source-snapshot
  (let [saved (assoc-in stored [:entry :source-snapshot-url] "https://example.com/captured")
        written (atom nil)]
    (with-redefs [pool/call-on (fn [_ f] (f))
                  persistency/get-item-by-id (constantly saved)
                  config/get-source (constantly {:proc {:post [#(update % :entry dissoc :source-snapshot :source-snapshot-url)]}})
                  persistency/replace-item-content! (fn [_ _ item] (reset! written item) {:id 7})]
      (uut/reprocess-item! 7)
      (is (= (get-in saved [:data :source-snapshot]) (get-in @written [:entry :source-snapshot])))
      (is (= "https://example.com/captured" (get-in @written [:entry :source-snapshot-url]))))))

(deftest single-item-reprocessing-refuses-a-reserved-source
  (let [token (#'uut/reserve! :newsletter)]
    (try
      (with-redefs [persistency/get-item-by-id (constantly stored)
                    pool/call-on (fn [& _] (throw (AssertionError. "Must not process while reserved")))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"currently updating"
                              (uut/reprocess-item! 7)))
        (is (= #{:newsletter} (uut/in-flight-sources))))
      (finally (#'uut/release! :newsletter token)))))

(deftest single-item-reprocessing-reloads-under-lock-and-releases-on-failure
  (let [reads (atom 0)
        written (atom nil)]
    (with-redefs [pool/call-on (fn [_ f] (f))
                  persistency/get-item-by-id
                  (fn [& _]
                    (if (= 1 (swap! reads inc))
                      stored
                      (do
                        (is (= #{:newsletter} (uut/in-flight-sources)))
                        (assoc stored :title "Newer capture"))))
                  config/get-source (constantly {:proc {:post [identity]}})
                  persistency/replace-item-content!
                  (fn [_ _ item]
                    (reset! written item)
                    (throw (ex-info "Write failed" {})))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Write failed"
                            (uut/reprocess-item! 7)))
      (is (= "Newer capture" (get-in @written [:summary :title])))
      (is (empty? (uut/in-flight-sources))))))

(deftest batch-processes-an-item-while-owning-the-source-reservation
  (let [finished (promise)
        writes (atom 0)
        release @#'uut/release!]
    (with-redefs [uut/reprocess-plan (constantly {:source-key :newsletter :item-ids [7] :total 1})
                  pool/call-on (fn [_ f] (f))
                  persistency/get-item-by-id (constantly stored)
                  config/get-source (constantly {:proc {:post [identity]}})
                  persistency/replace-item-content!
                  (fn [& _]
                    (is (= #{:newsletter} (uut/in-flight-sources)))
                    (swap! writes inc)
                    {:id 7})
                  uut/release! (fn [k token] (release k token) (deliver finished true))]
      (uut/start-reprocess! :newsletter 30 100)
      (is (= true (deref finished 1000 :timeout)))
      (is (= 1 @writes))
      (is (= :complete (:status @uut/reprocess-state)))
      (is (empty? (uut/in-flight-sources))))))

(deftest single-item-reprocessing-rejects-an-item-that-moves-sources
  (let [reads (atom 0)]
    (with-redefs [pool/call-on (fn [_ f] (f))
                  persistency/get-item-by-id (fn [& _]
                                               (if (= 1 (swap! reads inc)) stored
                                                   (assoc stored :source-key "other")))
                  config/get-source (constantly {:proc {:post [identity]}})
                  persistency/replace-item-content! (fn [& _] (throw (AssertionError. "Must not write")))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"source changed" (uut/reprocess-item! 7)))
      (is (empty? (uut/in-flight-sources))))))
