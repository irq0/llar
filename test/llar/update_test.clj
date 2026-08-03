(ns llar.update-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.config :as config]
   [llar.fetch.feed]
   [llar.http :as http]
   [llar.src :as src]
   [llar.update :as uut]
   [slingshot.slingshot :refer [throw+]]))

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
