(ns llar.vibe-test
  (:require
   [clojure.test :refer [deftest is]]
   [java-time.api :as time]
   [llar.rc :as rc]
   [llar.vibe :as vibe]))

(def settings
  {:acuity 1.0 :cutoff 0.002 :random-seed 1})

(defn- story-item [id source title]
  {:id id
   :source-key source
   :title title
   :ts (time/zoned-date-time)
   :tags ["unread"]
   :top-words {:words [["election" 2.0]]}
   :names ["Berlin"]
   :nouns ["result"]
   :urls []})

(deftest cobweb-groups-overlapping-reports
  (with-redefs [rc/rc (fn [& _] settings)]
    (let [clusters (vibe/cluster-items
                    [(story-item 1 "source-a" "Berlin city election result")
                     (story-item 2 "source-b" "Berlin election results announced")
                     {:id 3
                      :source-key "source-c"
                      :title "Programming language release"
                      :ts (time/zoned-date-time)
                      :tags []
                      :top-words {:words [["programming" 2.0]]}
                      :names [] :nouns ["release"] :urls []}])
          cross-source (some #(when (= 2 (:source-count %)) %) clusters)]
      (is (= 2 (count clusters)))
      (is (= 2 (:article-count cross-source)))
      (is (= 2 (:unseen-count cross-source)))
      (is (some #{"election"} (:terms cross-source))))))

(deftest stale-run-cannot-resolve-cluster
  (with-redefs [vibe/current-vibe
                (atom {:run-id "current" :clusters [{:id 4 :items []}]})]
    (is (= 4 (:id (vibe/current-cluster "current" 4))))
    (is (nil? (vibe/current-cluster "stale" 4)))))

(deftest unsupported-items-never-form-a-false-multi-source-story
  (with-redefs [rc/rc (fn [& _] settings)]
    (let [now (time/zoned-date-time)
          clusters (vibe/cluster-items
                    [{:id 1 :source-key "a" :title "Alpha" :ts now :tags ["unread"]}
                     {:id 2 :source-key "b" :title "Beta" :ts now :tags ["unread"]}])]
      (is (= 2 (count clusters)))
      (is (every? #(= 1 (:source-count %)) clusters)))))
