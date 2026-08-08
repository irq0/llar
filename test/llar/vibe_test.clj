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
      (is (> (:match-score cross-source) 0.15))
      (is (some #{"election"} (:terms cross-source))))))

(deftest selection-prefers-strong-cross-source-stories-and-has-a-hard-budget
  (let [now (time/zoned-date-time)
        cluster (fn [id source-count article-count match-score age-minutes]
                  {:id id
                   :source-count source-count
                   :article-count article-count
                   :unseen-count article-count
                   :match-score match-score
                   :latest-ts (time/minus now (time/minutes age-minutes))})
        clusters [(cluster 1 3 3 0.8 30)
                  (cluster 2 2 2 0.3 10)
                  (cluster 3 2 2 0.1 5)
                  (cluster 4 1 1 nil 1)
                  (cluster 5 1 1 nil 20)]]
    (with-redefs [rc/rc (fn [& _]
                          (assoc settings
                                 :min-match-score 0.15
                                 :max-clusters 3
                                 :max-single-source-clusters 1))]
      (let [selection (vibe/select-clusters clusters)]
        (is (= [1 2] (mapv :id (:multi-source selection))))
        (is (= [4] (mapv :id (:other selection))))
        (is (= 3 (:shown-count selection)))
        (is (= 5 (:total-count selection)))))))

(deftest very-common-features-do-not-drive-clustering
  (let [now (time/zoned-date-time)
        items (mapv (fn [id]
                      {:id id
                       :title (str "Unique headline " id)
                       :top-words {:words (cond-> [["news" 1.0]]
                                            (< id 2) (conj ["election" 1.0]))}
                       :names []
                       :nouns []
                       :urls []
                       :ts now})
                    (range 10))]
    (with-redefs [rc/rc (fn [& _]
                          (assoc settings :max-feature-frequency-ratio 0.2))]
      (let [features (#'vibe/weighted-features items)]
        (is (not-any? #(contains? % "term:news") features))
        (is (every? #(contains? % "term:election") (take 2 features)))))))

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
