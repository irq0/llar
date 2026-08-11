(ns llar.fetch.demo-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [java-time.api :as time]
   [llar.fetch :as fetch]
   [llar.fetch.demo :as demo]
   [llar.persistency :as persistency]
   [llar.postproc :as postproc]
   [llar.rc :as rc]
   [llar.src :as src]
   [llar.vibe :as vibe]))

(def fixed-now
  (time/zoned-date-time 2026 8 11 12 0 0 0 "UTC"))

(defn- fetched-items [publication & {:as args}]
  (with-redefs [demo/now (constantly fixed-now)]
    (fetch/fetch-source (apply src/demo publication (mapcat identity args)) {})))

(defn- stable-item-view [item]
  {:summary (:summary item)
   :hash (:hash item)
   :tags (get-in item [:meta :tags])
   :entry (:entry item)})

(defn- vibe-item [id source-key item]
  (let [nlp (get-in item [:entry :nlp])]
    {:id id
     :source-key (name source-key)
     :title (get-in item [:summary :title])
     :ts (get-in item [:summary :ts])
     :tags (mapv name (get-in item [:meta :tags]))
     :top-words {"words" (get-in nlp [:top :words])}
     :names (:names nlp)
     :nouns (:nouns nlp)
     :urls (:urls nlp)}))

(deftest demo-source-is-seeded-and-network-free
  (let [first-run (fetched-items :signal-wire :count 3 :seed 17)
        second-run (fetched-items :signal-wire :count 3 :seed 17)]
    (is (= 3 (count first-run)))
    (is (= (mapv stable-item-view first-run)
           (mapv stable-item-view second-run)))
    (is (every? #(re-find #"^https://demo\.llar\.dev/" (get-in % [:entry :url]))
                first-run))
    (is (every? #(re-find #"^/static/demo/.+\.svg$"
                          (get-in % [:entry :lead-image-url]))
                first-run))
    (is (every? #(contains? (get-in % [:entry :contents]) "text/html")
                first-run))
    (is (every? #(contains? (get-in % [:entry :contents]) "text/plain")
                first-run))))

(deftest publications-cover-the-same-stories-from-different-angles
  (let [items (mapv #(first (fetched-items % :count 1 :seed 17))
                    src/demo-publications)]
    (is (= 4 (count (set (map #(get-in % [:summary :title]) items)))))
    (is (= #{:demo :local-first}
           (set (get-in (first items) [:meta :tags]))))
    (is (= 1 (count (set (map #(get-in % [:entry :lead-image-url]) items)))))))

(deftest demo-items-exercise-reader-workflow-states
  (let [source (src/demo :field-notes)
        raw-items (with-redefs [demo/now (constantly fixed-now)]
                    (fetch/fetch-source source {}))
        processed (mapv #(-> %
                             (postproc/all-items-process-first source {:key :field-notes})
                             (postproc/post-process-item source {}))
                        raw-items)
        archived (filter #(contains? (get-in % [:meta :tags]) :archive) processed)]
    (testing "recent items remain unread and include saved/checkpoint examples"
      (is (contains? (get-in processed [0 :meta :tags]) :unread))
      (is (every? #(contains? (get-in processed [% :meta :tags]) :saved) [2 4]))
      (is (contains? (get-in processed [2 :meta :tags]) :in-progress)))
    (testing "archived fixtures do not retain an incompatible unread state"
      (is (= 2 (count archived)))
      (is (every? #(not (contains? (get-in % [:meta :tags]) :unread)) archived)))
    (testing "stored demo items use the ordinary reader link type"
      (is (every? #(= :link (:type (persistency/to-couch %))) processed)))
    (testing "NLP metadata is present for search, ranking, and Today’s Vibe"
      (is (every? pos? (map #(get-in % [:entry :nlp :nwords]) processed))))))

(deftest demo-stories-produce-cross-source-vibe-clusters
  (let [settings {:acuity 2.0
                  :cutoff 0.002
                  :random-seed 1
                  :max-feature-frequency-ratio 0.2}
        items (->> src/demo-publications
                   sort
                   (mapcat (fn [source-key]
                             (let [source (src/demo source-key)
                                   fetched (with-redefs [demo/now (constantly fixed-now)]
                                             (fetch/fetch-source source {}))]
                               (map #(-> %
                                         (postproc/all-items-process-first source {:key source-key})
                                         (postproc/post-process-item source {}))
                                    fetched))))
                   (map-indexed #(vibe-item (inc %1)
                                            (get-in %2 [:meta :source-key])
                                            %2))
                   (sort-by (juxt :ts :id))
                   vec)]
    (with-redefs [rc/rc (fn [& _] settings)]
      (let [clusters (vibe/cluster-items items)
            selected (:multi-source (vibe/select-clusters clusters))]
        (is (some #(>= (:source-count %) 2) selected))))))

(deftest demo-source-validates-its-small-bounded-corpus
  (is (thrown? AssertionError (src/demo :unknown-publication)))
  (is (thrown? AssertionError (src/demo :signal-wire :count 0)))
  (is (thrown? AssertionError (src/demo :signal-wire :count 9))))
