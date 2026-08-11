(ns llar.lab-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.db.search :as db-search]
   [llar.lab :as lab]))

(deftest saved-dataset-coerces-integral-features-for-weka
  (let [features (set (map #(str "feature-" %) (range 42)))
        item (into {"item_id" 7.0}
                   (map #(vector % 0) features))]
    (with-redefs [db-search/saved-items-tf-idf-terms (fn [_ _] features)
                  db-search/saved-items-tf-idf (fn [_] [item])]
      (let [dataset (lab/make-saved-dataset ::db)
            instance (.instance dataset 0)]
        (is (= 1 (.numInstances dataset)))
        (is (= 7.0 (.value instance (.index (.attribute dataset "item_id")))))
        (is (every? zero?
                    (map #(.value instance (.index (.attribute dataset %)))
                         features)))))))
