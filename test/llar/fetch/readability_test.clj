(ns llar.fetch.readability-test
  (:require
   [clojure.test :refer [deftest is]]
   [llar.analysis :as analysis]
   [llar.fetch.readability :as readability]
   [llar.postproc :as postproc]))

(deftest post-process-tags-analyzed-video-urls
  (let [item (readability/->ReadabilityItem
              {:tags #{}}
              {}
              nil
              {:url "https://example.com/article"})
        processed (with-redefs [analysis/analyze-entry
                                (constantly
                                 {:language :en
                                  :nlp {:urls #{"https://www.youtube.com/watch?v=123"}}})]
                    (postproc/post-process-item item nil nil))]
    (is (contains? (get-in processed [:meta :tags]) :has-video))
    (is (= #{"https://www.youtube.com/watch?v=123"}
           (get-in processed [:entry :nlp :urls])))))
