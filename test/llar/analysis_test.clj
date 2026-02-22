(ns llar.analysis-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llar.analysis :as uut]))

(def +english-text+
  "The quick brown fox jumps over the lazy dog. Scientists discovered a new species of bird in the Amazon rainforest. The president announced new economic policies yesterday.")

(def +german-text+
  "Der schnelle braune Fuchs springt über den faulen Hund. Wissenschaftler entdeckten eine neue Vogelart im Amazonas-Regenwald. Der Präsident kündigte gestern neue Wirtschaftspolitiken an.")

(def +short-text+
  "Hello world.")

(deftest detect-language-test
  (testing "detects English"
    (is (= "en" (uut/detect-language +english-text+))))
  (testing "detects German"
    (is (= "de" (uut/detect-language +german-text+)))))

(deftest find-best-analysis-language-test
  (testing "explicit :en language is honored"
    (is (= :en (uut/find-best-analysis-language {:text +english-text+ :lang :en}))))
  (testing "explicit :de language is honored"
    (is (= :de (uut/find-best-analysis-language {:text +german-text+ :lang :de}))))
  (testing "short text defaults to :en"
    (is (= :en (uut/find-best-analysis-language {:text +short-text+ :lang nil}))))
  (testing "long English text detected as :en"
    (is (= :en (uut/find-best-analysis-language {:text +english-text+ :lang nil})))))

(deftest analyze-text-test
  (testing "returns map with required keys"
    (let [result (uut/analyze-text +english-text+)]
      (is (map? result))
      (is (contains? result :language))
      (is (contains? result :nlp))))

  (testing "nlp map has required sub-keys"
    (let [{:keys [nlp]} (uut/analyze-text +english-text+)]
      (is (contains? nlp :nwords))
      (is (contains? nlp :top))
      (is (contains? nlp :nouns))
      (is (contains? nlp :verbs))
      (is (contains? nlp :names))
      (is (contains? nlp :urls))))

  (testing "English text produces non-empty nouns"
    (let [{:keys [nlp]} (uut/analyze-text +english-text+)]
      (is (seq (:nouns nlp)))))

  (testing "word count is positive"
    (let [{:keys [nlp]} (uut/analyze-text +english-text+)]
      (is (pos? (:nwords nlp)))))

  (testing "language detected correctly for English"
    (let [{:keys [language]} (uut/analyze-text +english-text+)]
      (is (= :en language))))

  (testing "top words are present"
    (let [{{:keys [words]} :top} (:nlp (uut/analyze-text +english-text+))]
      (is (seq words)))))

(deftest extract-urls-test
  (testing "extracts URLs from text"
    (let [text "Visit https://example.com and http://test.org/path for more info"]
      (is (= #{"https://example.com" "http://test.org/path"}
             (set (uut/extract-urls text))))))
  (testing "returns nil/empty for text without URLs"
    (is (nil? (seq (uut/extract-urls "No URLs here at all."))))))

(deftest remove-urls-test
  (testing "removes URLs from text"
    (let [text "Visit https://example.com for info"]
      (is (not (.contains (uut/remove-urls text) "https://example.com")))))
  (testing "non-URL text unchanged"
    (is (= "hello world" (uut/remove-urls "hello world")))))

(deftest analyze-entry-test
  (testing "returns nil for blank text"
    (is (nil? (uut/analyze-entry {:contents {"text/plain" ""}})))
    (is (nil? (uut/analyze-entry {:contents {}}))))
  (testing "returns analysis for non-blank text"
    (let [entry {:contents {"text/plain" +english-text+}}]
      (is (some? (uut/analyze-entry entry))))))

(deftest sanitize-text-test
  (testing "removes URLs"
    (is (not (.contains (uut/sanitize-text "See https://example.com for details") "https"))))
  (testing "fixes smart quotes"
    (is (.contains (uut/sanitize-text "\u201chello\u201d") "\"")))
  (testing "removes reference-style links"
    (is (not (.contains (uut/sanitize-text "See [1]: http://example.com") "[1]:")))))
