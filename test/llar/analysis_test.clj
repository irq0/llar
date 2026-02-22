(ns llar.analysis-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [llar.analysis :as uut]))

(def +texts+
  {:en "The quick brown fox jumps over the lazy dog. Scientists discovered a new species of bird in the Amazon rainforest. The president announced new economic policies yesterday."
   :de "Der schnelle braune Fuchs springt über den faulen Hund. Wissenschaftler entdeckten eine neue Vogelart im Amazonas-Regenwald. Der Präsident kündigte gestern neue Wirtschaftspolitiken an."})

(def +short-text+ "Hello world.")

(deftest detect-language-test
  (doseq [[lang text] +texts+]
    (testing (str "detects " lang)
      (is (= (name lang) (uut/detect-language text))))))

(deftest find-best-analysis-language-test
  (doseq [[lang text] +texts+]
    (testing (str "explicit " lang " language is honored")
      (is (= lang (uut/find-best-analysis-language {:text text :lang lang}))))
    (testing (str lang " text auto-detected without hint")
      (is (= lang (uut/find-best-analysis-language {:text text :lang nil})))))
  (testing "short text defaults to :en"
    (is (= :en (uut/find-best-analysis-language {:text +short-text+ :lang nil})))))

(deftest analyze-text-result-shape-test
  (doseq [[lang text] +texts+]
    (testing (str lang " result has required top-level keys")
      (let [result (uut/analyze-text text)]
        (is (map? result))
        (is (contains? result :language))
        (is (contains? result :nlp))))
    (testing (str lang " nlp map has required sub-keys")
      (let [{:keys [nlp]} (uut/analyze-text text)]
        (is (contains? nlp :nwords))
        (is (contains? nlp :top))
        (is (contains? nlp :nouns))
        (is (contains? nlp :verbs))
        (is (contains? nlp :names))
        (is (contains? nlp :urls))))))

(deftest analyze-text-content-test
  (doseq [[lang text] +texts+]
    (testing (str lang " produces non-empty nouns")
      (let [{:keys [nlp]} (uut/analyze-text text)]
        (is (seq (:nouns nlp)))))
    (testing (str lang " word count is positive")
      (let [{:keys [nlp]} (uut/analyze-text text)]
        (is (pos? (:nwords nlp)))))
    (testing (str lang " language field matches input language")
      (let [{:keys [language]} (uut/analyze-text text)]
        (is (= lang language))))
    (testing (str lang " top words are present")
      (let [{{:keys [words]} :top} (:nlp (uut/analyze-text text))]
        (is (seq words))))))

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
  (doseq [[lang text] +texts+]
    (testing (str "returns analysis for non-blank " lang " text")
      (let [entry {:contents {"text/plain" text}}]
        (is (some? (uut/analyze-entry entry)))))))

(deftest sanitize-text-test
  (testing "removes URLs"
    (is (not (.contains (uut/sanitize-text "See https://example.com for details") "https"))))
  (testing "fixes smart quotes"
    (is (.contains (uut/sanitize-text "\u201chello\u201d") "\"")))
  (testing "removes reference-style links"
    (is (not (.contains (uut/sanitize-text "See [1]: http://example.com") "[1]:")))))

(def +ner-person-tokens+
  ["Albert" "Einstein" "developed" "the" "theory" "of" "relativity" "."
   "Marie" "Curie" "discovered" "polonium" "and" "radium" "."
   "Barack" "Obama" "served" "as" "president" "."])

(def +ner-location-tokens+
  ["The" "conference" "was" "held" "in" "New" "York" "."
   "Delegates" "arrived" "from" "London" "," "Paris" "and" "Berlin" "."
   "The" "summit" "concluded" "in" "Geneva" "."])

(def +ner-organization-tokens+
  ["Microsoft" "and" "Google" "are" "technology" "companies" "."
   "The" "United" "Nations" "held" "a" "meeting" "."
   "Apple" "released" "new" "products" "."])

(deftest find-names-person-test
  (testing "finds at least one person name from a list of famous scientists/politicians"
    (let [names (set (uut/find-names :en +ner-person-tokens+))]
      (is (seq names) "should find at least one named entity")
      (is (some #(re-find #"(?i)einstein|curie|obama|albert|marie|barack" %) names)
          (str "expected a known person name, got: " names)))))

(deftest find-names-location-test
  (testing "finds at least one well-known location"
    (let [names (set (uut/find-names :en +ner-location-tokens+))]
      (is (seq names) "should find at least one named entity")
      (is (some #(re-find #"(?i)new york|london|paris|berlin|geneva" %) names)
          (str "expected a known location, got: " names)))))

(deftest find-names-organization-test
  (testing "finds at least one well-known organization"
    (let [names (set (uut/find-names :en +ner-organization-tokens+))]
      (is (seq names) "should find at least one named entity")
      (is (some #(re-find #"(?i)microsoft|google|united nations|apple" %) names)
          (str "expected a known organization, got: " names)))))

(deftest find-names-de-no-person-test
  (testing "German NER does not have a person finder configured"
    ;; name-find for :de only has location + organization finders
    (let [names-de (set (uut/find-names :de +ner-person-tokens+))
          names-en (set (uut/find-names :en +ner-person-tokens+))]
      (is (<= (count names-de) (count names-en))
          "DE should find fewer or equal entities than EN (no person finder)"))))

(deftest names-via-analyze-text-test
  (testing "names are populated via full analyze-text pipeline"
    (let [text "Albert Einstein worked at the Institute for Advanced Study in Princeton ."
          {:keys [nlp]} (uut/analyze-text text :language :en)]
      (is (set? (:names nlp)))
      (is (seq (:names nlp)) "full pipeline should find at least one named entity"))))
