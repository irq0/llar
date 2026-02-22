(ns llar.analysis
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [opennlp.nlp :as nlp]
   [opennlp.tools.filters :as nlp-filter]
   [slingshot.slingshot :refer [try+]])
  (:import
   [org.apache.tika.langdetect.optimaize OptimaizeLangDetector]
   [org.apache.tika.language.detect LanguageDetector]))

(def ^LanguageDetector language-detector (-> (OptimaizeLangDetector.) .loadModels))

(def stopwords
  {:en (with-open [rdr (io/reader (io/resource "stopwords_en.txt"))] (set (line-seq rdr)))
   :de (with-open [rdr (io/reader (io/resource "stopwords_de.txt"))] (set (line-seq rdr)))})

(def tokenizer
  {:en (nlp/make-tokenizer (io/resource "opennlp-en-ud-ewt-tokens-1.3-2.5.4.bin"))
   :de (nlp/make-tokenizer (io/resource "opennlp-de-ud-gsd-tokens-1.3-2.5.4.bin"))})

(def name-find
  {:de [(nlp/make-name-finder (io/resource "nlp/models/en-ner-location.bin"))
        (nlp/make-name-finder (io/resource "nlp/models/en-ner-organization.bin"))]
   :en [(nlp/make-name-finder (io/resource "nlp/models/en-ner-location.bin"))
        (nlp/make-name-finder (io/resource "nlp/models/en-ner-organization.bin"))
        (nlp/make-name-finder (io/resource "nlp/models/en-ner-person.bin"))]})

;; UD UPOS tagset (used by opennlp-models 1.3.0 / OpenNLP 2.x)
;; Verbs: VERB (main verbs) — AUX excluded (auxiliaries are not content words)
(nlp-filter/pos-filter verbs-de-en #"^VERB$")
;; Keep everything except punctuation (PUNCT) and symbols (SYM)
(nlp-filter/pos-filter not-punctuation #"^(?!PUNCT$|SYM$)[A-Z]")
;; Nouns: NOUN (common) + PROPN (proper nouns)
(nlp-filter/pos-filter nouns-ud #"^(NOUN|PROPN)$")

(def pos-tagger
  {:en (nlp/make-pos-tagger (io/resource "opennlp-en-ud-ewt-pos-1.3-2.5.4.bin"))
   :de (nlp/make-pos-tagger (io/resource "opennlp-de-ud-gsd-pos-1.3-2.5.4.bin"))})

(defn find-names [lang tokens]
  (try+
   (mapcat (fn [finder] (finder tokens)) (get name-find lang))
   (catch Object _
     (log/warn (:throwable &throw-context) "Open NLP Name finder failed. Returning empty set")
     [])))

(def url-regex #"https?://(?:[\w_-]+(?:(?:\.[\w_-]+)+))(?:[\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?")

(defn extract-urls [s]
  (re-seq url-regex s))

(defn remove-urls [s]
  (string/replace s url-regex ""))

(defn remove-html2text-artifacts [s]
  (-> s
      (string/replace #"[’']" "'")
      (string/replace #"[”“]" "\"")
      (string/replace #"\[[0-9]+\]:?" "")
      (string/replace #"\[+(.+)\]+" "$1")
      (string/replace #"\[|\]" "")))

(defn non-word-string-filter [s]
  (or (re-find #"^[\W_]+$" s)
      (re-find #"^['\"]+$" s)
      (re-find #"^['\"]\w['\"]$" s)
      (re-find #"^['\"]\w{1,2}$" s)))

(defn sanitize-text [s]
  (-> s
      remove-urls
      remove-html2text-artifacts))

(defn token-frequencies
  "Calcular term frequency. Token freq normanized to token count"
  [tokens]
  (let [freqs (frequencies tokens)
        ntokens (count tokens)]
    (->> freqs
         (map (fn [[token freq]]
                [token (/ freq ntokens)]))
         (into {}))))

(defn detect-language [text]
  (->> text
       (.detect language-detector)
       (.getLanguage)))

(defn find-best-analysis-language [{:keys [text lang]}]
  (let [text-size (count text)]
    (cond
      (#{:de :en} lang) lang
      (< text-size 150) :en
      :else
      (let [detected-lang (keyword (detect-language text))]
        (if (#{:de :en} detected-lang)
          detected-lang
          (do
            (log/debug "NLP: Unsupported language detected: " detected-lang)
            :en))))))

(defn analyze-text [text & {:keys [language]}]
  (let [lang (find-best-analysis-language {:text text
                                           :lang language})
        urls (extract-urls text)
        text-sanitized (sanitize-text text)

        tokens ((get tokenizer lang)  text-sanitized)

        pos ((get pos-tagger lang) tokens)

        stopwords (get stopwords lang)
        all-words (->> pos
                       not-punctuation
                       (map first)
                       (remove non-word-string-filter)
                       (map string/lower-case))
        words (->> all-words
                   (remove stopwords))
        nouns (->> pos
                   nouns-ud
                   (map first)
                   (remove non-word-string-filter)
                   (map string/lower-case)
                   (remove stopwords))
        verbs (->> pos
                   verbs-de-en
                   (map first)
                   (remove non-word-string-filter)
                   (map string/lower-case))]
    {:language lang
     :nlp {:nwords (count all-words)
           :top {:words (->> words
                             token-frequencies
                             (sort-by val)
                             reverse)
                 :nouns (->> nouns
                             token-frequencies
                             (sort-by val)
                             reverse)}
           :urls (set urls)
           :names (set (find-names lang tokens))
           :nouns (set nouns)
           :verbs (set verbs)}}))

(defn analyze-entry [entry]
  (let [text (or (get-in entry [:contents "text/plain"])
                 (get-in entry [:descriptions "text/plain"]))]
    (when-not (string/blank? text)
      (analyze-text text :language (get entry :language)))))

(defn analyze-item [item]
  (let [text (get-in item [:entry :contents "text/plain"])]
    (when-not (string/blank? text)
      (analyze-text text :language (get-in item [:entry :language])))))
