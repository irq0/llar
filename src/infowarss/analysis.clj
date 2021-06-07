(ns infowarss.analysis
  (:require
   [infowarss.appconfig :as appconfig]
   [clojure.java.io :as io]
   [opennlp.nlp :as nlp]
   [clojure.java.shell :as shell]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [try+]]
   [clojure.string :as string]
   [opennlp.tools.filters :as nlp-filter]
   [pantomime.languages :as pl]))

(def stopwords
  {:en (with-open [rdr (io/reader (io/resource "stopwords_en.txt"))] (set (line-seq rdr)))
   :de (with-open [rdr (io/reader (io/resource "stopwords_de.txt"))] (set (line-seq rdr)))})

(def tokenizer
  {:en (nlp/make-tokenizer (io/resource "nlp/models/en-token.bin"))
   :de (nlp/make-tokenizer (io/resource "nlp/models/de-token.bin"))})

(def name-find
  {:de [(nlp/make-name-finder (io/resource "nlp/models/en-ner-location.bin"))
        (nlp/make-name-finder (io/resource "nlp/models/en-ner-organization.bin"))]
   :en [(nlp/make-name-finder (io/resource "nlp/models/en-ner-location.bin"))
        (nlp/make-name-finder (io/resource "nlp/models/en-ner-organization.bin"))
        (nlp/make-name-finder (io/resource "nlp/models/en-ner-person.bin"))]})

(nlp-filter/pos-filter verbs-de-en #"^(VB|VV|VA|VM)")
(nlp-filter/pos-filter not-punctuation #"^[A-Z]+")

(def pos-tagger
  {:en (nlp/make-pos-tagger (io/resource "nlp/models/en-pos-maxent.bin"))
   :de (nlp/make-pos-tagger (io/resource "nlp/models/de-pos-perceptron.bin"))})

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

(defn readability-scores [s]
  (let [{:keys [exit out err]}
        (shell/sh (appconfig/command :diction-style) :in s :env {"LANG" "c"})]
    {:flesch-index (when-let [[_ x] (re-find #"Flesch Index: ([\d\.-]+)/" out)]
                     (Float/parseFloat x))
     :smog-grade (when-let [[_ x] (re-find #"SMOG-Grading: ([\d\.-]+)" out)]
                   (Float/parseFloat x))}))

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

(defn find-best-analysis-language [{:keys [text lang]}]
  (let [text-size (count text)]
    (cond
      (#{:de :en} lang) lang
      (< text-size 300) :en
      :else
      (let [detected-lang (keyword (pl/detect-language text))]
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
                   nlp-filter/nouns
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
     :readability (readability-scores text-sanitized)
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
