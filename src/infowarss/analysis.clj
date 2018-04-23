(ns infowarss.analysis
  (:require
   [clojure.java.io :as io]
   [opennlp.nlp :as nlp]
   [clojure.java.shell :as shell]
   [taoensso.timbre :as log]
   [clojure.string :as string]
   [opennlp.tools.filters :as nlp-filter]
   [pantomime.languages :as pl]))

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
(nlp-filter/pos-filter not-punctuation #"^[^[A-Z]]")

(def pos-tagger
  {:en (nlp/make-pos-tagger (io/resource "nlp/models/en-pos-maxent.bin"))
   :de (nlp/make-pos-tagger (io/resource "nlp/models/de-pos-perceptron.bin"))})

(defn find-names [lang tokens]
  (mapcat (fn [finder] (finder tokens)) (get name-find lang)))

(def url-regex #"https?://(?:[\w_-]+(?:(?:\.[\w_-]+)+))(?:[\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?")

(defn extract-urls [s]
  (re-seq url-regex s))

(defn remove-urls [s]
  (string/replace s url-regex ""))

(defn remove-html2text-artifacts [s]
  (-> s
    (string/replace #"\[[0-9]+\]:?" "")
    (string/replace #"\[+(.+)\]+" "$1")
    (string/replace #"\[|\]" "")))

(defn readability-scores [s]
  (let [{:keys [exit out err]}
        (shell/sh "style" :in s :env {"LANG" "c"})]
    {:flesch-index (when-let [[_ x] (re-find #"Flesch Index: ([\d\.-]+)/" out)]
                     (Float/parseFloat x))
     :smog-grade (when-let [[_ x] (re-find #"SMOG-Grading: ([\d\.-]+)" out)]
                   (Float/parseFloat x))}))

(defn sanitize-text [s]
  (-> s
    remove-urls
    remove-html2text-artifacts
    ))

(defn analyze-text [text & {:keys [language]}]
  (let [lang (or language (keyword (pl/detect-language text)))]
    (if (#{:de :en} lang)
      (let [urls (extract-urls text)
            text-sanitized (sanitize-text text)
            tokens ((get tokenizer lang)  text-sanitized)
            pos ((get pos-tagger lang) tokens)
            words (not-punctuation pos)]
        (log/debug "NLP Analysis running" )
        {:language lang
         :readability (readability-scores text-sanitized)
         :nlp {:nwords (count words)
               :top {:words (->> (not-punctuation words)
                              (map #(-> % first string/lower-case))
                              frequencies
                              (sort-by val)
                              reverse
                              (take 100))
                     :nouns (->> (nlp-filter/nouns words)
                              (map #(-> % first string/lower-case))
                              frequencies
                              (sort-by val)
                              reverse
                              (take 23))}
               :urls urls
               :names (set (find-names lang tokens))
               :nouns (set (map string/lower-case (map first (nlp-filter/nouns pos))))
               :verbs (set (map string/lower-case (map first (verbs-de-en pos))))}})
      {})))

(defn analyze-entry [entry]
  (let [text (or (get-in entry [:contents "text/plain"])
               (get-in entry [:descriptions "text/plain"]))]
    (when-not (string/blank? text)
      (analyze-text text :language (get entry :language)))))


(defn analyze-item [item]
  (let [text (get-in item [:entry :contents "text/plain"])]
    (when-not (string/blank? text)
      (analyze-text text :language (get-in item [:entry :language])))))
