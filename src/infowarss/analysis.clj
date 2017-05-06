(ns infowarss.analysis
  (:require
   [clojure.java.io :as io]
   [opennlp.nlp :as nlp]
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

(def pos-tagger
  {:en (nlp/make-pos-tagger (io/resource "nlp/models/en-pos-maxent.bin"))
   :de (nlp/make-pos-tagger (io/resource "nlp/models/de-pos-perceptron.bin"))})

(defn find-names [lang tokens]
  (mapcat (fn [finder] (finder tokens)) (get name-find lang)))

(defn analyze-text [text & {:keys [language]}]
  (let [lang (or language (keyword (pl/detect-language text)))]
    (if (#{:de :en} lang)
      (let [tokens ((get tokenizer lang)  text)
            pos ((get pos-tagger lang) tokens)]
        (log/debug "NLP Analysis running" )
        {:language lang
         :nlp {:names (set (find-names lang tokens))
               :nouns (set (map first (nlp-filter/nouns pos)))
               :verbs (set (map first (verbs-de-en pos)))}})
      {})))

(defn analyze-entry [entry]
  (let [text (get-in entry [:contents "text/plain"])]
    (when-not (string/blank? text)
      (analyze-text text :language (get entry :language)))))


(defn analyze-item [item]
  (let [text (get-in item [:entry :contents "text/plain"])]
    (when-not (string/blank? text)
      (analyze-text text :language (get-in item [:entry :language])))))
