(ns llar.tags
  (:require
   [clojure.string :as string]))

(defn normalize-tag
  "Normalize a tag for storage. Accepts keywords, strings, or nil.
   For namespaced keywords, only the name part is used.
   - Converts to lowercase
   - Trims leading/trailing whitespace
   - Replaces whitespace runs with hyphens
   - Strips characters that are not Unicode letters, digits, or hyphens
   - Returns nil for empty/blank/nil results"
  [t]
  (when-let [s (cond
                 (nil? t) nil
                 (keyword? t) (name t)
                 (string? t) t
                 :else (str t))]
    (-> s
        string/trim
        string/lower-case
        (string/replace #"\s+" "-")
        (string/replace #"[^\p{L}\p{N}\-]" "")
        (string/replace #"-{2,}" "-")
        (string/replace #"^-|-$" "")
        not-empty)))

(defn normalize-tags
  "Normalize a collection of tags, removing any that normalize to nil."
  [tags]
  (keep normalize-tag tags))
