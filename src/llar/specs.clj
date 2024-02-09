(ns llar.specs
  (:require
   [clojure.spec.alpha :as s]
   [org.bovinegenius.exploding-fish :as uri]
   [clojure.java.io :as io]))

(s/def :irq0/url-str (s/and string? #(try (io/as-url %) (catch Exception _ false))))
(s/def :irq0/url #(instance? org.bovinegenius.exploding_fish.UniformResourceIdentifier %))
(s/def :irq0/proper-url (s/and #(instance? org.bovinegenius.exploding_fish.UniformResourceIdentifier %)
                               #(uri/scheme %)
                               #(uri/host %)))
(s/def :irq0/absolute-url (s/or :proper-url (s/and :irq0/proper-url #(uri/absolute-path? %))
                                :data-url #(= "data" (uri/scheme %))
                                :mailto-url #(= "mailto" (uri/scheme %))))
(s/def :irq0/ts #(instance? java.time.ZonedDateTime %))
(def iso-date-time-regex #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}([+-]\d{2}:\d{2}|Z|)$")
(s/def :irq0/iso-date-time-str (s/and string? #(re-matches iso-date-time-regex %)))

;; (def url-gen
;;   (gen/let [path (gen/not-empty (gen/list (gen/fmap string/lower-case
;;                                                     (gen/not-empty gen/string-alphanumeric))))
;;             proto (gen/elements ["http" "ftp"])
;;             address (gen/not-empty (gen/list (gen/fmap string/lower-case
;;                                                        (gen/not-empty gen/string-alphanumeric))))]
;;     (str proto "://" (string/join "." address) "/" (string/join "/" path))))

;; (def leaf-generators
;;   {Hash (gen/fmap #(str "SHA-256:" (digest/sha-256 %)) gen/string)
;;    PosInt gen/pos-int
;;    PosFloat (gen/double* {:min 0})
;;    KwSet (gen/set gen/keyword)
;;    BoolInt (gen/elements [0 1])
;;    NotEmptyStr (gen/not-empty gen/string-alphanumeric)
;;    UnixTimestamp (gen/resize Integer/MAX_VALUE gen/pos-int)
;;    URLStr url-gen
;;    java.net.URL (gen/fmap #(io/as-url %) url-gen)})
