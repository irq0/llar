(ns u1f596.schema-test
  (:require [schema.core :as s]
            [u1f596.schema :refer :all]
            [schema-generators.complete :as c]
            [schema-generators.generators :as g]
            [clojure.test.check.generators :as gen]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def url-gen
  (gen/let [path (gen/not-empty (gen/list (gen/fmap string/lower-case
                                            (gen/not-empty gen/string-alphanumeric))))
                    proto (gen/elements ["http" "ftp"])
            address (gen/not-empty (gen/list (gen/fmap string/lower-case
                                               (gen/not-empty gen/string-alphanumeric))))]
            (str proto "://" (string/join "." address) "/" (string/join "/" path))))

(def leaf-generators
  {Hash (gen/fmap #(str "SHA-256:" (digest/sha-256 %)) gen/string )
   PosInt gen/pos-int
   PosFloat (gen/double* {:min 0})
   KwSet (gen/set gen/keyword)
   BoolInt (gen/elements [0 1])
   NotEmptyStr (gen/not-empty gen/string-alphanumeric)
   UnixTimestamp (gen/resize Integer/MAX_VALUE gen/pos-int)
   URLStr url-gen
   java.net.URL (gen/fmap #(io/as-url %) url-gen)
   })
