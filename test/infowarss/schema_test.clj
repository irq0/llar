(ns infowarss.schema-test
  (:require [schema.core :as s]
            [infowarss.schema :refer :all]
            [schema-generators.complete :as c]
            [schema-generators.generators :as g]
            [clj-time.format :as tf]
            [clojure.test.check.generators :as gen]
            [clj-time.core :as time]
            [clj-time.coerce :as tc]
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
   org.joda.time.DateTime (gen/fmap #(tc/from-long (* 1000 %)) (gen/resize Integer/MAX_VALUE gen/pos-int))
   PosInt gen/pos-int
   PosFloat (gen/double* {:min 0})
   KwSet (gen/set gen/keyword)
   BoolInt (gen/elements [0 1])
   UnixTimestamp (gen/resize Integer/MAX_VALUE gen/pos-int)
   FeverIntList (gen/fmap #(string/join "," %) (gen/vector gen/pos-int))
   FeverImageData (g/always (str "image/gif;base64;foo="))
   URLStr url-gen
   java.net.URL (gen/fmap #(io/as-url %) url-gen)
   TwitterTimestamp (gen/fmap #(->> (* 1000 %)
                                tc/from-long
                                (tf/unparse (tf/formatter "EEE MMM dd HH:mm:ss Z yyyy")))
                      (gen/resize Integer/MAX_VALUE gen/pos-int))
   })
