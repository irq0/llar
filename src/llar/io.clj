(ns llar.io
  (:import
   [java.io ByteArrayOutputStream InputStream]))

(defn read-bounded-bytes!
  "Read `input` fully into a byte array, but call `(too-large! size)` as soon as
  the running byte count would exceed `limit`. Reads one byte past `limit` so a
  body of exactly `limit` bytes is accepted while `limit`+1 bytes is rejected.
  `too-large!` is expected to throw."
  [^InputStream input limit too-large!]
  (let [buffer (byte-array 8192)
        output (ByteArrayOutputStream.)]
    (loop [total 0]
      (let [remaining (inc (- limit total))
            n (.read input buffer 0 (min (alength buffer) remaining))]
        (cond
          (neg? n) (.toByteArray output)
          (> (+ total n) limit) (too-large! (+ total n))
          :else (do
                  (.write output buffer 0 n)
                  (recur (+ total n))))))))
