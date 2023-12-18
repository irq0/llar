(ns u1f596.human
  (:require
   [clojure.string :as string]
   [slingshot.slingshot :refer [try+ throw+]]
   [org.bovinegenius [exploding-fish :as uri]]
   [java-time :as time]))

(def +ellipsis+ "…")

(defn truncate-ellipsis [s len]
  (if (<= (count s) len) s (str (subs s 0 (- len 1)) +ellipsis+)))

(defn truncate [s len]
  (if (<= (count s) len) s (str (subs s 0 len))))

(defn datetime-ago [ts]
  (let [raw-duration (time/duration ts (time/zoned-date-time))
        duration (-> raw-duration
                     (.minusNanos (.getNano raw-duration)))
        period (time/period (time/local-date ts) (time/local-date))]
    (if (>= (.toDays duration) 2)
      (subs (string/lower-case (str period)) 1)
      (subs (string/lower-case (str duration)) 2))))

(defn host-identifier [url]
  ;; hack because some database entries got a strange serialization
  (let [url (if (map? url) (uri/map->uri url) url)
        host (uri/host url)]
    (try+
     (let [guava-host (com.google.common.net.InternetDomainName/from host)
           site (.topPrivateDomain guava-host)]
       (.name site))
     (catch Object _
       (str host)))))

(defn filesize [bytes]
  (let [units  [\space \K \M \G \T \P]
        pow (min (count units)
                 (int (Math/floor (/ (Math/log bytes) (Math/log 1024)))))
        val (float (/ bytes (Math/pow 1024 pow)))]
    (format "%.2f%c%c" val (nth units pow) \B)))
