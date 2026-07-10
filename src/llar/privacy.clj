(ns llar.privacy
  (:require
   [clojure.string :as string])
  (:import
   [java.net URI]))

(def ^:private tracking-param-names
  #{"fbclid" "gclid" "dclid" "gbraid" "wbraid" "mc_cid" "mc_eid"
    "igshid" "msclkid" "yclid" "_hsenc" "_hsmi"})

(defn- tracking-param? [raw-name]
  (let [name (string/lower-case raw-name)]
    (or (string/starts-with? name "utm_")
        (contains? tracking-param-names name))))

(defn- query-param-name [raw-param]
  (-> raw-param
      (string/split #"=" 2)
      first
      (or "")))

(defn- filtered-query [raw-query]
  (when raw-query
    (let [params (remove #(tracking-param? (query-param-name %))
                         (string/split raw-query #"&" -1))]
      (when (seq params)
        (string/join "&" params)))))

(defn strip-tracking-params [url]
  (when (some? url)
    (try
      (let [uri (URI. (str url))
            scheme (.getScheme uri)]
        (if-not (#{"http" "https"} (some-> scheme string/lower-case))
          (str url)
          (let [query (filtered-query (.getRawQuery uri))
                path (or (.getRawPath uri) "")
                authority (.getRawAuthority uri)
                fragment (.getRawFragment uri)]
            (str scheme
                 "://"
                 authority
                 path
                 (when (some? query) (str "?" query))
                 (when (some? fragment) (str "#" fragment))))))
      (catch Exception _
        (str url)))))

(defn external-http-url? [url]
  (try
    (let [uri (URI. (str url))
          scheme (some-> (.getScheme uri) string/lower-case)]
      (and (#{"http" "https"} scheme)
           (some? (.getHost uri))))
    (catch Exception _
      false)))

(defn- rel-token-set [rel]
  (->> (string/split (or rel "") #"\s+")
       (remove string/blank?)
       (into #{})))

(defn secure-link-attrs [attrs]
  (if (external-http-url? (:href attrs))
    (assoc attrs
           :rel (->> (into (rel-token-set (:rel attrs)) ["noopener" "noreferrer"])
                     sort
                     (string/join " "))
           :referrerpolicy "no-referrer")
    attrs))
