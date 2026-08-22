(ns llar.auth
  (:require
   [ring.util.response :as response])
  (:import
   [java.nio.charset StandardCharsets]
   [java.security MessageDigest]))

(defn constant-time=
  [expected provided]
  (and (string? expected)
       (string? provided)
       (MessageDigest/isEqual (.getBytes expected StandardCharsets/UTF_8)
                              (.getBytes provided StandardCharsets/UTF_8))))

(defn bearer-token [request]
  (some->> (response/get-header request "Authorization")
           (re-matches #"(?i)^Bearer[ ]+([^ ]+)$")
           second))

(defn validate-named-tokens!
  "Normalize a credentials :tokens map and fail fast on weak or ambiguous entries."
  [credentials service-name]
  (let [tokens (:tokens credentials)]
    (when-not (and (map? tokens) (seq tokens))
      (throw (ex-info (str service-name " credentials need a non-empty :tokens map")
                      {:type ::invalid-token-credentials})))
    (let [tokens (mapv (fn [[token-name token]]
                         (let [normalized-name
                               (when (or (keyword? token-name)
                                         (string? token-name))
                                 (name token-name))]
                           (when-not (and (some? normalized-name)
                                          (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,63}"
                                                      normalized-name)
                                          (string? token)
                                          (re-matches #"[A-Za-z0-9_-]{32,}" token))
                             (throw
                              (ex-info
                               (str service-name
                                    " tokens need names and at least 32 base64url-safe characters")
                               {:type ::invalid-token-credentials
                                :token-name token-name})))
                           {:name normalized-name :token token}))
                       tokens)]
      (when-not (= (count tokens) (count (set (map :name tokens))))
        (throw (ex-info (str service-name " token names must be unique")
                        {:type ::invalid-token-credentials})))
      (when-not (= (count tokens) (count (set (map :token tokens))))
        (throw (ex-info (str service-name " token values must be unique")
                        {:type ::invalid-token-credentials})))
      tokens)))

(defn token-owner [tokens provided]
  (when provided
    (->> tokens
         (map (fn [{:keys [name token]}]
                [name (constant-time= token provided)]))
         doall
         (keep (fn [[name match?]] (when match? name)))
         first)))
