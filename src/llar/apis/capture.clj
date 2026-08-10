(ns llar.apis.capture
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :refer [context GET POST routes]]
   [compojure.route :as route]
   [hiccup2.core :as h]
   [llar.bookmark-capture :as capture])
  (:import
   [java.nio.charset StandardCharsets]
   [java.security MessageDigest]))

(defn- json-response [status body]
  {:status status
   :headers {"Cache-Control" "no-store"}
   :body body})

(defn feedback-page []
  {:status 200
   :headers {"Cache-Control" "no-store"
             "Content-Type" "text/html; charset=utf-8"}
   :body
   (str
    (h/html
     [:html {:lang "en"}
      [:head
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
       [:title "Save to Llar"]
       [:link {:rel "stylesheet" :href "static/capture.css"}]]
      [:body
       [:main
        [:h1 "Save to Llar"]
        [:p {:id "capture-status" :role "status" :aria-live "polite"}
         "Preparing capture…"]
        [:p {:id "capture-detail"}]
        [:button {:id "capture-close" :type "button" :hidden true} "Close"]]
       [:script {:src "static/capture.js" :defer true}]]]))})

(defn validate-tokens!
  "Return a vector of named bearer tokens, or fail service startup."
  ([credentials]
   (validate-tokens! credentials "Bookmark capture"))
  ([credentials service-name]
   (let [tokens (:tokens credentials)]
     (when-not (and (map? tokens) (seq tokens))
       (throw (ex-info (str service-name " credentials need a non-empty :tokens map")
                       {:type ::invalid-credentials})))
     (let [tokens (mapv (fn [[token-name token]]
                          (let [normalized-name (when (or (keyword? token-name)
                                                          (string? token-name))
                                                  (name token-name))]
                            (when-not (and (some? normalized-name)
                                           (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,63}"
                                                       normalized-name)
                                           (string? token)
                                           (re-matches #"[A-Za-z0-9_-]{32,}" token))
                              (throw (ex-info (str service-name " tokens need names and at least 32 base64url-safe characters")
                                              {:type ::invalid-credentials
                                               :token-name token-name})))
                            {:name normalized-name :token token}))
                        tokens)]
       (when-not (= (count tokens) (count (set (map :name tokens))))
         (throw (ex-info (str service-name " token names must be unique")
                         {:type ::invalid-credentials})))
       (when-not (= (count tokens) (count (set (map :token tokens))))
         (throw (ex-info (str service-name " token values must be unique")
                         {:type ::invalid-credentials})))
       tokens))))

(defn- constant-time= [left right]
  (MessageDigest/isEqual
   (.getBytes ^String left StandardCharsets/UTF_8)
   (.getBytes ^String right StandardCharsets/UTF_8)))

(defn- bearer-token [request]
  (some->> (get-in request [:headers "authorization"])
           (re-matches #"(?i)^Bearer[ ]+([^ ]+)$")
           second))

(defn token-owner [tokens provided]
  (when provided
    (->> tokens
         (map (fn [{:keys [name token]}]
                [name (constant-time= token provided)]))
         doall
         (keep (fn [[name match?]] (when match? name)))
         first)))

(defn wrap-token-auth [handler credentials]
  (let [tokens (validate-tokens! credentials)]
    (fn [request]
      (if (string/starts-with? (:uri request) "/api/")
        (if-let [owner (token-owner tokens (bearer-token request))]
          (handler (assoc request :capture/token-owner owner))
          {:status 401
           :headers {"Cache-Control" "no-store"
                     "Content-Type" "application/json"
                     "WWW-Authenticate" "Bearer realm=\"llar-capture\""}
           :body {:error "unauthorized"
                  :message "Missing or invalid capture token"}})
        (handler request)))))

(defn- capture-result [capture]
  (let [outcome (capture/enqueue-outcome capture)
        status (case outcome :queued 201 :needs-attention 409 200)]
    (json-response
     status
     {:capture_id (:id capture)
      :item_id (:item-id capture)
      :state (:status capture)
      :result (string/replace (name outcome) "-" "_")
      :message (get capture/outcome-messages outcome)})))

(defn capture-response [db request]
  (let [{:keys [url title]} (:params request)]
    (try
      (capture-result
       (capture/enqueue! db url title (:capture/token-owner request)))
      (catch clojure.lang.ExceptionInfo exception
        (if (#{:llar.bookmark-capture/invalid-url
               :llar.bookmark-capture/invalid-title}
             (:type (ex-data exception)))
          (json-response 400 {:error "invalid_request"
                              :message (ex-message exception)})
          (do
            (log/error exception "bookmark capture enqueue failed")
            (json-response 503 {:error "temporarily_unavailable"
                                :message "Llar could not durably queue this capture"}))))
      (catch Throwable throwable
        (log/error throwable "bookmark capture enqueue failed")
        (json-response 503 {:error "temporarily_unavailable"
                            :message "Llar could not durably queue this capture"})))))

(defn handler [db]
  (routes
   (GET "/" [] (feedback-page))
   (context "/api/v1" []
     (POST "/captures" request (capture-response db request)))
   (route/resources "/static" {:root "status"})
   (route/not-found {:status 404
                     :headers {"Content-Type" "application/json"}
                     :body {:error "not_found"}})))
