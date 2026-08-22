(ns llar.apis.capture
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [compojure.core :refer [context GET POST routes]]
   [compojure.route :as route]
   [hiccup2.core :as h]
   [llar.auth :as auth]
   [llar.bookmark-capture :as capture]))

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

(defn wrap-token-auth [handler credentials]
  (let [tokens (auth/validate-named-tokens! credentials "Bookmark capture")]
    (fn [request]
      (if (string/starts-with? (:uri request) "/api/")
        (if-let [owner (auth/token-owner tokens (auth/bearer-token request))]
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
