(ns llar.apis.config-lab
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [llar.apis.capture :as token-auth]
   [llar.appconfig :as appconfig]
   [llar.config-lab :as lab]
   [ring.util.time :as ring-time])
  (:import
   [java.util Date]))

(def cookie-name "llar-config-lab-session")

(defn- no-store [response]
  (assoc-in response [:headers "Cache-Control"] "no-store"))

(defn- error-status [type]
  (case type
    :llar.config-lab/not-found 404
    :llar.config-lab/busy 429
    :llar.config-lab/timeout 408
    :llar.config-lab/unsafe-url 400
    :llar.config-lab/too-many-sessions 429
    400))

(defmacro with-errors [& body]
  `(try
     (no-store (do ~@body))
     (catch clojure.lang.ExceptionInfo exception#
       (let [data# (ex-data exception#)]
         (no-store {:status (error-status (:type data#))
                    :body {:error (some-> (:type data#) name)
                           :message (ex-message exception#)
                           :data (select-keys data# [:url :address :item-index
                                                     :timeout-ms :source-type
                                                     :field :expected :actual-type])}})))
     (catch Throwable throwable#
       (log/error throwable# "Config Lab request failed")
       (no-store {:status 500
                  :body {:error "config_lab_failure"
                         :message "Config Lab failed; see the server log"}}))))

(defn- credentials []
  (let [name (appconfig/config-lab :credentials)]
    (when-not name
      (throw (ex-info "Enabled Config Lab needs a :credentials entry"
                      {:type ::invalid-credentials})))
    (appconfig/credentials name)))

(defn- secure-request? [request]
  (or (= :https (:scheme request))
      (= "https" (get-in request [:headers "x-forwarded-proto"]))))

(defn login-response [request]
  (with-errors
    (let [tokens (token-auth/validate-tokens! (credentials) "Config Lab")
          provided (get-in request [:params :token])
          owner (token-auth/token-owner tokens provided)]
      (if owner
        (let [session-token (lab/login! owner)]
          {:status 200
           :cookies {cookie-name {:value session-token
                                  :http-only true
                                  :same-site :strict
                                  :secure (secure-request? request)
                                  :path "/api/config-lab"
                                  :max-age (* 60 (:session-ttl-minutes (lab/settings)))}}
           :body {:status "authenticated" :owner owner}})
        {:status 401
         :headers {"WWW-Authenticate" "Bearer realm=\"llar-config-lab\""}
         :body {:error "unauthorized"
                :message "Invalid Config Lab token"}}))))

(defn logout-response [request]
  (with-errors
    (lab/logout! (get-in request [:cookies cookie-name :value]))
    {:status 200
     :cookies {cookie-name {:value "" :max-age 0 :path "/api/config-lab"}}
     :body {:status "logged_out"}}))

(defn status-response [request]
  (no-store {:status 200
             :body {:status "authenticated"
                    :owner (:config-lab/owner request)}}))

(defn create-session-response [request]
  (with-errors
    {:status 201
     :body (lab/create-session! (:config-lab/owner request) (:params request))}))

(defn session-response [request id]
  (with-errors
    {:status 200 :body (lab/session-view (:config-lab/owner request) id)}))

(defn delete-session-response [request id]
  (with-errors
    (lab/delete-session! (:config-lab/owner request) id)
    {:status 200 :body {:deleted true}}))

(defn compile-response [request id]
  (with-errors
    {:status 200
     :body (lab/compile-session! (:config-lab/owner request) id (:params request))}))

(defn fetch-response [request id]
  (with-errors
    {:status 200 :body (lab/fetch-session! (:config-lab/owner request) id)}))

(defn selector-item-response [request id]
  (with-errors
    (let [item-index (get-in request [:params :item-index])
          item-index (if (int? item-index) item-index (parse-long (str item-index)))]
      (when-not (nat-int? item-index)
        (throw (ex-info "item-index must be a non-negative integer"
                        {:type ::invalid-item-index})))
      {:status 200
       :body (lab/fetch-selector-item! (:config-lab/owner request) id item-index)})))

(defn process-response [request id]
  (with-errors
    {:status 200
     :body (lab/process-session! (:config-lab/owner request)
                                 id
                                 (get-in request [:params :processors]))}))

(defn export-response [request id]
  (with-errors
    {:status 200
     :headers {"Content-Type" "application/edn; charset=utf-8"
               "Content-Disposition" "attachment; filename=\"config-lab.llar\""}
     :body (lab/export-form (:config-lab/owner request) id (:params request))}))

(defn blob-response [request id hash]
  (with-errors
    (let [blob (lab/session-blob (:config-lab/owner request) id hash)
          ^java.time.ZonedDateTime created (:created blob)]
      {:status 200
       :headers (cond-> {"Content-Type" (:mime-type blob)
                         "ETag" (str "W/\"" hash "\"")
                         "Last-Modified" (ring-time/format-date
                                          (Date/from (.toInstant created)))
                         "X-Content-Type-Options" "nosniff"}
                  (number? (:size blob))
                  (assoc "Content-Length" (str (:size blob))))
       :body (:data blob)})))

(defn wrap-auth
  "Authenticate only Config Lab endpoints. The rest of the dashboard retains its
  existing deployment model. A custom header protects cookie-authenticated writes
  from cross-site form submissions."
  [handler]
  (when (lab/enabled?)
    (token-auth/validate-tokens! (credentials) "Config Lab"))
  (fn [request]
    (let [uri (:uri request)
          lab-api? (string/starts-with? uri "/api/config-lab")
          login? (= uri "/api/config-lab/login")]
      (cond
        (and lab-api? (not (lab/enabled?)))
        {:status 404 :body {:error "not_found"}}

        (or (not lab-api?) login?)
        (handler request)

        :else
        (if-let [owner (lab/login-owner (get-in request [:cookies cookie-name :value]))]
          (if (and (#{:post :put :patch :delete} (:request-method request))
                   (not= "1" (get-in request [:headers "x-llar-config-lab"])))
            {:status 403
             :headers {"Cache-Control" "no-store"}
             :body {:error "csrf_check_failed"
                    :message "Missing Config Lab request header"}}
            (handler (assoc request :config-lab/owner owner)))
          {:status 401
           :headers {"Cache-Control" "no-store"
                     "WWW-Authenticate" "Bearer realm=\"llar-config-lab\""}
           :body {:error "unauthorized"
                  :message "Log in to Config Lab"}})))))

(defn- processor-input [id label default]
  [:div {:class "mb-2"}
   [:label {:class "form-label" :for (str "config-lab-" id)} label]
   [:textarea {:id (str "config-lab-" id)
               :class "form-control font-monospace"
               :rows 3}
    default]])

(defn- source-card []
  [:div {:class "col-xl-5"}
   [:div {:class "card"}
    [:div {:class "card-body"}
     [:div {:class "d-flex justify-content-between align-items-center"}
      [:h5 {:class "mb-0"} "Try a source"]
      [:span {:id "config-lab-dirty" :class "badge text-bg-secondary"} "Not run"]]
     [:p {:class "text-muted small mt-2"}
      "Paste the source constructor from a .llar file. Run compiles it, fetches a snapshot, and previews the first item."]
     [:label {:class "form-label" :for "config-lab-source-form"} "Source form"]
     [:textarea {:id "config-lab-source-form" :class "form-control font-monospace" :rows 16}
      "(src/selector-feed\n  \"https://example.org/\"\n  {:urls (S/tag :a)}\n  {}\n  {})"]
     [:div {:class "mt-3 d-flex align-items-center gap-2"}
      [:button {:id "config-lab-run" :class "btn btn-primary" :type "button"}
       [:i {:class "fas fa-play me-1"}] "Run"]
      [:button {:id "config-lab-refetch" :class "btn btn-outline-primary" :type "button" :disabled true}
       [:i {:class "fas fa-sync-alt me-1"}] "Refetch"]
      [:span {:id "config-lab-run-progress" :class "text-muted small"}]]
     [:details {:id "config-lab-export" :class "border rounded p-2 mt-3"}
      [:summary {:class "fw-semibold"} "Export configuration"]
      [:p {:class "text-muted small mt-2 mb-2"}
       "Deployment metadata is only needed when creating the final fetch form."]
      [:label {:class "form-label" :for "config-lab-source-key"} "Source key"]
      [:input {:id "config-lab-source-key" :class "form-control mb-2"
               :data-auto "true" :value "config-lab-source"}]
      [:label {:class "form-label" :for "config-lab-tags"} "Tags (EDN)"]
      [:input {:id "config-lab-tags" :class "form-control font-monospace mb-2" :value "#{}"}]
      [:label {:class "form-label" :for "config-lab-options"} "Reader options (EDN)"]
      [:input {:id "config-lab-options" :class "form-control font-monospace mb-2" :value "#{}"}]
      [:div {:class "d-flex gap-2"}
       [:button {:id "config-lab-copy" :class "btn btn-outline-secondary btn-sm" :type "button" :disabled true}
        "Copy .llar form"]
       [:button {:id "config-lab-download" :class "btn btn-outline-secondary btn-sm" :type "button" :disabled true}
        "Download"]]]]]])

(defn- processors-card []
  [:div
   [:p {:class "text-muted small"}
    "Each field evaluates to a function or vector of functions. A filter returning true removes the item."]
   (processor-input "pre" "Pre" "[]")
   (processor-input "filter" "Filter" "[]")
   (processor-input "post" "Post" "[]")
   [:button {:id "config-lab-process" :class "btn btn-primary" :type "button" :disabled true}
    "Apply processors"]
   [:div {:id "config-lab-transform-result" :class "mt-3"}]])

(defn- result-card []
  [:div {:class "col-xl-7"}
   [:div {:class "card h-100"}
    [:div {:class "card-header bg-white"}
     [:div {:class "d-flex justify-content-between align-items-start gap-2"}
      [:div
       [:h5 {:id "config-lab-result-title" :class "mb-1"} "Run a source to begin"]
       [:div {:id "config-lab-result-summary" :class "text-muted small"}
        "The preview and diagnostics will stay available here."]]
      [:a {:id "config-lab-open-original" :class "btn btn-sm btn-outline-secondary d-none"
           :target "_blank" :rel "noopener noreferrer"} "Open original"]]]
    [:div {:class "card-body p-0"}
     [:ul {:class "nav nav-tabs px-3 pt-2" :role "tablist"}
      (for [[id label] [["preview" "Preview"]
                        ["articles" "Articles"]
                        ["selectors" "Extraction"]
                        ["transform" "Transform"]
                        ["data" "Data"]
                        ["http" "HTTP"]]]
        [:li {:class "nav-item" :role "presentation"}
         [:button {:class (str "nav-link" (when (= id "preview") " active"))
                   :id (str "config-lab-" id "-tab")
                   :data-bs-toggle "tab"
                   :data-bs-target (str "#config-lab-" id "-pane")
                   :type "button" :role "tab"}
          label]])]
     [:div {:class "tab-content p-3"}
      [:div {:id "config-lab-preview-pane" :class "tab-pane fade show active" :role "tabpanel"}
       [:div {:id "config-lab-preview-empty" :class "text-muted py-5 text-center"}
        "No item preview yet."]
       [:div {:id "config-lab-preview" :class "d-none"}
        [:div {:id "config-lab-preview-meta" :class "small text-muted mb-2"}]
        [:iframe {:id "config-lab-preview-frame" :class "config-lab-preview-frame"
                  :title "Extracted article preview" :sandbox "allow-same-origin"}]]]
      [:div {:id "config-lab-articles-pane" :class "tab-pane fade" :role "tabpanel"}
       [:div {:id "config-lab-articles" :class "list-group config-lab-scroll-pane"}]]
      [:div {:id "config-lab-selectors-pane" :class "tab-pane fade" :role "tabpanel"}
       [:div {:id "config-lab-selectors" :class "config-lab-scroll-pane text-muted"}
        "Extraction diagnostics appear after running a SelectorFeed."]]
      [:div {:id "config-lab-transform-pane" :class "tab-pane fade" :role "tabpanel"}
       (processors-card)]
      [:div {:id "config-lab-data-pane" :class "tab-pane fade" :role "tabpanel"}
       [:div {:class "d-flex gap-2 align-items-center mb-1"}
        [:label {:class "small fw-semibold" :for "config-lab-data-root"} "Snapshot"]
        [:select {:id "config-lab-data-root" :class "form-select form-select-sm w-auto"}]]
       [:p {:class "small text-muted mb-2"}
        "Keys and values use EDN notation."]
       [:div {:id "config-lab-data-tree" :class "config-lab-data-tree config-lab-scroll-pane"}]]
      [:div {:id "config-lab-http-pane" :class "tab-pane fade" :role "tabpanel"}
       [:div {:id "config-lab-http" :class "config-lab-scroll-pane text-muted"}
        "HTTP traces appear after fetching."]]]]]])

(defn tab []
  [:div {:id "config-lab-app"}
   [:div {:class "alert alert-warning"}
    [:strong "Ephemeral lab: "]
    "runs never update runtime config or the database. Snapshots expire automatically."]
   [:section {:id "config-lab-login" :class "card mb-3"}
    [:div {:class "card-body"}
     [:h5 "Unlock Config Lab"]
     [:div {:class "input-group"}
      [:input {:id "config-lab-token" :class "form-control" :type "password"
               :autocomplete "current-password" :placeholder "Dedicated Config Lab token"}]
      [:button {:id "config-lab-login-button" :class "btn btn-primary" :type "button"}
       "Unlock"]]]]
   [:section {:id "config-lab-workbench" :hidden true}
    [:div {:class "row g-3"}
     (source-card)
     (result-card)]
    [:div {:id "config-lab-status" :class "alert alert-secondary mt-3" :role "status"} "Ready."]]])
