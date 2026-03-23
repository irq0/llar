(ns llar.export.url-handler
  (:require
   [clojure.string :as string]
   [llar.appconfig :refer [appconfig]])
  (:import
   (java.net URLEncoder)))

;; Configurable URL handler for exporting annotations to external apps.
;; The :template string supports placeholders: {title}, {url}, {id}, {source}, {body}
;; Body: item notes as "- " list items on top, highlights as plain paragraphs.
;;
;; Example configurations (put in config.edn under :export :url-handler):
;;
;; org-roam via org-protocol (register llar: as org link type on Emacs side):
;;   {:name "Org-roam"
;;    :icon "fas fa-brain"
;;    :template "org-protocol://roam-ref?template=r&ref={url}&id={id}&title={title}&body={body}"}
;;
;; Obsidian:
;;   {:name "Obsidian"
;;    :icon "fas fa-gem"
;;    :template "obsidian://new?vault=MyVault&name={title}&content={body}"}
;;
;; Logseq:
;;   {:name "Logseq"
;;    :icon "fas fa-sitemap"
;;    :template "logseq://x-callback-url/quickCapture?title={title}&content={body}"}
;;
;; Bear:
;;   {:name "Bear"
;;    :icon "fas fa-paw"
;;    :template "bear://x-callback-url/create?title={title}&text={body}&url={url}&tags=llar,{source}"}

(defn url-handler-config []
  (get-in appconfig [:export :url-handler]))

(defn url-encode [s]
  (-> (URLEncoder/encode (str s) "UTF-8")
      (string/replace "+" "%20")))

(defn- extract-url [item]
  (let [raw (:url item)]
    (if (and (string? raw)
             (string/starts-with? raw "\"")
             (string/ends-with? raw "\""))
      (subs raw 1 (dec (count raw)))
      (str raw))))

(defn format-body
  "Format annotations as plain text.
   Notes as '- ' list items on top, highlights as plain paragraphs below."
  [_item annotations]
  (let [highlights (filter #(some? (:selector %)) annotations)
        notes (filter #(and (some? (:body %))
                            (nil? (:selector %))) annotations)]
    (str (when (seq notes)
           (str (string/join "\n" (map #(str "- " (:body %)) notes))
                "\n"))
         (when (and (seq notes) (seq highlights))
           "\n")
         (when (seq highlights)
           (str (string/join "\n\n"
                             (map #(get-in % [:selector :quote :exact]) highlights))
                "\n")))))

(defn build-export-url [item annotations]
  (let [config (url-handler-config)]
    (when (nil? config)
      (throw (ex-info "URL handler export not configured"
                      {:type ::not-configured})))
    (let [body (format-body item annotations)
          template (:template config)]
      (-> template
          (string/replace "{title}" (url-encode (:title item)))
          (string/replace "{url}" (url-encode (extract-url item)))
          (string/replace "{id}" (url-encode (str (:id item))))
          (string/replace "{source}" (url-encode (str (:source-key item))))
          (string/replace "{body}" (url-encode body))))))
