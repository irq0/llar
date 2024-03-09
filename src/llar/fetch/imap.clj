(ns llar.fetch.imap
  (:require
   [llar.fetch :as fetch]
   [llar.item]
   [llar.persistency :as persistency]
   [llar.postproc :as postproc]
   [llar.analysis :as analysis]
   [llar.fetchutils :as fetchutils]
   [clojure-mail.core :as mail-core]
   [org.bovinegenius [exploding-fish :as uri]]
   [clojure-mail.message :as mail-message]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [try+ throw+]]
   [clojure.string :as string]
   [java-time.api :as time]
   [clojure.spec.alpha :as s])
  (:import
   [javax.mail Session]))

(defrecord ImapItem
           [meta
            summary
            hash
            raw
            entry]
  Object
  (toString [item] (fetch/item-to-string item)))

(defn make-imap-item [meta summary hash entry raw]
  {:pre [(s/valid? :irq0/item-metadata meta)
         (s/valid? :irq0/item-summary summary)
         (s/valid? :irq0/item-hash hash)]}
  (->ImapItem meta summary hash entry raw))

(defn try-get-base-type [content-type]
  (try+
   (->
    (org.apache.tika.mime.MediaType/parse content-type)
    .getBaseType
    str)
   (catch Object _
     content-type)))

(def foo (atom []))

(defn mail-to-contents [msg]
  (let [bodies (:body msg)]
    (if (seq? bodies)
      (into {}
            (for [{:keys [body content-type]} bodies]
              {(try-get-base-type content-type) body}))
      {(try-get-base-type (:content-type bodies)) (:body bodies)})))

(defn get-new-messages [uri {:keys [username password]}]
  (try
    (let [p (mail-core/as-properties {"mail.imap.starttls.enable" "true"
                                      "mail.imap.ssl.checkserveridentity" "true"})
          session (Session/getDefaultInstance p)
          store (mail-core/store (uri/scheme uri) session (uri/host uri)
                                 username password)
          msgs (doall (map (fn [id]
                             (let [msg (mail-message/read-message id)]
                               (swap! foo conj msg)
                               (if (nil? (:body msg))
                                 (log/warn "failed to parse mail body:" msg)
                                 {:to (:id msg)
                                  :cc (:cc msg)
                                  :bcc (:bcc msg)
                                  :from (:from msg)
                                  :subject (:subject msg)
                                  :date-sent (:date-sent msg)
                                  :date-received (:date-received msg)
                                  :content-type (:content-type msg)
                                  :contents (mail-to-contents msg)
                                  :headers (:headers msg)})))
                           (mail-core/unread-messages store (subs (uri/path uri) 1))))]
      (mail-core/close-store store)
      msgs)
    (catch javax.mail.AuthenticationFailedException ex
      (throw+ {:type :llar.http/request-error
               :message (ex-message ex)}))
    (catch java.lang.Throwable ex
      (log/error ex "unexpected IMAP error")
      (throw+ {:type :llar.http/unexpected-error
               :message (ex-message ex)}))))

(extend-protocol postproc/ItemProcessor
  ImapItem
  (post-process-item [item _ _]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (update item :entry merge (:entry item) nlp)))
  (filter-item [_ _ _] false))

(extend-protocol persistency/CouchItem
  ImapItem
  (to-couch [item]
    (-> item
        (assoc-in [:meta :source :creds] nil)
        (assoc :type :mail))))

(defn mail-ts [m]
  (let [ts (or (:date-sent m)
               (:date-received m))]
    (time/zoned-date-time ts "UTC")))

(extend-protocol fetch/FetchSource
  llar.src.ImapMailbox
  (fetch-source [src]
    (let [messages (get-new-messages (:uri src) (:creds src))]
      (log/debugf "[IMAP] new:%s" (count messages))
      (for [m messages
            :let [contents (fetchutils/process-html-contents nil (:contents m))]]
        (do
          (log/debug "[IMAP] processing: " (select-keys m [:subject :from :date-send]))
          (make-imap-item
           (fetch/make-meta src)
           {:ts (mail-ts m) :title (:subject m)}
           (fetch/make-item-hash (:subject m)
                                 (or (get-in m [:contents "text/plain"])
                                     (get-in m [:contents "text/html"])
                                     ""))
           {:title (:subject m)
            :id (some-> (filter (fn [x] (= (some-> x first key string/lower-case) "message-id"))
                                (:headers m))
                        first
                        first
                        val)
            :received-ts (:date-received m)
            :sent-ts (:date-sent m)
            :authors (map #(format "%s <%s>" (:name %) (:address %)) (:from m))
            :descriptions {"text/plain" ""}
            :contents contents}
           m))))))
