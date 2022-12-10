(ns u1f596.fetch.imap
  (:require
   [u1f596.fetch :as fetch]
   [u1f596.schema :as schema]
   [u1f596.persistency :as persistency]
   [u1f596.postproc :as postproc]
   [u1f596.analysis :as analysis]
   [clojure-mail.core :as mail-core]
   [org.bovinegenius [exploding-fish :as uri]]
   [clojure-mail.message :as mail-message]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [try+]]
   [clojure.string :as string]
   [pantomime.media :as mt]
   [java-time :as time]
   [schema.core :as s])
  (:import
   [javax.mail Session]))

(s/defrecord ImapItem
             [meta :- schema/Metadata
              summary :- schema/Summary
              hash :- schema/Hash
              raw :- s/Any
              entry :- schema/MailEntry]
  Object
  (toString [item] (fetch/item-to-string item)))

(defn try-get-base-type [content-type]
  (try+
   (str (mt/base-type content-type))
   (catch Object _
     content-type)))

(defn mail-body-to-contents [m]
  (into {} (map (fn [b] [(try-get-base-type (:content-type b)) (:body b)]) (:body m))))

(defn get-new-messages [uri {:keys [username password]}]
  (let [p (mail-core/as-properties {"mail.imap.starttls.enable" "true"
                                    "mail.imap.ssl.checkserveridentity" "true"})
        session (Session/getDefaultInstance p)
        store (mail-core/store (uri/scheme uri) session (uri/host uri)
                               username password)
        msgs (doall (map (fn [id]
                           (let [msg (mail-message/read-message id)]
                             (if (nil? (:body msg))
                               (log/warn "Failed to parse mail body:" msg)
                               {:to (:id msg)
                                :cc (:cc msg)
                                :bcc (:bcc msg)
                                :from (:from msg)
                                :subject (:subject msg)
                                :date-sent (:date-sent msg)
                                :date-received (:date-received msg)
                                :content-type (:content-type msg)
                                :body (mail-body-to-contents msg)
                                :headers (:headers msg)})))
                         (mail-core/unread-messages store (subs (uri/path uri) 1))))]
    (mail-core/close-store store)
    msgs))

(extend-protocol postproc/ItemProcessor
  ImapItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (update item :entry merge (:entry item) nlp)))
  (filter-item [item src state] false))

(extend-protocol persistency/CouchItem
  ImapItem
  (to-couch [item]
    (-> item
        (assoc-in [:meta :source :creds] nil)
        (assoc :type :mail))))

(defn mail-ts [m]
  (let [ts (or (:date-sent m)
               (:date-received m))]
    (log/info ts (type ts))
    (time/zoned-date-time ts "UTC")))


(extend-protocol fetch/FetchSource
  u1f596.src.ImapMailbox
  (fetch-source [src]
    (for [m (get-new-messages (uri/uri (:uri src)) (:creds src))]
      (->ImapItem
       (fetch/make-meta src)
       {:ts (mail-ts m) :title (:subject m)}
       (fetch/make-item-hash (:subject m) (:body m))
       m
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
        :contents (:body m)}))))
