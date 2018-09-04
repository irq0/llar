(ns infowarss.fetch.imap
  (:require
   [infowarss.converter :as conv]
   [infowarss.src :as src]
   [infowarss.fetch :as fetch]
   [infowarss.schema :as schema]
   [infowarss.persistency :as persistency]
   [infowarss.postproc :as postproc]
   [infowarss.analysis :as analysis]
   [clojure-mail.core :refer :all]
   [clojure-mail.message :refer (read-message)]
   [digest]
   [hickory.core :as hick]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [slingshot.slingshot :refer [throw+ try+]]
   [clojure.string :as string]
   [clojure.java.io :as io]
   [pantomime.media :as mt]
   [schema.core :as s])
  (:import
   [javax.mail Session])
  )


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
  (let [p (as-properties {"mail.imap.starttls.enable" "true"
                          "mail.imap.ssl.checkserveridentity" "true"})
        session (Session/getDefaultInstance p)
        store (store (.getScheme uri) session (.getHost uri)
                username password)
        msgs (doall (map (fn [id]
                           (let [msg (read-message id)]
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
                           (unread-messages store (subs (.getPath uri) 1))))]
    (close-store store)
    msgs))

(extend-protocol postproc/ItemProcessor
  ImapItem
  (post-process-item [item src state]
    (let [nlp (analysis/analyze-entry (:entry item))]
      (-> item
        (update :entry merge (:entry item) nlp))))

  (filter-item [item src state] false))

(extend-protocol persistency/CouchItem
  ImapItem
  (to-couch [item]
    (-> item
      (assoc-in [:meta :source :creds] nil)
      (assoc :type :mail))))

(defn mail-ts [m]
  (tc/from-date (or (:date-sent m)
                  (:date-received m))))


(extend-protocol fetch/FetchSource
  infowarss.src.ImapMailbox
  (fetch-source [src]
    (for [m (get-new-messages (:uri src) (:creds src))]
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
