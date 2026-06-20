(ns llar.email
  "General outgoing email via the host's local MTA (the `sendmail` binary: postal
  finds /usr/sbin/sendmail etc., or $SENDMAIL). No SMTP, port, auth or TLS —
  delivery and queuing are the MTA's job. Thin wrapper over postal so other
  features can send mail without knowing the transport."
  (:require
   [llar.appconfig :as appconfig]
   [postal.core :as postal]))

(defn configured?
  "True when mail sending is enabled (a :mail config block is present)."
  []
  (boolean (appconfig/mail)))

(defn send-message!
  "Send msg (a postal message map: :to, :subject, :body, optionally :from) via
  the local MTA. :from defaults to the :mail :from config. Passing an empty
  server map makes postal pipe the message to the local `sendmail` binary."
  [msg]
  (let [from (or (:from msg) (appconfig/mail :from))]
    (postal/send-message {} (assoc msg :from from))))
