(ns llar.email
  "General outgoing email. Two transports, chosen by config: with a :mail :host
  set, mail is relayed over SMTP to that server (e.g. a postfix on the container
  host); otherwise it is piped to the host's local `sendmail` binary (postal
  finds /usr/sbin/sendmail etc., or $SENDMAIL). Either way delivery, queuing and
  retrying are the MTA's job. Thin wrapper over postal so other features can send
  mail without knowing the transport."
  (:require
   [llar.appconfig :as appconfig]
   [postal.core :as postal]))

(defn configured?
  "True when mail sending is enabled (a :mail config block is present)."
  []
  (boolean (appconfig/mail)))

(defn- server
  "postal server map. With a :host in the :mail config, relay over SMTP to it;
  otherwise an empty map, which makes postal pipe to the local `sendmail`. Auth
  :user/:pass come from the credentials entry named by :credentials. Maps our
  :tls (implicit TLS) and :starttls config to postal's :ssl and :tls."
  []
  (let [cfg (appconfig/mail)]
    (if (:host cfg)
      (let [creds (some-> (:credentials cfg) appconfig/credentials)
            user (or (:user creds) (:username creds))
            pass (or (:pass creds) (:password creds))]
        (cond-> (select-keys cfg [:host :port])
          user (assoc :user user)
          pass (assoc :pass pass)
          (:tls cfg) (assoc :ssl true)
          (:starttls cfg) (assoc :tls true)))
      {})))

(defn send-message!
  "Send msg (a postal message map: :to, :subject, :body, optionally :from). Uses
  SMTP when a :mail :host is configured, otherwise the local `sendmail` binary.
  :from defaults to the :mail :from config."
  [msg]
  (let [from (or (:from msg) (appconfig/mail :from))]
    (postal/send-message (server) (assoc msg :from from))))
