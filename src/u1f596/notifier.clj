(ns u1f596.notifier
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [mount.core :as mount]
   [telegrambot-lib.core :as tbot]))

(def creds (edn/read-string (slurp (io/resource "credentials.edn"))))

(mount/defstate telegram-bot
  :start (tbot/create (:telegram creds))
  :stop (tbot/close @telegram-bot))

(def notify-groups {:vac -698725621
                    :me 158641116})

(defn notify [group message]
  (tbot/send-message telegram-bot {:chat_id (get notify-groups group)
                                   :text message}))
