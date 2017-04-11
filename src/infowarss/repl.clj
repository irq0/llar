(ns infowarss.repl
  (:require
   [infowarss.core :refer :all]
   [infowarss.postproc :refer [postproc]]
   [infowarss.persistency :refer [store-items! duplicate?]]
   [infowarss.update :refer :all]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-time.coerce :as tc]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]))


(defn- src-light [[k v]]
  (let [base {:key k
              :title (get-in v [:src :title])
              :status (get-in v [:state :status])
              :last-success (tc/to-string (get-in v [:state :last-successful-fetch-ts]))}]
    (if (#{:perm-fail :temp-fail} (:status base))
      (assoc base :last-exception-msg
        (get-in v [:state :last-exception :message]))
      base)))

(defn sources []
  (map src-light @*srcs*))
