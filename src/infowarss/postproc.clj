(ns infowarss.postproc
  (:require
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders]))


(defn- make-postproc-chain-func [chain]
  (let [with-logging (map (fn [fun]
                            (fn [x]
                              (log/infof "Postproc: (%s %s)" fun x)
                              (fun x)))
                       chain)]
    (apply comp with-logging)))


(defn postproc [result]
  (log/infof "Postprocessing: %s -> %s"
    (get-in result [:source :title])
    (get-in result [:summary :title]))
  (if-let [chain (get-in result [:source :postproc])]
    (let [fun (make-postproc-chain-func chain)]
      (fun result))
    result))
