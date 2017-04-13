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
    (-> result :meta :source :title)
    (-> result :summary :title))
  (if-let [chain (get-in result [:meta :source :postproc])]
    (let [fun (make-postproc-chain-func chain)]
      (fun result))
    result))


;; postproc functions

(defn add-tag [tag]
  (fn [item]
    (update-in item [:meta :tags] conj tag)))

(defn copy [src dst]
  (fn [item]
    (let [src-val (get-in item src)]
      (-> item
        (assoc-in dst src-val)))))

(defn move [src dst]
  (fn [item]
    (let [src-val (get-in item src)]
      (-> item
        (assoc-in dst src-val)
        (assoc-in src nil)))))

(defn exchange [src dst]
  (fn [item]
    (let [src-val (get-in item src)
          dst-val (get-in item dst)]
      (-> item
        (assoc-in dst src-val)
        (assoc-in src dst-val)))))
