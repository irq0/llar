(ns user
  (:require
   [mount.core :as mount]
   [clojure.tools.namespace.repl :as tn]
   [infowarss.http :as http :refer [fetch]]
   [infowarss.core :as core :refer [*srcs* config]]
   [infowarss.persistency :as persistency :refer [store-items!]]
   [infowarss.db :as db]
   [infowarss.update :refer :all]
   [infowarss.webapp :as webapp]
   [infowarss.src :as src]
   [hickory.select :as S]
   [hickory.render :refer [hickory-to-html]]
   [infowarss.fetch :as fetch]
   [infowarss.postproc :as proc]
   [infowarss.live :as live]
   [infowarss.schema :as schema]
   [infowarss.analysis :as analysis]
   [infowarss.converter :as converter]
   [infowarss.logging :refer [with-logging-status]]
   [infowarss.apis.fever-test]
   [infowarss.repl :refer :all]
   [clj-http.client :as http-client]
   [slingshot.slingshot :refer [throw+ try+]]
   [clj-time.core :as time]
   [clj-time.coerce :as tc]
   [clj-time.format :as tf]
   [taoensso.timbre :as log]
   [table.core :refer [table]]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [infowarss.fetch.twitter]
   [postal.core :as postal]
   [schema.core :as s]
   [cheshire.core :as json]
   [twitter.oauth :as twitter-oauth]
   [twitter.api.restful :as twitter]
   [clojure.java.shell :as shell]
   [opennlp.nlp :as nlp]
   [clojure.core.async :refer [>!! <!!] :as async]))

(mount/in-clj-mode)


(defn start []
  (with-logging-status)
  (mount/start))


(defn stop []
  (mount/stop))

(defn refresh []
  (stop)
  (tn/refresh))

(defn refresh-all []
  (stop)
  (tn/refresh-all))


(defn go []
  (start)
  :ready)

(defn reset []
  (stop)
  (tn/refresh :after 'user/go))



(defn -main [& args]
  (refresh)
  (start))
