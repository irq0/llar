(defproject infowarss "0.1.0-SNAPSHOT"
  :description "infowarss - information processor"
  :url ""
  :jvm-opts ["-Xmx4g"
             "-server"
             "-XX:-OmitStackTraceInFastThrow"
             "-XX:+TieredCompilation"
             "-XX:TieredStopAtLevel=1"
             "-XX:+CMSClassUnloadingEnabled"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+UseParNewGC"
             "-XX:+CMSParallelRemarkEnabled"
             "-XX:+AggressiveOpts"
             "-XX:+UseFastAccessorMethods"]

  :license {:name "None"
            :url ""}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [clj-http "3.5.0"]
                 [org.clojure/core.async "0.3.442"]
                 [slingshot "0.12.2"]
                 [hickory "0.7.1"]
                 [hiccup "1.0.5"]
                 [clojurewerkz/serialism "1.3.0"]
                 [clj-rome "0.4.0"]
                 [digest "1.4.5"]
                 [table "0.5.0"]
                 [matchbox "0.0.9"]
                 [io.forward/clojure-mail "1.0.7"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.slf4j/log4j-over-slf4j "1.7.25"]
                 [org.slf4j/jul-to-slf4j "1.7.25"]
                 [org.slf4j/jcl-over-slf4j "1.7.25"]
                 [com.fzakaria/slf4j-timbre "0.3.5"]
                 [clojurewerkz/quartzite "2.0.0"]
                 [twitter-api "1.8.0"]
                 [com.draines/postal "2.0.2"]
                 [ring/ring-core "1.5.1"]
                 [compojure "1.5.2"]
                 [metosin/compojure-api "1.1.10"]
                 [prismatic/schema-generators "0.1.0"]
                 [liberator "0.14.1"]
                 [prismatic/schema "1.1.5"]
                 [org.clojure/test.check "0.9.0"]
                 [ring/ring-devel "1.5.1"]
                 [ring/ring-jetty-adapter "1.5.1"]
                 [ring/ring-json "0.4.0"]
                 [mount "0.1.11"]
                 [robert/hooke "1.3.0"]
                 [ring/ring-mock "0.3.0"]
                 [lambda-ml "0.1.0"]
                 [clojure-opennlp "0.4.0"]
                 [im.chit/hara.io.scheduler "2.5.2"]
                 [ring/ring-mock "0.3.0"]
                 [com.gravity/goose "2.1.23"]
                 [ring-basic-authentication "1.0.5"]
                 [com.novemberain/pantomime "2.9.0"]
                 [com.ashafa/clutch "0.4.0"]
                 [com.firebase/firebase-client-jvm "2.5.2"]
                 [com.google.api-client/google-api-client "1.22.0"]
                 [com.google.apis/google-api-services-youtube "v3-rev183-1.22.0"]
                 [clj-time "0.13.0"]])
