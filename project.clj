(defproject infowarss "0.1.0-SNAPSHOT"
  :description "infowarss - information processor"
  :url ""
  :jvm-opts ["-Xmx4g"
             "-server"
             "-XX:-OmitStackTraceInFastThrow"
             "-XX:+TieredCompilation"
             "-XX:TieredStopAtLevel=1"
             "-XX:+CMSClassUnloadingEnabled"
             "-XX:+CMSParallelRemarkEnabled"
             "-XX:+AggressiveOpts"]

  :main user

  :license {:name "None"
            :url ""}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [cider/cider-nrepl "0.17.0"]
                 [clj-http "3.9.0"]
                 [org.clojure/core.async "0.4.474"]
                 [slingshot "0.12.2"]
                 [hickory "0.7.1"]
                 [hiccup "1.0.5"]
                 [clojurewerkz/serialism "1.3.0"]
                 [clj-rome "0.4.0"]
                 [digest "1.4.8"]
                 [table "0.5.0"]
                 [matchbox "0.0.9"]
                 [io.forward/clojure-mail "1.0.7"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.slf4j/log4j-over-slf4j "1.7.25"]
                 [org.slf4j/jul-to-slf4j "1.7.25"]
                 [org.slf4j/jcl-over-slf4j "1.7.25"]
                 [com.fzakaria/slf4j-timbre "0.3.12"]
                 [byte-streams "0.2.4"]
                 [twitter-api "1.8.0"]
                 [com.draines/postal "2.0.2"]
                 [ring/ring-core "1.6.3"]
                 [compojure "1.6.1"]
                 [metosin/compojure-api "1.1.12"]
                 [prismatic/schema-generators "0.1.2"]
                 [prismatic/schema "1.1.9"]
                 [org.clojure/test.check "0.9.0"]
                 [ring/ring-devel "1.6.3"]
                 [ring/ring-jetty-adapter "1.6.3"]
                 [ring/ring-json "0.4.0"]
                 [bk/ring-gzip "0.3.0"]
                 [mount "0.1.12"]
                 [robert/hooke "1.3.0"]
                 [ring/ring-mock "0.3.2"]
                 [clojure-opennlp "0.4.0"]
                 [im.chit/hara.io.scheduler "2.5.10"]
                 [ring/ring-mock "0.3.2"]
                 [com.gravity/goose "2.1.23"]
                 [ring-basic-authentication "1.0.5"]
                 [com.novemberain/pantomime "2.10.0"]
                 [com.ashafa/clutch "0.4.0"]
                 [com.firebase/firebase-client-jvm "2.5.2"]
                 [com.google.api-client/google-api-client "1.23.0"]
                 [com.google.apis/google-api-services-youtube "v3-rev198-1.23.0"]
                 [org.clojure/java.jdbc "0.7.7"]
                 [org.postgresql/postgresql "42.2.4"]
                 [mpg "1.3.0"]
                 [me.raynes/clhue "0.1.2"]
                 [com.google.guava/guava "23.0"]
                 [clojure-humanize "0.2.2"]
                 [clj-time "0.14.4"]
