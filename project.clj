(defproject infowarss "0.1.0-SNAPSHOT"
  :description "infowarss - information processor"
  :url ""
  :jvm-opts ["-Xmx4g"
             "-server"
             "-XX:-OmitStackTraceInFastThrow"
             "-XX:+TieredCompilation"
             "-XX:TieredStopAtLevel=1"
             "-Djdk.attach.allowAttachSelf=true"
             "-XX:+CMSClassUnloadingEnabled"
             "-XX:+CMSParallelRemarkEnabled"
             "-XX:+AggressiveOpts"]

  :main user

  :license {:name "None"
            :url ""}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [nrepl "0.6.0"]
                 [cider/cider-nrepl "0.22.4"]
                 [clj-http "3.10.0"]
                 [org.clojure/core.async "0.6.532"]
                 [slingshot "0.12.2"]
                 [hickory "0.7.1"]
                 [hiccup "1.0.5"]
                 [matchbox "0.0.9"]
                 [clj-rome "0.4.0"]
                 [digest "1.4.9"]
                 [table "0.5.0"]
                 [nio2 "0.2.1"]
                 [io.forward/clojure-mail "1.0.8"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.slf4j/log4j-over-slf4j "1.7.30"]
                 [org.slf4j/jul-to-slf4j "1.7.30"]
                 [org.slf4j/jcl-over-slf4j "1.7.30"]
                 [com.fzakaria/slf4j-timbre "0.3.17"]
                 [byte-streams "0.2.4"]
                 [twitter-api "1.8.0"]
                 [com.draines/postal "2.0.3"]
                 [ring/ring-core "1.8.0"]
                 [ring/ring-devel "1.8.0"]
                 [ring/ring-jetty-adapter "1.8.0"]
                 [ring/ring-json "0.5.0"]
                 [compojure "1.6.1"]
                 [prismatic/schema-generators "0.1.3"]
                 [prismatic/schema "1.1.12"]
                 [org.clojure/test.check "0.10.0"]
                 [bk/ring-gzip "0.3.0"]
                 [mount "0.1.16"]
                 [robert/hooke "1.3.0"]
                 [clojure-opennlp "0.5.0"]
                 [im.chit/hara.io.scheduler "2.5.10"]
                 [com.novemberain/pantomime "2.11.0"]
                 [com.firebase/firebase-client-jvm "2.5.2"]
                 [org.clojure/java.jdbc "0.7.11"]
                 [org.postgresql/postgresql "42.2.9"]
                 [mpg "1.3.0"]
                 [clojure-humanize "0.2.2"]
                 [clj-time "0.15.2"]
                 [com.clojure-goes-fast/clj-memory-meter "0.1.2"]
                 [clojurewerkz/urly "1.0.0"]
                 [com.mchange/c3p0 "0.9.5.2"]
                 [mvxcvi/puget "1.2.0"]
                 [cc.artifice/clj-ml "0.8.5"]
                 [clj-commons/iapetos "0.1.9"]])
