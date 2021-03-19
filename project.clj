(defproject infowarss "0.1.0-SNAPSHOT"
  :profiles {:dev {:dependencies [[alembic "0.3.2"]]}}
  
  :description "infowarss - information processor"
  :url ""
  :jvm-opts ["-Xmx4g"
             "-server"
             "-XX:-OmitStackTraceInFastThrow"
             "-XX:+TieredCompilation"
             "-XX:TieredStopAtLevel=1"
             "-Djdk.attach.allowAttachSelf=true"
             "-XX:+CMSClassUnloadingEnabled"
             "-XX:+CMSParallelRemarkEnabled"]

  :aot :all
  :main infowarss.core
  :plugins [[lein-cljfmt "0.6.8"] [lein-aot-order "0.1.0"]]
  :license {:name "None"
            :url ""}
  :exclusions [org.slf4j/slf4j-nop]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [nrepl "0.8.0"]
                 [cider/cider-nrepl "0.25.3"]
                 [clj-http "3.10.2"]
                 [org.clojure/core.async "1.3.610"]
                 [slingshot "0.12.2"]
                 [hickory "0.7.1"]
                 [hiccup "1.0.5"]
                 [matchbox "0.0.9"]
                 [clj-rome "0.4.0"]
                 [digest "1.4.9"]
                 [table "0.5.0"]
                 [nio2 "0.2.3"]
                 [io.forward/clojure-mail "1.0.8"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.slf4j/log4j-over-slf4j "1.7.30"]
                 [org.slf4j/jul-to-slf4j "1.7.30"]
                 [org.slf4j/jcl-over-slf4j "1.7.30"]
                 [com.fzakaria/slf4j-timbre "0.3.19"]
                 [byte-streams "0.2.4"]
                 [twitter-api "1.8.0"]
                 [com.draines/postal "2.0.3"]
                 [ring/ring-core "1.8.1"]
                 [ring/ring-devel "1.8.1"]
                 [ring/ring-jetty-adapter "1.8.1"]
                 [ring/ring-json "0.5.0"]
                 [compojure "1.6.2"]
                 [prismatic/schema-generators "0.1.3"]
                 [prismatic/schema "1.1.12"]
                 [org.clojure/test.check "1.1.0"]
                 [bk/ring-gzip "0.3.0"]
                 [mount "0.1.16"]
                 [robert/hooke "1.3.0"]
                 [clojure-opennlp "0.5.0"]
                 [hara/io.scheduler "3.0.11"]
                 [com.novemberain/pantomime "2.11.0"]
                 [com.firebase/firebase-client-jvm "2.5.2"]
                 [org.clojure/java.jdbc "0.7.11"]
                 [org.postgresql/postgresql "42.2.15"]
                 [mpg "1.3.0"]
                 [clojure-humanize "0.2.2"]
                 [clojure.java-time "0.3.2"]
                 [com.clojure-goes-fast/clj-memory-meter "0.1.3"]
                 [org.bovinegenius/exploding-fish "0.3.6"]
                 [hikari-cp "2.13.0"]
                 [mvxcvi/puget "1.3.1"]
                 [cc.artifice/clj-ml "0.8.7"]
                 [clj-commons/iapetos "0.1.9"]
                 [com.layerware/hugsql "0.5.1"]])

