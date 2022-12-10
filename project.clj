(defproject u1f596 "0.1.0-SNAPSHOT"
  :profiles {:uberjar {:omit-source true
                       :aot :all}}
  :description "u1f596 - information processor"
  :url ""
  :jvm-opts ["-Xmx4g"
             "-server"
             "-XX:-OmitStackTraceInFastThrow"
             "-XX:+TieredCompilation"
             "-XX:TieredStopAtLevel=1"
             "-Djdk.attach.allowAttachSelf=true"]

  :aot :all
  :main u1f596.core
  :plugins [[lein-cljfmt "0.6.8"]]
  :license {:name "None"
            :url ""}
  :exclusions [org.slf4j/slf4j-nop]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [nrepl "1.0.0"]
                 [cider/cider-nrepl "0.29.0"]
                 [clj-http "3.12.3"]
                 [org.clojure/core.async "1.6.673"]
                 [slingshot "0.12.2"]
                 [hickory "0.7.1"]
                 [hiccup "1.0.5"]
                 [matchbox "0.0.9"]
                 [clj-rome "0.4.0"]
                 [digest "1.4.10"]
                 [table "0.5.0"]
                 [nio2 "0.2.3"]
                 [io.forward/clojure-mail "1.0.8"]
                 [com.taoensso/encore "3.23.0"]
                 [io.github.nextjournal/clerk "0.12.707"]
                 [com.taoensso/timbre "5.2.1"]
                 [org.slf4j/log4j-over-slf4j "1.7.36"]
                 [org.slf4j/jul-to-slf4j "1.7.36"]
                 [org.slf4j/jcl-over-slf4j "1.7.36"]
                 [com.fzakaria/slf4j-timbre "0.3.21"]
                 [byte-streams "0.2.4"]
                 [twitter-api "1.8.0"]
                 [com.draines/postal "2.0.5"]
                 [clj-stacktrace "0.2.8"]
                 [ring/ring-core "1.9.6"]
                 [ring/ring-devel "1.9.6"]
                 [ring/ring-jetty-adapter "1.9.6"]
                 [ring/ring-json "0.5.1"]
                 [compojure "1.7.0"]
                 [prismatic/schema-generators "0.1.5"]
                 [prismatic/schema "1.4.1"]
                 [org.clojure/test.check "1.1.1"]
                 [bk/ring-gzip "0.3.0"]
                 [mount "0.1.16"]
                 [robert/hooke "1.3.0"]
                 [clojure-opennlp "0.5.0"]
                 [com.novemberain/pantomime "2.11.0"]
                 [org.clojure/tools.cli "1.0.214"]
                 [com.firebase/firebase-client-jvm "2.5.2"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.postgresql/postgresql "42.5.1"]
                 [mpg "1.3.0"]
                 [clojure-humanize "0.2.2"]
                 [clojure.java-time "1.1.0"]
                 [com.clojure-goes-fast/clj-memory-meter "0.2.1"]
                 [org.bovinegenius/exploding-fish "0.3.6"]
                 [hikari-cp "3.0.1"]
                 [migratus "1.4.5"]
                 [mvxcvi/puget "1.3.4"]
                 [cc.artifice/clj-ml "0.8.7"]
                 [clj-commons/iapetos "0.1.13"]
                 [telegrambot-lib "2.3.0"]
                 [com.layerware/hugsql "0.5.3"]
                 [jarohen/chime "0.3.3"]])

