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
             "-Djdk.attach.allowAttachSelf=true"
             "-Dtika.config=tika-config.xml"
             "-Dclojure.tools.logging.factory=clojure.tools.logging.impl/log4j2-factory"]

  :aot :all
  :main u1f596.core
  :plugins [[lein-cljfmt "0.6.8"]]
  :license {:name "None"
            :url ""}
  :exclusions [org.slf4j/slf4j-nop]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 ;; logging
                 [org.clojure/tools.logging "1.2.4"]
                 [org.slf4j/slf4j-api "2.0.5"]
                 [org.slf4j/jcl-over-slf4j "2.0.5"]
                 [org.slf4j/jul-to-slf4j "2.0.5"]
                 [org.slf4j/osgi-over-slf4j "2.0.5"]

                 [org.apache.logging.log4j/log4j-slf4j2-impl "2.19.0"]
                 [org.apache.logging.log4j/log4j-core "2.19.0"]
                 [org.apache.logging.log4j/log4j-api "2.19.0"]

                 ;; leaf node deps
                 [org.clojure/data.priority-map "1.1.0"]
                 [org.clojure/tools.analyzer.jvm "1.2.3"]
                 [org.clojure/data.xml "0.2.0-alpha8"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/test.check "1.1.1"]
                 [org.clojure/tools.cli "1.0.214"]
                 [dev.weavejester/medley "1.5.0"]
                 [com.fasterxml.jackson.core/jackson-databind "2.14.1"]
                 [org.jsoup/jsoup "1.15.3"]
                 [org.apache.httpcomponents/httpcore "4.4.16"]
                 [org.bouncycastle/bcprov-jdk15on "1.70"]
                 [org.bouncycastle/bcpkix-jdk15on "1.70"]
                 [javax.activation/activation "1.1.1"]
                 [net.java.dev.jna/jna "5.12.1"]
                 [riddley "0.2.0"]
                 [cheshire "5.11.0"]
                 [bk/ring-gzip "0.3.0"]
                 [org.tukaani/xz "1.9"]
                 [table "0.5.0"]
                 [nio2 "0.2.3"]
                 [mount "0.1.16"]
                 [digest "1.4.10"]
                 [clj-stacktrace "0.2.8"]
                 [clojure.java-time "1.1.0"]
                 [jarohen/chime "0.3.3" :exclusions [org.clojure/tools.logging]]
                 [robert/hooke "1.3.0"]
                 [slingshot "0.12.2"]
                 [org.apache.commons/commons-text "1.10.0"]
                 [commons-io/commons-io "2.11.0"]
                 [com.sun.activation/jakarta.activation "2.0.1"]
                 [com.google.guava/guava "31.1-jre"]
                 [cider/cider-nrepl "0.29.0"]
                 [potemkin "0.4.6" :exclusions [riddley]]
                 [mvxcvi/puget "1.3.4"]
                 [nrepl "1.0.0"]
                 [org.clojure/core.async "1.6.673" :exclusions [org.clojure/data.priority-map org.clojure/tools.analyzer.jvm org.tukaani/xz]]
                 [byte-streams "0.2.4" :exclusions [riddley]]
                 
                 ;; schema
                 [prismatic/schema "1.4.1"]
                 [prismatic/schema-generators "0.1.5"]

                 ;; monitoring
                 [clj-commons/iapetos "0.1.13"]
                 [com.clojure-goes-fast/clj-memory-meter "0.2.1"]

                 ;; apis
                 [telegrambot-lib "2.3.0" :exclusions [org.clojure/data.priority-map org.clojure/tools.logging]]
                 [twitter-api "1.8.0" :exclusions [org.clojure/data.json org.bouncycastle/bcprov-jdk15on]]
                 [clj-http "3.12.3" :exclusions [org.apache.httpcomponents/httpcore]]
                 [clj-rome "0.4.0"]
                 [com.firebase/firebase-client-jvm "2.5.2" :exclusions [com.fasterxml.jackson.core/jackson-databind]]

                 ;; email
                 [com.draines/postal "2.0.5"]
                 [io.forward/clojure-mail "1.0.8" :exclusions [medley javax.activation/activation]]
                 
                 ;; database
                 [mpg "1.3.0" :exclusions [chesire]]
                 [com.layerware/hugsql "0.5.3"]
                 [org.postgresql/postgresql "42.5.1"]
                 [hikari-cp "3.0.1" :exclusions [org.slf4j/slf4j-api]]
                 [org.clojure/java.jdbc "0.7.12"]
                 [migratus "1.4.5" :exclusions [org.clojure/tools.logging]]

                 ;; webapp
                 [ring/ring-core "1.9.6"  :exclusions [commons-io]]
                 [ring/ring-devel "1.9.6"  :exclusions [commons-io]]
                 [ring/ring-jetty-adapter "1.9.6" :exclusions [commons-io]]
                 [ring/ring-json "0.5.1" :exclusions [cheshire]]
                 [compojure "1.7.0" :exclusions [commons-io medley]]

                 ;; html
                 [hiccup "1.0.5"]
                 [hickory "0.7.1" :exclusions [org.jsoup/jsoup]]
                 [org.bovinegenius/exploding-fish "0.3.6"]

                 ;; data processing and analysis
                 [clojure-opennlp "0.5.0"]
                 [org.apache.tika/tika-parsers-standard-package "2.6.0" :exclusions [org.ow2.asm/asm commons-io]]
                 [org.apache.tika/tika-langdetect-optimaize "2.6.0"]
                 [org.apache.tika/tika-core "2.6.0" :exclusions [commons-io]]
                 [cc.artifice/clj-ml "0.8.7" :exclusions [org.clojure/data.xml]]



