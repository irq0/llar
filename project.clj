(defproject llar "_"
  :profiles {:uberjar {:omit-source true
                       :aot :all}}
  :description "LLAR - Live Long and Read 🖖"
  :url "https://llar.dev"
  :jvm-opts ["-Xmx4g"
             "-server"
             "-XX:-OmitStackTraceInFastThrow"
             "-XX:+TieredCompilation"
             "-XX:TieredStopAtLevel=1"
             "-Djdk.attach.allowAttachSelf=true"
             "-Dtika.config=tika-config.xml"
             "-Dclojure.tools.logging.factory=clojure.tools.logging.impl/log4j2-factory"]

  :aot :all
  :main llar.core
  :plugins [[lein-cljfmt "0.6.8"] [me.arrdem/lein-git-version "2.0.8"] [lein-pprint "1.3.2"]]
  :license {:name "AGPL-3.0-or-later"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html"
            :distribution :repo}
  :git-version {:status-to-version
                (fn [{:keys [tag ref-short ahead ahead? dirty?]}]
                  (assert (re-find #"\d+\.\d+\.\d+" tag)
                          "Tag is assumed to be a raw SemVer version")
                  (letfn [(release? [] (and tag (not ahead?) (not dirty?)))
                          (snapshot-version []
                            (let [[major minor patch] (->> tag
                                                           (re-find #"(\d+)\.(\d+)\.(\d+)")
                                                           (drop 1)
                                                           (map #(Long/parseLong %)))
                                  patch+ (inc patch)]
                              (format "%d.%d.%d-SNAPSHOT.%d+%s" major minor patch+ ahead ref-short)))]
                    (if (release?) tag (snapshot-version))))

                :version-file "resources/version.edn"
                :version-file-keys [:ref :version :timestamp :ref-short :tag]}

  :exclusions [org.slf4j/slf4j-nop]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 ;; logging
                 [org.clojure/tools.logging "1.3.0"]
                 [org.slf4j/slf4j-api "2.0.16"]
                 [org.slf4j/jcl-over-slf4j "2.0.16"]
                 [org.slf4j/jul-to-slf4j "2.0.16"]
                 [org.slf4j/osgi-over-slf4j "2.0.16"]

                 [org.apache.logging.log4j/log4j-slf4j2-impl "2.24.3"]
                 [org.apache.logging.log4j/log4j-core "2.24.3"]
                 [org.apache.logging.log4j/log4j-api "2.24.3"]

                 ;; leaf node deps
                 [org.clojure/data.priority-map "1.2.0"]
                 [org.clojure/tools.analyzer.jvm "1.3.1"]
                 [org.clojure/data.xml "0.2.0-alpha8"]
                 [org.clojure/data.json "2.5.1"]
                 [org.clojure/test.check "1.1.1"]
                 [org.clojure/tools.cli "1.1.230"]
                 [dev.weavejester/medley "1.8.1"]
                 [com.fasterxml.jackson.core/jackson-databind "2.18.2"]
                 [org.jsoup/jsoup "1.18.3"]
                 [org.apache.httpcomponents/httpcore "4.4.16"]
                 [org.bouncycastle/bcprov-jdk15on "1.70"]
                 [org.bouncycastle/bcpkix-jdk15on "1.70"]
                 [javax.activation/activation "1.1.1"]
                 [net.java.dev.jna/jna "5.15.0"]
                 [riddley "0.2.0"]
                 [cheshire "5.13.0"]
                 [bk/ring-gzip "0.3.0"]
                 [org.tukaani/xz "1.10"]
                 [table "0.5.0"]
                 [nio2 "0.2.3"]
                 [mount "0.1.20"]
                 [tolitius/mount-up "0.1.3"]
                 [digest "1.4.10"]
                 [clj-stacktrace "0.2.8"]
                 [clojure.java-time "1.4.3"]
                 [jarohen/chime "0.3.3" :exclusions [org.clojure/tools.logging]]
                 [slingshot "0.12.2"]
                 [io.github.clj-kondo/config-slingshot-slingshot "1.0.0"]
                 [org.apache.commons/commons-text "1.12.0"]
                 [commons-io/commons-io "2.18.0"]
                 [com.sun.activation/jakarta.activation "2.0.1"]
                 [com.google.guava/guava "31.1-jre"]
                 [cider/cider-nrepl "0.50.3"]
                 [potemkin "0.4.7" :exclusions [riddley]]
                 [mvxcvi/puget "1.3.4"]
                 [nrepl "1.3.0"]
                 [djblue/portal "0.59.2"]
                 [org.clojure/core.async "1.6.681" :exclusions [org.clojure/data.priority-map org.clojure/tools.analyzer.jvm org.tukaani/xz]]
                 [byte-streams "0.2.4" :exclusions [riddley]]
                 [com.nextjournal/beholder "1.0.2"]

                 ;; schema
                 [prismatic/schema "1.4.1"]
                 [prismatic/schema-generators "0.1.5"]

                 ;; monitoring
                 [io.prometheus/simpleclient_hotspot "0.16.0"]
                 [clj-commons/iapetos "0.1.14"]
                 [com.clojure-goes-fast/clj-memory-meter "0.3.0"]

                 ;; apis
                 [twitter-api "1.8.0" :exclusions [org.clojure/data.json org.bouncycastle/bcprov-jdk15on]]
                 [clj-http "3.13.0" :exclusions [org.apache.httpcomponents/httpcore]]
                 [clj-rome "0.4.0"]
                 [clj-http-fake "1.0.4"]

                 ;; email
                 [com.draines/postal "2.0.5"]
                 [io.forward/clojure-mail "1.0.8" :exclusions [medley javax.activation/activation]]

                 ;; database
                 [mpg "1.3.0" :exclusions [chesire]]
                 [com.layerware/hugsql "0.5.3"]
                 [org.postgresql/postgresql "42.7.4"]
                 [hikari-cp "3.2.0" :exclusions [org.slf4j/slf4j-api]]
                 [org.clojure/java.jdbc "0.7.12"]
                 [migratus "1.6.3" :exclusions [org.clojure/tools.logging]]

                 ;; webapp
                 [ring/ring-core "1.13.0"  :exclusions [commons-io]]
                 [ring/ring-devel "1.13.0"  :exclusions [commons-io]]
                 [ring/ring-jetty-adapter "1.13.0" :exclusions [commons-io]]
                 [ring/ring-json "0.5.1" :exclusions [cheshire]]
                 [compojure "1.7.1" :exclusions [commons-io medley]]

                 ;; html
                 [hiccup "1.0.5"]
                 [hickory "0.7.1" :exclusions [org.jsoup/jsoup]]
                 [org.bovinegenius/exploding-fish "0.3.6"]

                 ;; data processing and analysis
                 [clojure-opennlp "0.5.0"]
                 [org.apache.tika/tika-parsers-standard-package "3.0.0" :exclusions [org.ow2.asm/asm commons-io]]
                 [org.apache.tika/tika-langdetect-optimaize "3.0.0"]
                 [org.apache.tika/tika-core "3.0.0" :exclusions [commons-io]]
                 [cc.artifice/clj-ml "0.8.7" :exclusions [org.clojure/data.xml]]])
