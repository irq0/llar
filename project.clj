(defproject llar "_"
  :profiles {:uberjar {:omit-source false
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
  ;; Note: build uberjar before lein run
  :dependencies [[org.clojure/clojure "1.12.4"]
                 ;; logging
                 [org.clojure/tools.logging "1.3.1"]
                 [org.slf4j/slf4j-api "2.0.17"]
                 [org.slf4j/jcl-over-slf4j "2.0.17"]
                 [org.slf4j/jul-to-slf4j "2.0.17"]
                 [org.slf4j/osgi-over-slf4j "2.0.17"]

                 [org.apache.logging.log4j/log4j-slf4j2-impl "2.25.3"]
                 [org.apache.logging.log4j/log4j-core "2.25.3"]
                 [org.apache.logging.log4j/log4j-api "2.25.3"]

                 ;; leaf node deps
                 [org.clojure/data.priority-map "1.2.1"]
                 [org.clojure/tools.analyzer.jvm "1.3.4"]
                 [org.clojure/data.xml "0.2.0-alpha8"]
                 [org.clojure/data.json "2.5.2"]
                 [org.clojure/test.check "1.1.3"]
                 [org.clojure/tools.cli "1.3.250"]
                 [org.clojure/tools.reader "1.6.0"]
                 [dev.weavejester/medley "1.9.0"]
                 [com.fasterxml.jackson.core/jackson-databind "2.21.0"]
                 [org.jsoup/jsoup "1.22.1"]
                 [org.apache.httpcomponents/httpcore "4.4.16"]
                 [org.bouncycastle/bcpkix-jdk18on "1.82"]
                 [org.bouncycastle/bcprov-jdk18on "1.82"]
                 [org.bouncycastle/bcutil-jdk18on "1.82"]
                 [javax.activation/activation "1.1.1"]
                 [net.java.dev.jna/jna "5.18.1"]
                 [riddley "0.2.0"]
                 [cheshire "6.1.0"]
                 [bk/ring-gzip "0.3.0"]
                 [org.tukaani/xz "1.11"]
                 [table "0.5.0"]
                 [nio2 "0.2.3"]
                 [mount "0.1.23"]
                 [tolitius/mount-up "0.1.3"]
                 [digest "1.4.10"]
                 [clj-stacktrace "0.2.8"]
                 [clojure.java-time "1.4.3"]
                 [jarohen/chime "0.3.3" :exclusions [org.clojure/tools.logging]]
                 [slingshot "0.12.2"]
                 [io.github.clj-kondo/config-slingshot-slingshot "1.0.0"]
                 [org.apache.commons/commons-text "1.15.0"]
                 [commons-io/commons-io "2.21.0"]
                 [com.sun.activation/jakarta.activation "2.0.1"]
                 [com.google.guava/guava "31.1-jre"]
                 [cider/cider-nrepl "0.58.0"]
                 [potemkin "0.4.9" :exclusions [riddley]]
                 [mvxcvi/puget "1.3.4"]
                 [nrepl "1.5.2"]
                 [djblue/portal "0.62.2"]
                 [org.clojure/core.async "1.8.741" :exclusions [org.clojure/data.priority-map org.clojure/tools.analyzer.jvm org.tukaani/xz]]
                 [byte-streams "0.2.4" :exclusions [riddley]]
                 [com.nextjournal/beholder "1.0.3"]

                 ;; schema
                 [prismatic/schema "1.4.1"]
                 [prismatic/schema-generators "0.1.5"]

                 ;; monitoring
                 [io.prometheus/simpleclient_hotspot "0.16.0"]
                 [clj-commons/iapetos "0.1.14"]
                 [com.clojure-goes-fast/clj-memory-meter "0.4.0"]

                 ;; apis
                 [twitter-api "1.8.0" :exclusions [org.clojure/data.json org.bouncycastle/bcprov-jdk15on]]
                 [clj-http "3.13.1" :exclusions [org.apache.httpcomponents/httpcore]]
                 [clj-rome "0.4.0"]
                 [clj-http-fake "1.0.4"]

                 ;; email
                 [com.draines/postal "2.0.5"]
                 [io.forward/clojure-mail "1.0.8" :exclusions [medley javax.activation/activation]]

                 ;; database
                 [com.layerware/hugsql-core "0.5.3"]
                 [com.layerware/hugsql-adapter-next-jdbc "0.5.3"]
                 [com.github.seancorfield/next.jdbc "1.2.659"]

                 [hikari-cp "4.0.0" :exclusions [org.slf4j/slf4j-api]]
                 [org.postgresql/postgresql "42.7.9"]
                 [migratus "1.6.5" :exclusions [org.clojure/tools.logging]]

                 ;; webapp
                 [ring/ring-core "1.15.3"  :exclusions [commons-io]]
                 [ring/ring-devel "1.15.3"  :exclusions [commons-io]]
                 [ring/ring-jetty-adapter "1.15.3" :exclusions [commons-io]]
                 [ring/ring-json "0.5.1" :exclusions [cheshire]]
                 [ring/ring-codec "1.3.0"]
                 [compojure "1.7.2" :exclusions [commons-io medley]]

                 ;; html
                 [hiccup "2.0.0"]
                 [hickory "0.7.1" :exclusions [org.jsoup/jsoup]]
                 [org.bovinegenius/exploding-fish "0.3.6"]

                 ;; data processing and analysis
                 [clojure-opennlp "0.5.0"]
                 [org.apache.tika/tika-parsers-standard-package "3.2.3" :exclusions [org.bouncycastle/bcpkix-jdk18on org.bouncycastle/bcprov-jdk18on org.bouncycastle/bcutil-jdk18on]]
                 [org.apache.tika/tika-langdetect-optimaize "3.2.3"]
                 [org.apache.tika/tika-core "3.2.3" :exclusions [commons-io]]
                 [cc.artifice/clj-ml "0.8.7" :exclusions [org.clojure/data.xml]]])
