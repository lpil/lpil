(defproject overtone-gentle-intro "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [overtone "0.9.1"]]
  :main ^:skip-aot overtone-gentle-intro.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
