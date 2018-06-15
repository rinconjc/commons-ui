(defproject commons-ui "0.1.0-SNAPSHOT"
  :description "basic ui components for reagent"
  :url "http://github.com/rinconjc/commons-ui"
  :clojurescript? true
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]
                 [reagent "0.6.0"]
                 [org.clojure/core.async "0.3.442"]]
  :jvm-opts ^:replace ["-Xmx1g" "-server"]
  :plugins [[lein-npm "0.6.1"]]
  :profiles {:dev
             {:plugins [[lein-cljsbuild "1.0.3"]]}}
  :npm {:dependencies [[source-map-support "0.4.0"]]}
  :target-path "target")
