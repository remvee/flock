(defproject flock "1.0.0-SNAPSHOT"
  :description "Simulate a flock of.. whatever.."

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.8.40"]
                 [reagent "0.5.1"]]

  :profiles {:dev {:plugins [[lein-cljsbuild "1.1.3"]]}}

  :cljsbuild {:builds {:app {:source-paths ["src"]
                             :compiler {:output-to "resources/public/js/app.js"
                                        :output-dir "resources/public/js/app"
                                        :optimizations :advanced}}}})
