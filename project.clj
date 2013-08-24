(defproject cellular "0.0.1-SNAPSHOT"
      :description "FIXME: write description"
      :dependencies [[org.clojure/clojure "1.5.1"]
                     [org.clojure/clojurescript "0.0-1847"]
                     [core.async "0.1.0-SNAPSHOT"]]
      :min-lein-version "2.0.0"
      :source-paths ["src/clj" "src/cljs"]
      
      :profiles {:dev {:plugins [[com.cemerick/austin "0.1.0"]]}}

      :plugins [[lein-cljsbuild "0.3.0"]
                [lein-simpleton "1.1.0"]]

      :cljsbuild {:builds [{:source-paths ["src/cljs" "src/clj"]
                            :compiler {:output-to "resources/public/build/deps.js"
                                       :output-dir "resources/public/build"
                                     ;  :pretty-print true
                                    ;   :optimizations :advanced}}]})
                                       :optimizations :none}}]})
