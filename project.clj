(defproject cellular "0.0.1-SNAPSHOT"
      :description "FIXME: write description"
      :dependencies [[org.clojure/clojure "1.5.1"]
                     [org.clojure/clojurescript "0.0-1847"]
                     [core.async "0.1.0-SNAPSHOT"]
                     [net.mikera/imagez "0.0.3"]
                     [net.mikera/mikera-gui "0.1.0"]
                     [fipp "0.4.0"]]
      :min-lein-version "2.0.0"
      :source-paths ["src/clj" "target/generated/clj"]
      
      :plugins [[lein-cljsbuild "0.3.0"]
                [lein-simpleton "1.1.0"]
                [com.keminglabs/cljx "0.3.0"]]
      
      :cljx {:builds [{:source-paths ["src/cljx"]
                       :output-path "target/generated/clj"
                       :rules :clj}
                      
                      {:source-paths ["src/cljx"]
                       :output-path "target/generated/cljs"
                       :rules :cljs}]}
      
      :hooks [cljx.hooks]

      :cljsbuild {:builds [{:source-paths ["src/cljs" "target/generated/cljs"]
                            :compiler {:output-to "resources/public/build/deps.js"
                                       :output-dir "resources/public/build"
                                     ;  :pretty-print true
                                     ;  :optimizations :advanced}}]})
                                     ;  :optimizations :simple}}]})
                                       :optimizations :none}}]})
