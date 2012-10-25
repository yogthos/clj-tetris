(defproject cljs-tetris "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :plugins [[lein-cljsbuild "0.2.9"]]
  
  :source-paths ["src"]
  ;;cljsbuild does not like cljs files being in the root source folder
  :cljsbuild {:crossovers [tetris.game]
              :builds 
              [{:source-path "src-cljs"
                :compiler 
                {:output-to "js/tetris.js"
                 :optimizations :advanced
                 :pretty-print false}}]})
