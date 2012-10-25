(defproject tetris "0.1.0"
  :description "a simple Tetris game"
  :url "https://github.com/yogthos/Clojure-Tetris"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.542"]]
  :plugins [[lein-cljsbuild "1.1.6"]]

  :main tetris.core

  :cljsbuild {:builds
              [{:compiler
                {:output-to "js/tetris.js"
                 :optimizations :advanced
                 :pretty-print false}
                :source-paths ["src"]}]})
