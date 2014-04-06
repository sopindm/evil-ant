(defproject evil-ant "0.1.0-SNAPSHOT"
  :description "Clojure event system"
  :url "http://devilin.net/evil-ant/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[io.tomw/lein-scalac "0.1.2"]]
  :profiles {:dev {:dependencies [[khazad-dum "0.2.0"]]
                   :repl-options {:init (use 'khazad-dum)}}}
  :source-paths ["src/"]
  :prep-tasks ["scalac"]
  :scala-source-path "scala"
  :scala-version "2.10.4"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.scala-lang/scala-library "2.10.4"]])

