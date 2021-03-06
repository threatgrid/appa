(defproject org.clojars.quoll/appa "0.1.0"
  :description "JSON processing library for Asami"
  :url "http://github.org/quoll/appa"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [instaparse "1.4.10"]
                 [cheshire "5.8.1"]
                 [org.clojars.quoll/asami "0.3.3"]
                 [org.clojars.quoll/qtest "0.1.0"]]
  :repl-options {:init-ns appa.core}
  :resource-paths ["resources"])
