(defproject push4-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/test.check "0.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :repl-options {:init-ns push4-clj.core}
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]}}
  :plugins [[lein-jupyter "0.1.16"]]
  :main push4-clj.core)
