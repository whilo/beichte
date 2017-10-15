(defproject beichte "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.analyzer "0.6.7"]
                 [org.clojure/tools.reader "1.0.5"]
                 [org.clojure/core.cache "0.6.5"]]
  :plugins [[lein-environ "1.0.0"]]
  :env {:squiggly {:checkers [:eastwood]
                   :eastwood-exclude-linters [:unlimited-use]}})
