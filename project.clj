(defproject provisdom/test "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "0.9.0"]
                 [midje "1.8.3" :exclusions [org.clojure/clojure]]
                 [criterium "0.4.3"]
                 [incanter "1.9.0"]])
