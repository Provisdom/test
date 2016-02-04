(defproject provisdom/test "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "(c) 2016 Provisdom Corporation"
            :url  "http://www.provisdom.com"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "0.9.0"]
                 [midje "1.8.3" :exclusions [org.clojure/clojure]]
                 [criterium "0.4.3"]
                 [incanter "1.5.7"]])
