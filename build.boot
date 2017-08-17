(def project 'provisdom/test)
(def version "0.3.2")

(set-env! :resource-paths #{"src"}
          :source-paths #{"test"}
          :dependencies '[[provisdom/boot-tasks "1.2" :scope "test"]
                          [adzerk/boot-test "1.2.0" :scope "test"]
                          [org.clojure/clojure "1.9.0-alpha17" :scope "provided"]
                          [org.clojure/spec.alpha "0.1.109"]
                          [incanter "1.5.7"]
                          [criterium "0.4.4"]
                          [midje "1.9.0-alpha6" :exclusions [org.clojure/clojure]]
                          [org.clojure/test.check "0.9.0"]])

(task-options!
  pom {:project     project
       :version     version
       :description "FIXME: write description"
       :url         "http://example/FIXME"
       :scm         {:url "https://github.com/Provisdom/test"}
       :license     {"Eclipse Public License"
                     "http://www.eclipse.org/legal/epl-v10.html"}})

(require '[adzerk.boot-test :refer [test]]
         '[provisdom.boot-tasks.core :refer [build auto-build push-jar]])