(def project 'provisdom/test)
(def version "0.2.2-SNAPSHOT")

(set-env! :resource-paths #{"src"}
          :source-paths #{"test"}
          :dependencies '[[provisdom/boot-tasks "0.7.0" :scope "test"]
                          [adzerk/boot-test "1.1.1" :scope "test"]
                          [incanter "1.5.7" :scope "test"]
                          [org.clojure/test.check "0.9.0" :scope "test"]
                          [org.clojure/clojure "1.8.0" :scope "provided"]])

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