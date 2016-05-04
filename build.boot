(def project 'provisdom/test)
(def version "0.2.0")

(set-env! :resource-paths #{"resources" "src" "test"}
          :dependencies '[[org.clojure/clojure "1.8.0"]
                          [provisdom/boot-tasks "0.6.0" :scope "test"]
                          [adzerk/boot-test "1.1.1" :scope "test"]
                          [incanter "1.5.7"]])

(task-options!
  aot {:namespace #{'provisdom.test.core}}
  pom {:project     project
       :version     version
       :description "FIXME: write description"
       :url         "http://example/FIXME"
       :scm         {:url "https://github.com/Provisdom/test"}
       :license     {"Eclipse Public License"
                     "http://www.eclipse.org/legal/epl-v10.html"}})

(require '[adzerk.boot-test :refer [test]]
         '[provisdom.boot-tasks.core :refer [release build]])