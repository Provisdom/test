(def project 'provisdom/test)
(def version "2")

(set-env! :resource-paths #{"src"}
          :source-paths #{"test"}
          :repositories #(conj % ["deploy-clojars" {:url      "https://clojars.org/repo"
                                                    :username (System/getenv "CLOJARS_USER")
                                                    :password (System/getenv "CLOJARS_PASSWORD")}])
          :dependencies '[[provisdom/boot-tasks "1.4" :scope "test"]
                          [adzerk/boot-test "1.2.0" :scope "test"]
                          [org.clojure/test.check "0.10.0-alpha2" :scope "test"]
                          [org.clojure/clojure "1.9.0" :scope "provided"]
                          [org.clojure/spec.alpha "0.1.143" :scope "provided"]])

(task-options!
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/Provisdom/test"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}})

(require '[adzerk.boot-test :refer [test]]
         '[provisdom.boot-tasks.core :refer [build auto-build]])

(deftask circle-deploy
  []
  (let [n (System/getenv "CIRCLE_BUILD_NUM")]
    (assert n "CIRCLE_BUILD_NUM not set.")
    (comp
     (pom :version (str version "." n))
     (jar)
     (push :repo "deploy-clojars"))))