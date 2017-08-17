(ns provisdom.test.core
  (:require
    [clojure.test :as t]
    [clojure.string :as str]
    [clojure.spec.alpha :as s]))

(defmacro is=
  ([expected actual] `(is= ~expected ~actual nil))
  ([expected actual msg]
   `(t/is (~'= ~expected ~actual) ~msg)))

(defmacro is-not
  ([form] `(is-not ~form nil))
  ([form msg] `(t/is (~'not ~form) ~msg)))

(defn midje-just
  [expected actual]
  (if (= (count expected) (count actual))
    (every? true?
            (map (fn [e a]
                   (if (fn? e)
                     (e a)
                     (= e a))) expected actual))
    false))

(defmethod t/assert-expr 'just
  [msg form]
  `(let [expected# ~(nth form 1)
         actual# ~(nth form 2)
         result# (midje-just expected# actual#)]
     (if result#
       (t/do-report {:type     :pass
                     :message  ~msg
                     :expected '~form
                     :actual   actual#})
       (t/do-report {:type     :fail
                     :message  ~msg
                     :expected '~(nth form 1)
                     :actual   actual#}))
     result#))

(defmacro defspec-test
  ([name sym-or-syms] `(defspec-test ~name ~sym-or-syms nil))
  ([name sym-or-syms opts]
   `(t/deftest ~name
      (let [check-results# (clojure.spec.test.alpha/check ~sym-or-syms ~opts)
            checks-passed?# (every? nil? (map :failure check-results#))]
        (if checks-passed?#
          (t/do-report {:type    :pass
                        :message (str "Generative tests pass for "
                                      (str/join ", " (map :sym check-results#)))})
          (doseq [failed-check# (filter :failure check-results#)
                  :let [sym# (:sym failed-check#)
                        abbrev# (clojure.spec.test.alpha/abbrev-result failed-check#)
                        failure# ^Throwable (:failure abbrev#)
                        m# {:type    :fail
                            :message (str "generative Spec tests in " sym# " failed.\n")}]]
            (t/do-report
              (if (instance? Throwable failure#)
                (-> m#
                    (assoc :expected (->> abbrev# :spec rest (apply hash-map) :ret)
                           :actual failure#)
                    (update :message str (str (.getCause failure#))))
                (let [data# (ex-data (:failure failed-check#))
                      expected# (get-in data# [::s/problems 0 :pred])
                      actual# (get-in data# [::s/problems 0 :val])]
                  (-> m#
                      (assoc :expected expected#
                             :actual actual#)))))))
        checks-passed?#))))