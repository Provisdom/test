(ns provisdom.test.core
  (:require [clojure.test :as t]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

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
                  :let [r# (clojure.spec.test.alpha/abbrev-result failed-check#)
                        failure# (:failure r#)]]
            (t/do-report
              {:type     :fail
               :message  (with-out-str (clojure.spec.alpha/explain-out failure#))
               :expected (->> r# :spec rest (apply hash-map) :ret)
               :actual   (if (instance? Throwable failure#)
                           failure#
                           (:clojure.spec.test/val failure#))})))
        checks-passed?#))))