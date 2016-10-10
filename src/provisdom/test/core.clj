(ns provisdom.test.core
  (:require [clojure.test :refer [deftest is are try-expr assert-expr do-report]]))

(defmacro is=
  [expected actual]
  `(is (= ~expected ~actual)))

(defmacro is-not
  ([form] `(is-not ~form nil))
  ([form msg] `(is (not ~form) ~msg)))

(defn midje-just
  [expected actual]
  (if (= (count expected) (count actual))
    (every? true?
            (map (fn [e a]
                   (if (fn? e)
                     (e a)
                     (= e a))) expected actual))
    false))

(defmethod assert-expr 'just
  [msg form]
  `(let [expected# ~(nth form 1)
         actual# ~(nth form 2)
         result# (midje-just expected# actual#)]
     (if result#
       (do-report {:type     :pass
                   :message  ~msg
                   :expected '~form
                   :actual   actual#})
       (do-report {:type     :fail
                   :message  ~msg
                   :expected '~(nth form 1)
                   :actual   actual#}))
     result#))