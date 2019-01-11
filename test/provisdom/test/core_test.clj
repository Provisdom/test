(ns provisdom.test.core-test
  (:require
    [clojure.test :refer :all]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [provisdom.test.core :as t]))

(deftest test-macro-expansions
  (are [expected-form quoted-form] (= expected-form (macroexpand-1 quoted-form))
    `(is (~'= 1 1) nil) `(t/is= 1 1 nil)
    `(is (~'not false) "some message") `(t/is-not false "some message")))

(deftest t-midje-just
  (are [e a] (t/midje-just e a)
    [1 1 1] [1 1 1]
    [1 1 #(and (number? %) (not (== % %)))] [1 1 Double/NaN])
  (are [e a] (not (t/midje-just e a))
    [1 1 1] [1 1 1.0]))

(defn my-add
  [x y]
  (str (+ x y)))

(s/fdef my-add
        :args (s/cat :x number? :y number?)
        :ret string?)

(t/defspec-test test-my-add `my-add)

(def opts {:test-check {:num-tests 10}})

(deftest test-spec-check-assert
  (is (spec-check my-add {:test-check {:num-tests 10}}))
  (is (spec-check my-add opts))
  (t/with-spec-check-opts {:test-check {:num-tests 10}}
    (is (spec-check my-add)))
  (t/with-spec-check-opts opts
    (is (spec-check my-add)))
  (is (spec-check my-add {:coll-check-limit 10
                          :coll-error-limit 10
                          :fspec-iterations 10
                          :recursion-limit  1
                          :test-check       {:num-tests 10}})))

(deftest data-to-paths-test
  (let [f #'t/data-to-paths]
    (is (= {[:a 0]  1.0
            [:b :c] 1.0
            [:set]  #{1 2 3}}
           (f {:a   [1.0]
               :b   {:c 1.0}
               :set #{1 2 3}}))
        "map expansion")
    (is (= {[0] 1.0
            [1] 2.0}
           (f [1.0 2.0]))
        "coll expansion")
    (is (= {[] 1.0}
           (f 1.0))
        "not coll or map expansion")))

(deftest approx=-test
  (is (t/approx= 1.0 1.0))
  (is (t/approx= 1.0 1.001 1e-2))
  (is (not (t/approx= 1.0 1.01 1e-2))))

(deftest data-approx=-test
  (is (t/data-approx=
        {:a 1.0
         :b #{1.0}}
        {:a 1.0000001
         :b #{1.0}}))
  (is (t/data-approx=
        {:a 1.0}
        {:a 1.001}
        {:tolerance 1e-2}))
  (is (not (t/data-approx= {:a 1} {:b 1})))
  (is (not (t/data-approx=
             {:a 1.0}
             {:a 1.01}))))