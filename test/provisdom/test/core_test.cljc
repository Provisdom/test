(ns provisdom.test.core-test
  (:require
    [clojure.test :refer [deftest are is testing]]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [provisdom.test.core :as t])
  #?(:clj (:import (clojure.lang ExceptionInfo))))

;(deftest test-macro-expansions
;  (are [expected-form quoted-form] (= expected-form (macroexpand-1 quoted-form))
;    `(is (~'= 1 1) nil) `(t/is= 1 1 nil)
;    `(is (~'not false) "some message") `(t/is-not false "some message")))
;
;(deftest t-midje-just
;  (are [e a] (t/midje-just e a)
;    [1 1 1] [1 1 1]
;    [1 1 #(and (number? %) (not (== % %)))] [1 1 Double/NaN])
;  (are [e a] (not (t/midje-just e a))
;    [1 1 1] [1 1 1.0]))

(defn my-add
  [x y]
  (str (+ x y)))

(s/fdef my-add
  :args (s/cat :x (s/int-in 0 100) :y (s/int-in 0 100))
  :ret string?)
;
;(defn return-does-not-pass-spec
;  [x]
;  x)
;
;(s/fdef return-does-not-pass-spec
;  :args (s/cat :x (s/int-in 0 100))
;  :ret string?)
;
;(comment
;  (deftest return-does-not-pass-spec-test
;    (is (spec-check return-does-not-pass-spec)))
;  )
;
;
;(defn gen-throws-exception
;  [x]
;  x)
;
;(s/fdef gen-throws-exception
;  :args (s/cat :x (s/with-gen (s/int-in 0 100)
;                    (fn []
;                      (throw (ex-info "fail" {})))))
;  :ret string?)
;
;(comment
;  (deftest gen-throws-exception-test
;    (is (spec-check gen-throws-exception)))
;  )
;
;
;(comment
;  (deftest return-does-not-pass-spec-test
;    (is (spec-check return-does-not-pass-spec)))
;  )
;
;
;(defn throws-exception
;  [x]
;  (throw (ex-info "i throw" {}))
;  x)
;
;(s/fdef throws-exception
;  :args (s/cat :x (s/int-in 0 100))
;  :ret int?)
;
;(comment
;  (deftest throws-exception-test
;    (is (spec-check throws-exception)))
;  )
;
;
;(defn cannot-satisfy-such-that
;  [x y]
;  (str (+ x y)))
;
;(s/fdef cannot-satisfy-such-that
;  :args (s/and (s/cat :x (s/int-in 0 100) :y (s/int-in 0 100))
;               (fn [{:keys [x y]}]
;                 (= x (/ (inc y) 10))))
;  :ret string?)
;
;(deftest function-instrumented?-test
;  (st/unstrument `my-add)
;  (st/instrument `my-add)
;  (is (= true (t/function-instrumented? `my-add)))
;  (st/unstrument `my-add)
;  (is (= false (t/function-instrumented? `my-add))))

#?(:clj (deftest with-instrument-test
          (testing "started instrumented"
            (st/instrument `my-add)
            (t/with-instrument `my-add
                               (my-add 1 2))
            (is (thrown?  ExceptionInfo (my-add 1 "a")))
            (st/unstrument `my-add))
          (testing "started uninstrumented"
            (t/with-instrument `my-add
                               (is (thrown? ExceptionInfo) (my-add 1 "a")))
            (is (thrown? ClassCastException (my-add 1 "a"))))))

(deftest instrumentation-test
  (with-open [_ (t/instrumentation {:instrument [`my-add]})]
    (is (thrown? ExceptionInfo (my-add 1 "a"))))
  (is (thrown? ClassCastException (my-add 1 "a"))))
;
;
;(t/defspec-test test-my-add `my-add)
;
;(deftest test-spec-check-assert
;  (is (spec-check my-add {:test-check {:num-tests 10}}))
;  (is (spec-check my-add {:test-check {:num-tests 10}}))
;  (t/with-spec-check-opts
;    {:test-check {:num-tests 10}}
;    (is (spec-check my-add)))
;  (is (spec-check my-add {:coll-check-limit 10
;                          :coll-error-limit 10
;                          :fspec-iterations 10
;                          :recursion-limit  1
;                          :test-check       {:num-tests 10}})))
;
;(deftest data-to-paths-test
;  (let [f #'t/data-to-paths]
;    (is (= {[:a 0]  1.0
;            [:b :c] 1.0
;            [:set]  #{1 2 3}}
;           (f {:a   [1.0]
;               :b   {:c 1.0}
;               :set #{1 2 3}}))
;        "map expansion")
;    (is (= {[0] 1.0
;            [1] 2.0}
;           (f [1.0 2.0]))
;        "coll expansion")
;    (is (= {[] 1.0}
;           (f 1.0))
;        "not coll or map expansion")))
;
;(deftest approx=-test
;  (is (t/approx= 1.0 1.0))
;  (is (t/approx= 1.0 1.001 1e-2))
;  (is (not (t/approx= 1.0 1.01 1e-2))))
;
;(deftest is-valid-test
;  (t/is-valid int? 1))
;
;(deftest data-approx=-test
;  (is (t/data-approx=
;        {:a 1.0
;         :b #{1.0}}
;        {:a 1.0000001
;         :b #{1.0}}))
;  (is (t/data-approx=
;        {:a 1.0}
;        {:a 1.001}
;        {:tolerance 1e-2}))
;  (is (not (t/data-approx= {:a 1} {:b 1})))
;  (is (not (t/data-approx=
;             {:a 1.0}
;             {:a 1.01}))))