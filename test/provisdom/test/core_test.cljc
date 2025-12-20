(ns provisdom.test.core-test
  #?(:cljs (:require-macros [provisdom.test.core]))
  (:require
    [clojure.test :as ct]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [provisdom.test.core :as t])
  #?(:clj (:import (clojure.lang ExceptionInfo))))

(ct/deftest bind-spec-opts-unit-test
  (t/bind-spec-opts {:fspec-iterations 10}
    (ct/is (= 10 s/*fspec-iterations*))))

#?(:clj (ct/deftest test-macro-expansions
          (ct/are [expected-form quoted-form] (= expected-form (macroexpand-1 quoted-form))
            `(ct/is (~'= 1 1) nil) `(t/is= 1 1 nil)
            `(ct/is (~'not false) "some message") `(t/is-not false "some message"))))

(ct/deftest t-midje-just
  (ct/are [e a] (t/midje-just e a)
    [1 1 1] [1 1 1]
    [1 1 #(and (number? %) (not (== % %)))] [1 1 #?(:clj Double/NaN :cljs js/NaN)])
  (ct/are [e a] (not (t/midje-just e a))
    [1 1 1] [1 1 2]))

(defn my-add
  [x y]
  (str (+ x y)))

(s/fdef my-add
  :args (s/cat :x (s/int-in 0 100) :y (s/int-in 0 100))
  :ret string?)

(defn return-does-not-pass-spec
  [x]
  x)

(s/fdef return-does-not-pass-spec
  :args (s/cat :x (s/int-in 0 100))
  :ret string?)

(defn gen-throws-exception
  [x]
  x)

(s/fdef gen-throws-exception
  :args (s/cat :x (s/with-gen (s/int-in 0 100)
                    (fn []
                      (throw (ex-info "fail" {})))))
  :ret string?)

(defn throws-exception
  [x]
  (throw (ex-info "i throw" {}))
  x)

(s/fdef throws-exception
  :args (s/cat :x (s/int-in 0 100))
  :ret int?)

(defn cannot-satisfy-such-that
  [x y]
  (str (+ x y)))

(s/fdef cannot-satisfy-such-that
  :args (s/and (s/cat :x (s/int-in 0 100) :y (s/int-in 0 100))
          (fn [{:keys [x y]}]
            (= x (/ (inc y) 10))))
  :ret string?)

(ct/deftest function-instrumented?-test
  (st/unstrument `my-add)
  (st/instrument `my-add)
  (ct/is (= true (t/function-instrumented? `my-add)))
  (st/unstrument `my-add)
  (ct/is (= #?(:clj false :cljs true) (t/function-instrumented? `my-add))))

#?(:clj
   (ct/deftest with-instrument-test
     (ct/testing "started instrumented"
       (st/instrument `my-add)
       (t/with-instrument `my-add
         (my-add 1 2))
       (ct/is (thrown-with-msg? ExceptionInfo #"Call to" (my-add 1 "a")))
       (ct/is (= [`my-add] (st/unstrument `my-add)))
       (t/with-instrument :all
         (ct/is (= "3" (my-add 1 2)))))
     (ct/testing "started uninstrumented"
       (t/with-instrument `my-add
         (ct/is (thrown-with-msg? ExceptionInfo #"Call to" (my-add 1 "a"))))
       (ct/is (thrown? ClassCastException (my-add 1 "a"))))))

#?(:clj (ct/deftest instrumentation-test
          (with-open [_ (t/instrumentation {:instrument [`my-add]})]
            (ct/is (thrown? ExceptionInfo (my-add 1 "a"))))
          (ct/is (thrown? ClassCastException (my-add 1 "a")))))

(t/defspec-test test-my-add `my-add)

(ct/deftest test-spec-check-assert
  (ct/is (spec-check [my-add] {:test-check {:num-tests 10}}))
  (t/with-spec-check-opts
    {:test-check {:num-tests 10}}
    (ct/is (spec-check my-add)))
  (ct/is (spec-check my-add {:coll-check-limit 10
                          :coll-error-limit 10
                          :fspec-iterations 10
                          :recursion-limit  1
                          :test-check       {:num-tests 10}})))

(ct/deftest data-to-paths-test
  (let [f #'t/data-to-paths]
    (ct/is (= {[:a 0]  1.0
            [:b :c] 1.0
            [:set]  #{1 2 3}}
          (f {:a   [1.0]
              :b   {:c 1.0}
              :set #{1 2 3}}))
      "map expansion")
    (ct/is (= {[0] 1.0
            [1] 2.0}
          (f [1.0 2.0]))
      "coll expansion")
    (ct/is (= {[] 1.0}
          (f 1.0))
      "not coll or map expansion")))

(ct/deftest approx=-test
  ;; Testing private approx= function via #'
  (let [approx= @#'t/approx=]
    (ct/testing "Default tolerance (1e-6)"
      (ct/is (approx= 1.0 1.0))
      (ct/is (approx= 1.0 1.000001))
      (ct/is (not (approx= 1.0 1.00001)))
      (ct/is (approx= 0.0 0.0))
      (ct/is (approx= -1.0 -1.0))
      (ct/is (approx= 1000000.0 1000000.0000009)))

    (ct/testing "Custom tolerance"
      (ct/is (approx= 1.0 1.001 :tolerance 1e-2))
      (ct/is (not (approx= 1.0 1.01 :tolerance 1e-2)))
      (ct/is (approx= 1.0 1.01 :tolerance 1e-1)))

    #?(:clj
       (ct/testing "Infinity cases"
         (ct/is (approx= Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY))
         (ct/is (approx= Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY))
         (ct/is (not (approx= Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY)))
         (ct/is (not (approx= Double/POSITIVE_INFINITY 1000.0)))
         (ct/is (not (approx= 1000.0 Double/POSITIVE_INFINITY)))
         (ct/is (not (approx= Double/NEGATIVE_INFINITY 1000.0)))
         (ct/is (not (approx= 1000.0 Double/NEGATIVE_INFINITY)))
         (ct/is (not (approx= Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY :tolerance 1e10)))))

    #?(:clj
       (ct/testing "NaN cases without nan-equal? flag"
         (ct/is (not (approx= Double/NaN Double/NaN)))
         (ct/is (not (approx= Double/NaN 1.0)))
         (ct/is (not (approx= 1.0 Double/NaN)))
         (ct/is (not (approx= Double/NaN Double/POSITIVE_INFINITY)))
         (ct/is (not (approx= Double/POSITIVE_INFINITY Double/NaN)))))

    #?(:clj
       (ct/testing "NaN cases with nan-equal? true"
         (ct/is (approx= Double/NaN Double/NaN :nan-equal? true))
         (ct/is (not (approx= Double/NaN 1.0 :nan-equal? true)))
         (ct/is (not (approx= 1.0 Double/NaN :nan-equal? true)))
         (ct/is (not (approx= Double/NaN Double/POSITIVE_INFINITY :nan-equal? true)))))))

(ct/deftest is-valid-test
  (t/is-valid int? 1))

(ct/deftest data-approx=-test
  (t/is-data-approx=
    {:a 1.0
     :b #{1.0}}
    {:a 1.0000001
     :b #{1.0}})
  (t/is-data-approx=
    {:a 1.0}
    {:a 1.001}
    :tolerance 1e-2)
  (t/is-data-approx= [[1.0000001]] [[1.0]])
  (t/is-not (#'t/data-approx= {:a 1} {:b 1}))
  (t/is-not (#'t/data-approx=
              {:a 1.0}
              {:a 1.01})))
