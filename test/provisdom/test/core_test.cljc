(ns provisdom.test.core-test
  #?(:cljs (:require-macros [provisdom.test.core]))
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [provisdom.test.core :as t])
  #?(:clj (:import (clojure.lang ExceptionInfo)
                   (java.io Closeable))))

;;2 seconds

#?(:clj (set! *warn-on-reflection* true))

(t/deftest bind-spec-opts-unit-test
  (t/bind-spec-opts {:fspec-iterations 10}
    (t/is (= 10 s/*fspec-iterations*))))

#?(:clj (t/deftest test-macro-expansions
          ;; is= expands correctly
          (t/is (= `(clojure.test/is (~'= 1 1) nil) (macroexpand-1 `(t/is= 1 1 nil))))
          ;; is-not expands correctly
          (t/is (= `(clojure.test/is (~'not false) "some message")
                   (macroexpand-1 `(t/is-not false "some message"))))))

(t/deftest t-midje-just
  ;; matching cases
  (t/is (t/midje-just [1 1 1] [1 1 1]))
  (t/is (t/midje-just [1 1 #(and (number? %) (not (== % %)))]
                      [1 1 #?(:clj Double/NaN :cljs js/NaN)]))
  ;; non-matching case
  (t/is-not (t/midje-just [1 1 1] [1 1 2])))

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

(t/deftest function-instrumented?-test
  (st/unstrument `my-add)
  (st/instrument `my-add)
  (t/is= true (t/function-instrumented? `my-add))
  (st/unstrument `my-add)
  (t/is= #?(:clj false :cljs true) (t/function-instrumented? `my-add)))

#?(:clj
   (t/deftest with-instrument-test
     ;; started instrumented
     (st/instrument `my-add)
     (t/with-instrument `my-add
       (my-add 1 2))
     (t/is (thrown-with-msg? ExceptionInfo #"Call to" (my-add 1 "a")))
     (t/is= [`my-add] (st/unstrument `my-add))
     (t/with-instrument :all
       (t/is= "3" (my-add 1 2)))
     ;; started uninstrumented
     (t/with-instrument `my-add
       (t/is (thrown-with-msg? ExceptionInfo #"Call to" (my-add 1 "a"))))
     (t/is (thrown? ClassCastException (my-add 1 "a")))))

#?(:clj (t/deftest instrumentation-test
          (with-open [^Closeable _ (t/instrumentation {:instrument [`my-add]})]
            (t/is (thrown? ExceptionInfo (my-add 1 "a"))))
          (t/is (thrown? ClassCastException (my-add 1 "a")))))

#?(:clj (t/defspec-test test-my-add `my-add))

#?(:clj
   (t/deftest test-spec-check-assert
     (t/is-spec-check [my-add] {:num-tests 10})
     (t/with-spec-check-opts
       {:num-tests 10}
       (t/is-spec-check my-add))
     (t/is-spec-check my-add {:coll-check-limit 10
                              :coll-error-limit 10
                              :fspec-iterations 10
                              :num-tests        10
                              :recursion-limit  1})))

#?(:clj
   (t/deftest data-to-paths-test
     (let [f #'t/data-to-paths]
       ;; map expansion
       (t/is= {[:a 0]  1.0
               [:b :c] 1.0
               [:set]  #{1 2 3}}
         (f {:a   [1.0]
             :b   {:c 1.0}
             :set #{1 2 3}}))
       ;; coll expansion
       (t/is= {[0] 1.0
               [1] 2.0}
         (f [1.0 2.0]))
       ;; not coll or map expansion
       (t/is= {[] 1.0}
         (f 1.0)))))

#?(:clj
   (t/deftest approx=-test
     ;; Testing private approx= function via #'
     (let [approx= @#'t/approx=]
       ;; Default tolerance (1e-6)
       (t/is (approx= 1.0 1.0))
       (t/is (approx= 1.0 1.000001))
       (t/is (not (approx= 1.0 1.00001)))
       (t/is (approx= 0.0 0.0))
       (t/is (approx= -1.0 -1.0))
       (t/is (approx= 1000000.0 1000000.0000009))
       ;; Custom tolerance
       (t/is (approx= 1.0 1.001 :tolerance 1e-2))
       (t/is (not (approx= 1.0 1.01 :tolerance 1e-2)))
       (t/is (approx= 1.0 1.01 :tolerance 1e-1))
       ;; Relative tolerance: |x1 - x2| / max(|x1|, |x2|) < rel-tolerance
       (t/is (approx= 1e10 1.0000001e10 :rel-tolerance 1e-6))
       (t/is (not (approx= 1e10 1.001e10 :rel-tolerance 1e-6)))
       ;; works at different scales
       (t/is (approx= 1000.0 1000.0005 :rel-tolerance 1e-6))
       (t/is (approx= 0.001 0.0010000005 :rel-tolerance 1e-6))
       ;; zero case - both zero
       (t/is (approx= 0.0 0.0 :rel-tolerance 1e-6))
       ;; zero case - one zero, one non-zero should fail
       (t/is (not (approx= 0.0 0.001 :rel-tolerance 1e-6)))
       ;; Infinity cases
       (t/is (approx= Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY))
       (t/is (approx= Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY))
       (t/is (not (approx= Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY)))
       (t/is (not (approx= Double/POSITIVE_INFINITY 1000.0)))
       (t/is (not (approx= 1000.0 Double/POSITIVE_INFINITY)))
       (t/is (not (approx= Double/NEGATIVE_INFINITY 1000.0)))
       (t/is (not (approx= 1000.0 Double/NEGATIVE_INFINITY)))
       (t/is (not (approx= Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY :tolerance 1e10)))
       ;; NaN cases without nan-equal? flag
       (t/is (not (approx= Double/NaN Double/NaN)))
       (t/is (not (approx= Double/NaN 1.0)))
       (t/is (not (approx= 1.0 Double/NaN)))
       (t/is (not (approx= Double/NaN Double/POSITIVE_INFINITY)))
       (t/is (not (approx= Double/POSITIVE_INFINITY Double/NaN)))
       ;; NaN cases with nan-equal? true
       (t/is (approx= Double/NaN Double/NaN :nan-equal? true))
       (t/is (not (approx= Double/NaN 1.0 :nan-equal? true)))
       (t/is (not (approx= 1.0 Double/NaN :nan-equal? true)))
       (t/is (not (approx= Double/NaN Double/POSITIVE_INFINITY :nan-equal? true))))))

(t/deftest is-valid-test
  (t/is-valid int? 1))

#?(:clj
   (t/deftest is-thrown-with-data-test
     ;; matches when all expected keys present with correct values
     (t/is (t/is-thrown-with-data {:type :test-error}
             (throw (ex-info "test" {:type :test-error :extra :data}))))
     ;; matches with multiple keys
     (t/is (t/is-thrown-with-data {:type :test-error :code 42}
             (throw (ex-info "test" {:type :test-error :code 42 :extra :data}))))
     ;; empty expected-data matches any ex-info
     (t/is (t/is-thrown-with-data {}
             (throw (ex-info "test" {:anything :here}))))))

(t/deftest data-approx=-test
  (t/is-data-approx=
    {:a 1.0
     :b #{1.0}}
    {:a 1.0000001
     :b #{1.0}})
  (t/is-data-approx=
    {:a 1.0}
    {:a 1.001}
    :tolerance 1e-2)
  ;; rel-tolerance test
  (t/is-data-approx=
    {:x 1e10}
    {:x 1.0000001e10}
    :rel-tolerance 1e-6)
  (t/is-data-approx= [[1.0000001]] [[1.0]])
  #?(:clj (t/is-not (#'t/data-approx= {:a 1} {:b 1})))
  #?(:clj (t/is-not (#'t/data-approx= {:a 1.0} {:a 1.01}))))
