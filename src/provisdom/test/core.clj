(ns provisdom.test.core
  (:require
    [clojure.test :as t]
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st])
  (:import (clojure.lang ExceptionInfo)
           (java.io FileNotFoundException)))

(def instrument-delay
  (delay
    (let [ost-instrument (try
                           (requiring-resolve 'orchestra.spec.test/instrument)
                           (catch FileNotFoundException _ nil))]
      (or ost-instrument st/instrument))))

(def unstrument-delay
  (delay
    (let [ost-unstrument (try
                           (requiring-resolve 'orchestra.spec.test/unstrument)
                           (catch FileNotFoundException _ nil))]
      (or ost-unstrument st/unstrument))))

(defmacro with-instrument*
  [instrument-args & body]
  `(do
     (@instrument-delay ~@instrument-args)
     (try
       ~@body
       (finally
         (@unstrument-delay ~(first instrument-args))))))

(defmacro with-instrument
  "Enables instrumentation for `sym-or-syms` while executing `body`. Once `body`
  has completed, unstrument will be called."
  [sym-or-syms & body]
  `(with-instrument* ~[sym-or-syms] ~@body))

(defmacro with-instrument2
  [{:keys [orchestra spec]} & body]
  (let [before-forms (->> [(when spec
                             `(st/instrument ~@spec))
                           (when orchestra
                             `(orchestra.spec.test/instrument ~@orchestra))]
                          (filter some?))
        after-forms (->> [(when spec
                            `(orchestra.spec.test/unstrument ~@(take 1 spec)))
                          (when orchestra
                            `(orchestra.spec.test/unstrument ~@(take 1 orchestra)))]
                         (filter some?))]
    `(do
       ~@before-forms
       (try
         ~@body
         (finally
           ~@after-forms)))))

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

(defn- data-to-paths
  [x]
  (letfn [(data-to-paths' [x acc cur-path]
            (cond
              (sequential? x)
              (data-to-paths' (into {} (map-indexed vector x)) acc cur-path)


              (map? x)
              (reduce-kv
                (fn [acc k v]
                  (merge acc (data-to-paths' v acc (conj cur-path k))))
                acc x)

              :else (assoc acc cur-path x)))]
    (data-to-paths' x {} [])))

(defn approx=
  ([x1 x2] (approx= x1 x2 1e-6))
  ([x1 x2 tolerance]
   (< (Math/abs (double (- x1 x2))) tolerance)))

(defn data-approx=
  ([expected actual] (data-approx= expected actual {:tolerance 1e-6}))
  ([expected actual {:keys [tolerance]}]
   (let [expected-paths-map (data-to-paths expected)
         actual-paths-map (data-to-paths actual)]
     (and (= (set (keys expected-paths-map))
             (set (keys actual-paths-map)))
          (every? (fn [[path expected-val]]
                    (let [actual-val (get actual-paths-map path)]
                      (if (number? expected-val)
                        (approx= expected-val actual-val tolerance)
                        (= expected-val (get actual-paths-map path)))))
                  expected-paths-map)))))

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

(def ^:dynamic *default-spec-check-opts* {})

(defmacro with-spec-check-opts
  [opts & forms]
  `(binding [*default-spec-check-opts* ~opts]
     ~@forms))

(defn- normalize-spec-test-opts
  [opts]
  (let [base-opts (merge *default-spec-check-opts* opts)]
    (update base-opts :clojure.spec.test.check/opts
            (fn [stc-opts] (merge stc-opts (:test-check base-opts))))))

(defn spec-test-check
  ([sym-or-syms] (spec-test-check sym-or-syms {}))
  ([sym-or-syms opts]
   (st/check sym-or-syms (normalize-spec-test-opts opts))))

(defn fspec-data
  [sym]
  (apply hash-map (rest (s/form (s/get-spec sym)))))

(defn report-spec-check
  [check-results]
  (let [first-failure (-> (filter (fn [result]
                                    (not= true (get-in result [:clojure.spec.test.check/ret :result])))
                                  check-results)
                          (first))]
    (if first-failure
      ;; reasons for a check to fail:
      ;; - generator threw an exception: test.check results
      ;; - return value does not pass :ret spec: test.check results
      ;; - function threw exception while running: test.check results
      ;; - cannot satisfy such-that in args: thrown as an exception
      (let [fn-sym (:sym first-failure)
            test-check-ret (:clojure.spec.test.check/ret first-failure)
            ;; the seed can be used to reproduce the test results
            seed (:seed test-check-ret)
            ;; args used during the failed function call
            failing-args (first (:fail test-check-ret))
            spec-error (-> (:result-data test-check-ret)
                           :clojure.test.check.properties/error)
            spec-error? (fn [ex]
                          (and (instance? ExceptionInfo ex)
                               (::s/failure (ex-data ex))))
            spec-error-map (fn [ex]
                             (let [spec-error-message (.getMessage ex)
                                   explain-data (ex-data ex)]
                               {:type     :fail
                                :expected "n/a"
                                :actual   "n/a"
                                :message  (str spec-error-message " (seed: " seed ")" "\n\n"
                                               (with-out-str (s/explain-out explain-data)) "\n"
                                               "Args:" "\n\n"
                                               (pr-str failing-args) "\n\n"
                                               "---------------")}))]
        (cond
          (spec-error? spec-error)
          (spec-error-map spec-error)

          ;; Exceptions thrown from Spec itself
          (spec-error? (:failure first-failure))
          (spec-error-map (:failure first-failure))

          ;; Generator threw an exception
          (instance? Throwable (:failure first-failure))
          {:type     :fail
           :expected ""
           :actual   (:failure first-failure)
           :message  "A generator threw an exception.\n"}

          :else {:type     :fail
                 :expected ""
                 :actual   spec-error
                 :message  (str fn-sym " threw an exception.\n")}))
      ;; all checks passed
      {:type    :pass
       :message (str "Generative tests pass for "
                     (str/join ", " (map :sym check-results)))})))

(defn- fully-qualified-namespace
  [sym]
  (let [metadata (meta (resolve sym))]
    (when metadata
      (symbol (str (:ns metadata)) (str (:name metadata))))))

(defmacro spec-check*
  ([sym-or-syms] `(spec-check* ~sym-or-syms {}))
  ([sym-or-syms opts]
   (let [syms (if (sequential? sym-or-syms) sym-or-syms [sym-or-syms])
         syms (->> syms
                   (map fully-qualified-namespace)
                   (filter some?))
         {:keys [coll-check-limit
                 coll-error-limit
                 fspec-iterations
                 recursion-limit
                 num-tests
                 seed]} opts
         check-opts (cond-> opts
                      num-tests (assoc-in [:clojure.spec.test.check/opts :num-tests] num-tests)
                      seed (assoc-in [:clojure.spec.test.check/opts :seed] seed))]
     (if (not-empty syms)
       `(binding [s/*coll-check-limit* (or ~coll-check-limit s/*coll-check-limit*)
                  s/*coll-error-limit* (or ~coll-error-limit s/*coll-error-limit*)
                  s/*fspec-iterations* (or ~fspec-iterations s/*fspec-iterations*)
                  s/*recursion-limit* (or ~recursion-limit s/*recursion-limit*)]
          (st/check '~syms ~check-opts))
       (throw (ex-info "Cannot qualify some symbols." {:sym ~syms}))))))

;; must be done at compile time for correct line number resolution
(defmacro do-spec-check-report
  [sym-or-syms opts]
  `(let [check-results# (spec-check* ~sym-or-syms ~opts)
         report-map# (report-spec-check check-results#)]
     (t/do-report report-map#)))

(defmethod t/assert-expr 'spec-check
  [msg form]
  (let [[_ sym-form opts] form]
    `(do-spec-check-report ~sym-form ~opts)))

(defmacro defspec-test
  ([name sym-or-syms] `(defspec-test ~name ~sym-or-syms nil))
  ([name sym-or-syms opts]
   `(t/deftest ~name (do-spec-check-report ~sym-or-syms ~opts))))