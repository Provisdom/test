(ns provisdom.test.core
  #?(:cljs (:require-macros
             [provisdom.test.core]
             [provisdom.test.assertions.cljs]))
  (:require
    [clojure.pprint :as pprint]
    [clojure.set :as set]
    #?(:clj [clojure.test :as t]
       :cljs [cljs.test :as t])
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [clojure.spec.gen.alpha :as gen]
    #?(:clj [provisdom.test.spec-check :as p.st])
    #?(:cljs [orchestra-cljs.spec.test])

    ;; We require clojure.test for some CLJS code, in order for spec-check to run.
    #?(:cljs [clojure.test.check])
    #?(:cljs [clojure.test.check.properties]))
  #?(:clj (:import (clojure.lang ExceptionInfo)
                   (java.io FileNotFoundException Closeable))))

(s/def ::tolerance (s/double-in :min 0 :max 1000 :NaN? false :infinite? false))
(s/def ::rel-tolerance (s/double-in :min 0 :max 1 :NaN? false :infinite? false))
(s/def ::nan-equal? boolean?)

#?(:clj
   (defn- cljs-env?
     "Returns true if &env indicates ClojureScript compilation context."
     [env]
     (boolean (:ns env))))

(defmacro bind-spec-opts
  [opts & body]
  (let [{:keys [coll-check-limit
                coll-error-limit
                fspec-iterations
                recursion-limit]} opts]
    `(binding [~@(when coll-check-limit [`s/*coll-check-limit* coll-check-limit])
               ~@(when coll-error-limit [`s/*coll-error-limit* coll-error-limit])
               ~@(when fspec-iterations [`s/*fspec-iterations* fspec-iterations])
               ~@(when recursion-limit [`s/*recursion-limit* recursion-limit])]
       ~@body)))

(def instrument-delay
  (delay
    #?(:clj  (let [ost-instrument (try
                                    (requiring-resolve 'orchestra.spec.test/instrument)
                                    (catch FileNotFoundException _ nil))]
               (or ost-instrument st/instrument))
       :cljs (constantly []))))

(def unstrument-delay
  (delay
    #?(:clj  (let [ost-unstrument (try
                                    (requiring-resolve 'orchestra.spec.test/unstrument)
                                    (catch FileNotFoundException _ nil))]
               (or ost-unstrument st/unstrument))
       :cljs (constantly []))))

(defn function-instrumented?
  "Returns `true` if the function named by `sym` is currently instrumented.

   CLJS Note: Always returns `true` on ClojureScript because the internal instrumented-vars atom is
   private and inaccessible. This means [[with-instrument]] won't unstrument functions on CLJS -
   they remain instrumented. This is usually acceptable for test code."
  [sym]
  #?(:clj  (let [instrumented-vars @(var-get #'st/instrumented-vars)]
             (contains? instrumented-vars (resolve sym)))
     :cljs true))

(defn collectionize
  [x]
  (if (coll? x) x [x]))

(defn with-instrument-impl
  [{:keys [sym-or-syms f opts instrument unstrument]
    :or   {instrument @instrument-delay
           unstrument @unstrument-delay}}]
  (let [syms (collectionize sym-or-syms)
        unstrument-syms (into [] (remove function-instrumented?) syms)
        {:keys [coll-check-limit
                coll-error-limit
                fspec-iterations
                recursion-limit]} opts]
    (binding [s/*coll-check-limit* (or coll-check-limit s/*coll-check-limit*)
              s/*coll-error-limit* (or coll-error-limit s/*coll-error-limit*)
              s/*fspec-iterations* (or fspec-iterations s/*fspec-iterations*)
              s/*recursion-limit* (or recursion-limit s/*recursion-limit*)]
      (instrument syms opts)
      (try
        (f)
        (finally
          (unstrument unstrument-syms))))))

(defmacro with-instrument*
  [instrument-args & body]
  `(with-instrument-impl
     {:sym-or-syms ~(first instrument-args)
      :f           (fn [] ~@body)
      :opts        ~(second instrument-args)}))

(defmacro with-instrument
  "Enables instrumentation for `sym-or-syms` while executing `body`. Once `body` has completed,
  unstrument will be called."
  [sym-or-syms & body]
  (let [instrumentable-syms-sym (if (cljs-env? &env)
                                  'cljs.spec.test.alpha/instrumentable-syms
                                  'clojure.spec.test.alpha/instrumentable-syms)
        sym-or-syms (if (= :all sym-or-syms) `(~instrumentable-syms-sym) sym-or-syms)]
    `(with-instrument* ~[sym-or-syms] ~@body)))

#?(:clj (defn instrumentation
          "Enables instrumentation for the symbols in `instrument` until the `close` method is
           invoked. Typically used in `with-open`. `instrument` is a collection of symbols to
           instrument or `:all` for all instrumentable symbols. Will unstrument symbols on close."
          [{:keys [instrument]}]
          (let [syms (cond
                       (coll? instrument) instrument
                       (= instrument :all) (st/instrumentable-syms)
                       :else (throw
                               (ex-info "Instrument must be passed a collection of symbols or :all"
                                 {:instrument instrument})))
                unstrument-syms (set (filter (complement function-instrumented?) syms))
                instrument! @instrument-delay]
            (instrument! syms)
            (reify Closeable
              (close [_]
                (st/unstrument unstrument-syms))))))

(defmacro such-that-override
  [such-that-opts & body]
  `(let [such-that# gen/such-that]
     (with-redefs [gen/such-that (fn
                                   ([pred# gen#]
                                    (such-that# pred# gen# ~such-that-opts))
                                   ([pred# gen# _opts#]
                                    (such-that# pred# gen# ~such-that-opts)))]
       ~@body)))

(defonce invalid-gen-vals (atom {}))

(defn my-gensub
  ([spec overrides path rmap form] (my-gensub spec overrides path rmap form invalid-gen-vals))
  ([og-spec overrides path rmap form invalid-store]
   ;;(prn {:spec spec :over overrides :path path :form form})
   (let [spec (#'s/specize og-spec)]
     (if-let [g (or (when-let [gfn (or (get overrides (or (#'s/spec-name spec) spec))
                                       (get overrides path))]
                      (gfn))
                    (s/gen* spec overrides path rmap))]
       (gen/such-that (with-meta (fn [x]
                                   (let [valid? (s/valid? spec x)]
                                     (when-not valid?
                                       (swap! invalid-store assoc og-spec x))
                                     valid?))
                                 {:spec      spec
                                  :overrides overrides
                                  :path      path
                                  :form      form}) g 100)
       (let [abbr (s/abbrev form)]
         (throw (ex-info (str "Unable to construct gen at: " path " for: " abbr)
                  {::path path ::form form ::failure :no-gen})))))))

(defmacro deftest
  "Re-export `clojure.test/deftest` for convenience when using `[provisdom.test.core :as t]`."
  [name & body]
  (if (cljs-env? &env)
    `(cljs.test/deftest ~name ~@body)
    `(clojure.test/deftest ~name ~@body)))

(defmacro is
  "Re-export `clojure.test/is` for convenience when using `[provisdom.test.core :as t]`."
  [form & args]
  (if (cljs-env? &env)
    `(cljs.test/is ~form ~@args)
    `(clojure.test/is ~form ~@args)))

(defmacro is=
  ([expected actual] `(is= ~expected ~actual nil))
  ([expected actual msg]
   (if (cljs-env? &env)
     `(cljs.test/is (~'= ~expected ~actual) ~msg)
     `(clojure.test/is (~'= ~expected ~actual) ~msg))))

(defmacro is-not
  ([form] `(is-not ~form nil))
  ([form msg]
   (if (cljs-env? &env)
     `(cljs.test/is (~'not ~form) ~msg)
     `(clojure.test/is (~'not ~form) ~msg))))

(defmacro is-not=
  ([expected actual] `(is-not= ~expected ~actual nil))
  ([expected actual msg]
   (if (cljs-env? &env)
     `(cljs.test/is (~'not= ~expected ~actual) ~msg)
     `(clojure.test/is (~'not= ~expected ~actual) ~msg))))

(defmacro is-approx=
  "Asserts that `x1` and `x2` are approximately equal within tolerance.

   Options:
     `:tolerance`     - absolute maximum allowed difference (default `1e-6`)
     `:rel-tolerance` - relative tolerance as fraction of `max(|x1|,|x2|)`; if provided, used
                        instead of absolute tolerance
     `:nan-equal?`    - if `true`, two `NaN` values are considered equal"
  ([x1 x2] `(is-approx= ~x1 ~x2 :tolerance 1e-6))
  ([x1 x2 & opts]
   (if (cljs-env? &env)
     `(cljs.test/is (approx= ~x1 ~x2 ~@opts))
     `(clojure.test/is (#'approx= ~x1 ~x2 ~@opts)))))

(defmacro is-thrown-with-data
  "Asserts that `body` throws an `ExceptionInfo` whose `ex-data` contains all key-value pairs in
   `expected-data` (subset match). CLJ only.

   Example:
     (is-thrown-with-data {:type :validation-error}
       (throw (ex-info \"bad\" {:type :validation-error :field :name})))"
  [expected-data & body]
  (let [do-report-sym (if (cljs-env? &env) 'cljs.test/do-report 'clojure.test/do-report)]
    `(try
       ~@body
       (~do-report-sym {:type     :fail
                        :message  "Expected exception to be thrown"
                        :expected '~expected-data
                        :actual   nil})
       (catch ~(if (cljs-env? &env) :default 'ExceptionInfo) e#
         (let [actual-data# (ex-data e#)
               matches?# (every? (fn [[k# v#]]
                                   (= v# (get actual-data# k#)))
                           ~expected-data)]
           (if matches?#
             (~do-report-sym {:type     :pass
                              :message  nil
                              :expected '~expected-data
                              :actual   actual-data#})
             (~do-report-sym {:type     :fail
                              :message  "Exception data did not match expected"
                              :expected '~expected-data
                              :actual   actual-data#}))
           matches?#)))))

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

(defn ^:no-doc approx=
  "Internal. Use [[is-approx=]] macro instead."
  {:private true}
  ([x1 x2] (approx= x1 x2 :tolerance 1e-6))
  ([x1 x2 & {:keys [tolerance rel-tolerance nan-equal?]}]
   (cond
     ;; Both are infinite - check if same infinity
     (and (infinite? x1) (infinite? x2))
     (= x1 x2)

     ;; One is infinite, other is not - never equal
     (or (infinite? x1) (infinite? x2))
     false

     ;; Both are NaN - consider equal (optional behavior)
     (and nan-equal? (NaN? x1) (NaN? x2))
     true

     ;; One is NaN, other is not - never equal
     (or (NaN? x1) (NaN? x2))
     false

     :else
     (let [diff (abs (double (- x1 x2)))]
       (if rel-tolerance
         ;; Relative tolerance: |x1 - x2| / max(|x1|, |x2|) < rel-tolerance
         ;; Handle zero case specially
         (let [max-abs (max (abs (double x1)) (abs (double x2)))]
           (if (zero? max-abs)
             (zero? diff)
             (< (/ diff max-abs) rel-tolerance)))
         ;; Absolute tolerance
         (< diff tolerance))))))

(defmacro no-problems
  "Returns `true` if `x` has no problems when validated against `spec`, `false` otherwise. Most
  useful to be called inside a `clojure.test/is` form:

    (t/is (no-problems int? 1))

  [[is-valid]] is provided as a shortcut to this."
  [spec x]
  `(= nil (s/explain-data ~spec ~x)))

(defmacro is-valid
  [spec x]
  (let [form (macroexpand `(no-problems ~spec ~x))]
    (if (cljs-env? &env)
      `(cljs.test/is ~form)
      `(clojure.test/is ~form))))

(defn ^:no-doc data-approx=
  "Internal. Use [[is-data-approx=]] macro instead."
  {:private true}
  ([expected actual] (data-approx= expected actual :tolerance 1e-6))
  ([expected actual & {:as approx=-opts}]
   (let [expected-paths-map (data-to-paths expected)
         actual-paths-map (data-to-paths actual)]
     (and (= (set (keys expected-paths-map))
             (set (keys actual-paths-map)))
          (every? (fn [[path expected-val]]
                    (let [actual-val (get actual-paths-map path)]
                      (if (number? expected-val)
                        (approx= expected-val actual-val approx=-opts)
                        (= expected-val (get actual-paths-map path)))))
            expected-paths-map)))))

(defn data-diff
  "Returns a sequence of maps describing differences between `expected` and `actual`. Each map has
   `:path`, `:expected`, `:actual`, and `:equal?` keys. Only returns entries where values differ."
  ([expected actual] (data-diff expected actual :tolerance 1e-6))
  ([expected actual & {:as approx=-opts}]
   (let [expected-paths (data-to-paths expected)
         actual-paths (data-to-paths actual)
         all-paths (set/union (set (keys expected-paths)) (set (keys actual-paths)))]
     (->> all-paths
          (map (fn [path]
                 (let [e (get expected-paths path ::missing)
                       a (get actual-paths path ::missing)
                       equal? (cond
                                (= e ::missing) false
                                (= a ::missing) false
                                (number? e) (apply approx= e a (mapcat identity approx=-opts))
                                :else (= e a))]
                   {:path path :expected e :actual a :equal? equal?})))
          (remove :equal?)))))

(defmacro is-data-approx=
  "Asserts that nested data structures `x1` and `x2` are approximately equal. Compares numbers with
   [[approx=]] and non-numbers with `=`. On failure, shows which paths differ.

   Options:
     `:tolerance`     - absolute max difference for numbers (default `1e-6`)
     `:rel-tolerance` - relative tolerance; if provided, used instead of absolute
     `:nan-equal?`    - if `true`, two `NaN` values are considered equal"
  ([x1 x2] `(is-data-approx= ~x1 ~x2 :tolerance 1e-6))
  ([x1 x2 & opts]
   (let [do-report-sym (if (cljs-env? &env) 'cljs.test/do-report 'clojure.test/do-report)
         data-approx=-sym (if (cljs-env? &env)
                            'provisdom.test.core/data-approx=
                            `#'data-approx=)]
     `(let [expected# ~x1
            actual# ~x2
            result# (~data-approx=-sym expected# actual# ~@opts)]
        (if result#
          (~do-report-sym {:type :pass :message nil :expected expected# :actual actual#})
          (let [diffs# (data-diff expected# actual# ~@opts)
                diff-str# (with-out-str
                            (doseq [{:keys [~'path ~'expected ~'actual]} diffs#]
                              (println "  Path:" ~'path)
                              (println "    expected:" ~'expected)
                              (println "    actual:  " ~'actual)))]
            (~do-report-sym {:type     :fail
                             :message  (str "Data structures differ at " (count diffs#)
                                         " path(s):\n" diff-str#)
                             :expected expected#
                             :actual   actual#})))
        result#))))

#?(:clj (defmethod t/assert-expr 'just
          [_menv msg form]
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
             result#)))

(def ^:dynamic *default-spec-check-opts* {})

(defmacro with-spec-check-opts
  [opts & forms]
  `(binding [*default-spec-check-opts* ~opts]
     ~@forms))

(def quick-check-stc-keys
  [:num-tests :seed :max-size :reporter-fn])

(defn- normalize-spec-test-opts
  [opts]
  (let [base-opts (merge *default-spec-check-opts* opts)]
    (-> base-opts
        (assoc :clojure.spec.test.check/opts (select-keys opts quick-check-stc-keys))
        (update :clojure.spec.test.check/opts
                ;; exists for backwards compatibility. Eventually this can be removed
                merge (:test-check base-opts {}))
        ;; Pass timeout through for spec-check
        (cond-> (:timeout opts) (assoc :timeout (:timeout opts))))))

#?(:clj
   (defn spec-test-check
     ([sym-or-syms] (spec-test-check sym-or-syms {}))
     ([sym-or-syms opts]
      (st/check sym-or-syms (normalize-spec-test-opts opts)))))

(defn fspec-data
  [sym]
  (apply hash-map (rest (s/form (s/get-spec sym)))))

(defonce failed-args-store (atom {}))

(defn get-failed-args
  "Returns the failed args for `fn-sym` from the most recent spec-check failure, or `nil` if no
   failure recorded."
  [fn-sym]
  (get @failed-args-store fn-sym))

(defn pprint-failed-args
  "Pretty-prints the failed args for `fn-sym`. Returns the args if found, `nil` otherwise."
  [fn-sym]
  (when-let [args (get-failed-args fn-sym)]
    (pprint/pprint args)
    args))

(defn spec-check-report
  "Generate a test report from spec check results. Includes shrinking information when available."
  [check-results]
  (let [first-failure (->> check-results
                           (remove #(-> % :clojure.spec.test.check/ret :result true?))
                           first)]
    (if first-failure
      ;; reasons for a check to fail:
      ;; - generator threw an exception: test.check results
      ;; - return value does not pass :ret spec: test.check results
      ;; - function threw exception while running: test.check results
      ;; - cannot satisfy such-that in args: thrown as an exception
      ;; - timeout
      (let [fn-sym (:sym first-failure)
            test-check-ret (:clojure.spec.test.check/ret first-failure)
            ;; the seed can be used to reproduce the test results
            seed (:seed test-check-ret)
            ;; shrinking info
            shrunk (:shrunk test-check-ret)
            shrink-depth (get-in shrunk [:depth] 0)
            shrink-total (:total shrunk)
            pass-count (:pass? test-check-ret (:num-tests test-check-ret))
            ;; args used during the failed function call (prefer shrunk if available)
            original-args (first (:fail test-check-ret))
            shrunk-args (first (:smallest shrunk))
            failing-args (or shrunk-args original-args)
            spec-error (-> (:result-data test-check-ret)
                           :clojure.test.check.properties/error)
            timeout? (and (instance? ExceptionInfo (:result test-check-ret))
                          (contains? (ex-data (:result test-check-ret))
                                     :provisdom.test.spec-check/timeout))
            spec-error? (fn [ex]
                          (and (instance? ExceptionInfo ex)
                               (::s/failure (ex-data ex))))
            shrink-info (when (pos? shrink-depth)
                          (str "Shrunk " shrink-depth " times"
                            (when shrink-total (str " (" shrink-total " attempts)"))
                            " to find minimal case.\n"))
            pass-info (when pass-count
                        (str "Failed after " pass-count " passing tests.\n"))
            spec-error-map (fn [ex]
                             (let [spec-error-message (ex-message ex)
                                   explain-data (ex-data ex)]
                               {:type     :fail
                                :expected "n/a"
                                :actual   "n/a"
                                :message  (str spec-error-message " (seed: " seed ")\n"
                                            pass-info
                                            shrink-info
                                            "\n"
                                            (with-out-str (s/explain-out explain-data)) "\n"
                                            "Args:\n\n"
                                            (with-out-str (pprint/pprint failing-args))
                                            "---------------")}))]
        (swap! failed-args-store assoc fn-sym failing-args)
        (cond
          ;; Timeout
          timeout?
          {:type     :fail
           :expected ""
           :actual   (:result test-check-ret)
           :message  (str "Spec check timed out.\n" pass-info)}

          (spec-error? spec-error)
          (spec-error-map spec-error)

          ;; Exceptions thrown from Spec itself
          (spec-error? (:failure first-failure))
          (spec-error-map (:failure first-failure))

          ;; Generator threw an exception
          (instance? #?(:clj Throwable :cljs js/Error) (:failure first-failure))
          {:type     :fail
           :expected ""
           :actual   (:failure first-failure)
           :message  (str "A generator threw an exception.\n" pass-info)}

          :else {:type     :fail
                 :expected ""
                 :actual   spec-error
                 :message  (str fn-sym " threw an exception.\n" pass-info)}))
      ;; all checks passed
      (do
        (swap! failed-args-store (fn [failed]
                                   (apply dissoc failed (map :sym check-results))))
        {:type    :pass
         :message (str "Generative tests pass for "
                    (str/join ", " (map :sym check-results)))}))))

#?(:clj
   (defn- fully-qualified-namespace
     [env sym]
     (if (:ns env)
       (let [resolve (requiring-resolve 'cljs.analyzer.api/resolve)
             {ns-sym :ns name-sym :name} (resolve env sym)]
         (when (and ns-sym name-sym)
           (symbol (str ns-sym) (-> name-sym name str))))
       (let [metadata (meta (resolve sym))]
         (when metadata
           (symbol (str (:ns metadata)) (str (:name metadata))))))))

#?(:clj (defmacro -check-1
          [syms check-opts]
          (if (:ns &env)
            (st/check syms check-opts)
            `(p.st/check ~syms ~check-opts))))

#?(:clj
   (defmacro spec-check-
     "Internal. Run generative tests for spec conformance on vars named by `sym-or-syms`, a symbol
     or collection of symbols. If `sym-or-syms` is not specified, check all checkable vars.

     The `opts` map includes the following optional keys:
       `:gen`              - map from spec names to generator
       `:coll-check-limit` - the number of elements validated in a collection spec'ed with `every`
       `:coll-error-limit` - the number of errors reported by explain in a collection spec'ed with
                             `every`
       `:fspec-iterations` - the number of times an anonymous fn specified by `fspec` will be
                             (generatively) tested during conform
       `:recursion-limit`  - a soft limit on how many times a branching spec
                             (`or`/`alt`/`*`/`opt-keys`/`multi-spec`) can be recursed through
                             during generation; after this a non-recursive branch will be chosen

     These opts flow through `test.check/quick-check`:
       `:num-tests`   - number of gen tests to run
       `:seed`        - can be used to re-run previous tests
       `:max-size`    - can be used to control the 'size' of generated values; the size will start
                        at 0, and grow up to max-size, as the number of tests increases; generators
                        will use the size parameter to bound their growth, preventing, for example,
                        generating a five-thousand element vector on the very first test
       `:reporter-fn` - a callback function that will be called at various points in the test"
     ([sym-or-syms] `(spec-check- ~sym-or-syms {}))
     ([sym-or-syms opts]
      (let [syms (if (sequential? sym-or-syms) sym-or-syms [sym-or-syms])
            syms (->> syms
                      (map #(fully-qualified-namespace &env %))
                      (filter some?)
                      (vec))
            check-opts (normalize-spec-test-opts opts)]
        (if (not-empty syms)
          `(bind-spec-opts ~opts (-check-1 '~syms ~check-opts))
          (throw (ex-info "Cannot qualify some symbols." {:syms syms})))))))

;; must be done at compile time for correct line number resolution
#?(:clj (defmacro do-spec-check-report
          [sym-or-syms opts]
          (let [do-report-sym (if (cljs-env? &env)
                                'cljs.test/do-report
                                'clojure.test/do-report)]
            `(let [check-results# (spec-check- ~sym-or-syms ~opts)
                   report-map# (spec-check-report check-results#)]
               (~do-report-sym report-map#)))))

#?(:clj (defmethod t/assert-expr 'spec-check
          [_msg form]
          (let [[_ sym-form opts] form]
            `(do-spec-check-report ~sym-form ~opts))))

(defmacro defspec-test
  ([name sym-or-syms] `(defspec-test ~name ~sym-or-syms nil))
  ([name sym-or-syms opts]
   (if (cljs-env? &env)
     `(cljs.test/deftest ~name (do-spec-check-report ~sym-or-syms ~opts))
     `(clojure.test/deftest ~name (do-spec-check-report ~sym-or-syms ~opts)))))

#?(:clj (defmacro is-spec-check
          "Runs generative tests for spec conformance and reports results. Equivalent to
          `(t/is (spec-check ...))` but more direct.

          Options:
            `:num-tests`        - number of tests to run (default 1000)
            `:seed`             - seed for reproducible tests
            `:max-size`         - control size of generated values
            `:gen`              - map from spec names to generator overrides
            `:timeout`          - timeout in milliseconds
            `:debug`            - when `true`, provides detailed generator diagnostics
            `:coll-check-limit` - elements to validate in collection specs
            `:fspec-iterations` - times to test fns in `fspec`
            `:recursion-limit`  - soft limit on recursive generation

          When `:debug` is `true`:
            - Reduces `such-that` max-tries for faster failure
            - Stores problematic values for inspection
            - Provides more detailed error context"
          ([sym-or-syms] `(is-spec-check ~sym-or-syms {}))
          ([sym-or-syms opts]
           (let [debug? (:debug opts)]
             (if debug?
               `(such-that-override {:max-tries 10}
                                    (do-spec-check-report ~sym-or-syms ~(dissoc opts :debug)))
               `(do-spec-check-report ~sym-or-syms ~opts))))))

