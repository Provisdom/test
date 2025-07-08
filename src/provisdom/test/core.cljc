(ns provisdom.test.core
  #?(:cljs (:require-macros
             [provisdom.test.core]
             [provisdom.test.assertions.cljs]))
  (:require
    [clojure.test :as t]
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.test.spec-check :as p.st]
    #?(:cljs [orchestra-cljs.spec.test])
    #?(:cljs [cljs.test])

    ;; We require clojure.test for some CLJS code, in order for spec-check to run.
    #?(:cljs [clojure.test.check])
    #?(:cljs [clojure.test.check.properties]))
  #?(:clj (:import (clojure.lang ExceptionInfo)
                   (java.io FileNotFoundException Closeable))))

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
  "Enables instrumentation for `sym-or-syms` while executing `body`. Once `body`
  has completed, unstrument will be called."
  [sym-or-syms & body]
  (let [sym-or-syms (if (= :all sym-or-syms) `(st/instrumentable-syms) sym-or-syms)]
    `(with-instrument* ~[sym-or-syms] ~@body)))

#?(:clj (defn instrumentation
          "Enables instrumentation for the symbols in `instrument` until the `close`
           method is invoked. Typically used in `with-open`. `instrument` is a collection
           of symbols to instrument or `:all` for all instrumentable symbols. Will
           unstrument symbols on close."
          [{:keys [instrument]}]
          (let [syms (cond
                       (coll? instrument) instrument
                       (= instrument :all) (st/instrumentable-syms)
                       :else (throw (ex-info "Instrument must be passed a collection of symbols or :all"
                                      {:instrument instrument})))
                unstrument-syms (set (filter (complement function-instrumented?) syms))
                instrument! @instrument-delay]
            (instrument! syms)
            (reify Closeable
              (close [_]
                (st/unstrument unstrument-syms))))))

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

(defmacro such-that-override
  [such-that-opts & body]
  `(let [such-that# gen/such-that]
     (with-redefs [gen/such-that (fn
                                   ([pred# gen#]
                                    (such-that# pred# gen# ~such-that-opts))
                                   ([pred# gen# opts#]
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

(defmacro debug-slow-gen
  [{:keys [gen spec samples max-tries]
    :or   {samples   500
           max-tries 3}}]
  (assert (or gen spec) ":spec or :gen must be passed")
  `(let [gen# (or ~gen (s/gen ~spec))
         invalid-store# (atom {})]
     (provisdom.test.core/such-that-override {:max-tries ~max-tries}
       (with-redefs [s/gensub (fn [spec# overrides# path# rmap# form#]
                                (my-gensub spec# overrides# path# rmap# form# invalid-store#))]
         (do
           (try
             (doall (gen/sample gen# ~samples))
             :success

             (catch ExceptionInfo ex#
               (if (:max-tries (ex-data ex#))
                 (when ~spec
                   (let [failed# (get @invalid-store# ~spec)]
                     (binding [*out* *err*] (println "Failed such that"))
                     (def ~(symbol (str *ns*) "s-failed-val") failed#)
                     @invalid-store#))
                 (throw ex#)))))))))

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

(defmacro no-problems
  "Returns true if x has no problems when validated against spec, false otherwise.
  Most useful to be called inside an clojure.test/is form:
    (is (no-problems int? 1))

  [[is-valid]] is provided as a shortcut to this."
  [spec x]
  `(= nil (s/explain-data ~spec ~x)))

(defmacro is-valid
  [spec x]
  (let [form (macroexpand `(no-problems ~spec ~x))]
    `(~'is ~form)))

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

#?(:clj (defmethod t/assert-expr 'just
          [menv msg form]
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
        merge (:test-check base-opts {})))))

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
  [fn-sym]
  (get @failed-args-store fn-sym))

(defn spec-check-report
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
                             (let [spec-error-message #?(:clj (.getMessage ex)
                                                         :cljs (.-message ex))
                                   explain-data (ex-data ex)]
                               {:type     :fail
                                :expected "n/a"
                                :actual   "n/a"
                                :message  (str spec-error-message " (seed: " seed ")" "\n\n"
                                            (with-out-str (s/explain-out explain-data)) "\n"
                                            "Args:" "\n\n"
                                            (pr-str failing-args) "\n\n"
                                            "---------------")}))]
        (swap! failed-args-store assoc fn-sym failing-args)
        (cond
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
           :message  "A generator threw an exception.\n"}

          :else {:type     :fail
                 :expected ""
                 :actual   spec-error
                 :message  (str fn-sym " threw an exception.\n")}))
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

#?(:clj
   (defmacro spec-check
     "Run generative tests for spec conformance on vars named by sym-or-syms, a
     symbol or collection of symbols. If sym-or-syms is not specified, check all
     checkable vars.

     The opts map includes the following optional keys:
       :gen - map from spec names to generator
       :coll-check-limit - The number of elements validated in a collection spec'ed
         with 'every'
       :coll-error-limit - The number of errors reported by explain in a collection
         spec'ed with 'every'
       :fspec-iterations -
         The number of times an anonymous fn specified by fspec will be (generatively)
         tested during conform
       :recursion-limit - A soft limit on how many times a branching spec
         (or/alt/*/opt-keys/multi-spec) can be recursed through during generation.
         After this a non-recursive branch will be chosen.
     These opts flow through test.check/quick-check:
       :num-tests - Number of gen tests to run
       :seed - Can be used to re-run previous tests
       :max-size - can be used to control the 'size' of generated values. The size
         will start at 0, and grow up to max-size, as the number of tests increases.
         Generators will use the size parameter to bound their growth. This
         prevents, for example, generating a five-thousand element vector on
         the very first test.
       :reporter-fn - A callback function that will be called at various points in
         the test."
     ([sym-or-syms] `(spec-check ~sym-or-syms {}))
     ([sym-or-syms opts]
      (let [syms (if (sequential? sym-or-syms) sym-or-syms [sym-or-syms])
            syms (->> syms
                   (map #(fully-qualified-namespace &env %))
                   (filter some?)
                   (vec))
            check-opts (normalize-spec-test-opts opts)]
        (if (not-empty syms)
          `(bind-spec-opts ~opts (p.st/check '~syms ~check-opts))
          (throw (ex-info "Cannot qualify some symbols." {:syms syms})))))))

;; must be done at compile time for correct line number resolution
#?(:clj (defmacro do-spec-check-report
          [sym-or-syms opts]
          `(let [check-results# (spec-check ~sym-or-syms ~opts)
                 report-map# (spec-check-report check-results#)]
             (t/do-report report-map#))))

#?(:clj (defmethod t/assert-expr 'spec-check
          [msg form]
          (let [[_ sym-form opts] form]
            `(do-spec-check-report ~sym-form ~opts))))

(defmacro defspec-test
  ([name sym-or-syms] `(defspec-test ~name ~sym-or-syms nil))
  ([name sym-or-syms opts]
   `(t/deftest ~name (do-spec-check-report ~sym-or-syms ~opts))))
