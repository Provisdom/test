(ns provisdom.test.spec-check
  "Wrapper on `clojure.spec.test.alpha` with changes to support the `:throws` keyword in `fdef`
  specs and many other things.

  Some original code copied from Spec.
  https://github.com/clojure/spec.alpha/blob/eaae63904808a0988f6723d1e9e1ee7db6f07ee5/src/main/clojure/clojure/spec/test/alpha.clj"
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st])
  (:import (java.io FileNotFoundException)))

(defn- get-spec-meta
  "Gets spec metadata from utility-belt.spec-ext if available."
  [spec-key]
  (try
    (when-let [get-meta-fn (requiring-resolve 'provisdom.utility-belt.spec-ext/get-meta)]
      (get-meta-fn spec-key))
    (catch FileNotFoundException _ nil)))

(alias 'stc 'clojure.spec.test.check)

(defn- collectionize
  [x]
  (if (symbol? x)
    (list x)
    x))

(defn- explain-check
  [args spec v role]
  (ex-info
    "Specification-based check failed"
    (when-not (s/valid? spec v nil)
      (assoc (s/explain-data* spec [role] [] [] v)
        ::st/args args
        ::st/val v
        ::s/failure :check-failed))))

(defn- check-throwable-ret
  [throws throwable]
  (some (fn [{:keys [pred instanceof]}]
          (and
            (or (nil? pred) (pred throwable))
            (or (nil? instanceof) (instance? instanceof throwable))))
    throws))

(defn- check-call
  "Returns `true` if call passes specs, otherwise *returns* an exception with explain-data +
  `::s/failure`."
  [f specs throws args]
  (let [cargs (when (:args specs) (s/conform (:args specs) args))]
    (if (= cargs ::s/invalid)
      (explain-check args (:args specs) args :args)
      (let [ret (try
                  (apply f args)
                  (catch Throwable t {::throwable t}))
            ;; must use contains? to handle sorted set case
            throwable (when (and (map? ret) (contains? ret ::throwable)) (::throwable ret))
            _ (when (and throwable (not (check-throwable-ret throws throwable)))
                (throw throwable))
            c-ret (when (and (not throwable) (:ret specs)) (s/conform (:ret specs) ret))]
        (if (= c-ret ::s/invalid)
          (explain-check args (:ret specs) ret :ret)
          (if (and (:args specs) (:ret specs) (:fn specs))
            (if (or throwable (s/valid? (:fn specs) {:args cargs :ret c-ret}))
              true
              (explain-check args (:fn specs) {:args cargs :ret c-ret} :fn))
            true))))))

(defn- quick-check
  [f specs throws {gen :gen opts ::stc/opts timeout :timeout}]
  (let [{:keys [num-tests] :or {num-tests 1000}} opts
        g (try (s/gen (:args specs) gen) (catch Throwable t t))]
    (if (instance? Throwable g)
      {:result g}
      (let [prop (gen/for-all* [g] #(check-call f specs throws %))
            run-check #(apply gen/quick-check num-tests prop (mapcat identity opts))]
        (if timeout
          (let [fut (future (run-check))
                result (deref fut timeout ::timeout)]
            (if (= result ::timeout)
              (do
                (future-cancel fut)
                {:result (ex-info (str "Spec check timed out after " timeout "ms")
                           {::timeout timeout})})
              result))
          (run-check))))))

(defn- make-check-result
  "Builds spec result map."
  [check-sym spec test-check-ret]
  (merge {:spec     spec
          ::stc/ret test-check-ret}
    (when check-sym
      {:sym check-sym})
    (when-let [result (-> test-check-ret :result)]
      (when-not (true? result) {:failure result}))
    (when-let [shrunk (-> test-check-ret :shrunk)]
      {:failure (:result shrunk)})))

(defn- check-1
  [{:keys [s f v spec throws]} opts]
  (let [re-inst? (and v (seq (st/unstrument s)) true)
        f (or f (when v @v))
        specd (s/spec spec)
        timeout (:timeout opts)]
    (try
      (cond
        (or (nil? f) (some-> v meta :macro))
        {:failure (ex-info "No fn to spec" {::s/failure :no-fn})
         :sym     s :spec spec}

        (:args specd)
        (let [tc-ret (quick-check f specd throws (assoc opts :timeout timeout))]
          (make-check-result s spec tc-ret))

        :default
        {:failure (ex-info "No :args spec" {::s/failure :no-args-spec})
         :sym     s :spec spec})
      (finally
        (when re-inst? (st/instrument s))))))

(defn- normalize-fdef-throws
  [throws]
  (map (fn [throw-value]
         (cond
           (class? throw-value)
           {:instanceof throw-value}
           (fn? throw-value)
           {:pred throw-value}
           :else (throw (ex-info (format "Unsupported throws value %s" throw-value)
                          {:value throw-value})))) throws))

(defn- sym->check-map
  [s]
  (let [v (resolve s)]
    {:s      s
     :v      v
     :spec   (when v (s/get-spec v))
     :throws (-> (keyword (namespace s) (name s))
                 get-spec-meta
                 :throws
                 normalize-fdef-throws)}))

(defn- validate-check-opts
  [opts]
  (assert (every? ident? (keys (:gen opts))) "check :gen expects ident keys"))

(defn check-fn
  "Runs generative tests for fn `f` using `spec` and `opts`. See [[check]] for options and return."
  ([f spec] (check-fn f spec nil))
  ([f spec opts]
   (validate-check-opts opts)
   (check-1 {:f f :spec spec} opts)))

(defn check
  "Run generative tests for spec conformance on vars named by `sym-or-syms`, a symbol or collection
  of symbols. If `sym-or-syms` is not specified, check all checkable vars.

  The `opts` map includes the following optional keys, where `stc` aliases
  `clojure.spec.test.check`:

    `::stc/opts` - opts to flow through `test.check/quick-check`
    `:gen`       - map from spec names to generator overrides

  The `::stc/opts` include `:num-tests` in addition to the keys documented by test.check. Generator
  overrides are passed to `spec/gen` when generating function args.

  Returns a lazy sequence of check result maps with the following keys:

    `:spec`      - the spec tested
    `:sym`       - optional symbol naming the var tested
    `:failure`   - optional test failure
    `::stc/ret`  - optional value returned by `test.check/quick-check`

  The value for `:failure` can be any exception. Exceptions thrown by spec itself will have an
  `::s/failure` value in `ex-data`:

    `:check-failed` - at least one checked return did not conform
    `:no-args-spec` - no `:args` spec provided
    `:no-fn`        - no fn provided
    `:no-fspec`     - no fspec provided
    `:no-gen`       - unable to generate `:args`
    `:instrument`   - invalid args detected by instrument"
  ([] (check (st/checkable-syms)))
  ([sym-or-syms] (check sym-or-syms nil))
  ([sym-or-syms opts]
   (->> (collectionize sym-or-syms)
     (filter (st/checkable-syms opts))
     (pmap #(check-1 (sym->check-map %) opts)))))
