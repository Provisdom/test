(ns provisdom.test.core
  (:require
    [clojure.test :as t]
    [clojure.string :as str]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(defmacro with-instrument*
  [instrument-args & body]
  `(do
     (ost/instrument ~@instrument-args)
     (try
       ~@body
       (finally
         (ost/unstrument ~(first instrument-args))))))

(defmacro with-instrument
  "Enables instrumentation for `sym-or-syms` while executing `body`. Once `body`
  has completed, unstrument will be called."
  [sym-or-syms & body]
  `(with-instrument* ~[sym-or-syms] ~@body))

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

(defn fspec-data
  [sym]
  (apply hash-map (rest (s/form (s/get-spec sym)))))

;; must be done at compile time for correct line number resolution
(defmacro do-spec-check-report
  [sym-or-syms opts]
  `(let [opts# (#'normalize-spec-test-opts ~opts)
         check-results# (binding [s/*coll-check-limit* (or (:coll-check-limit opts#) s/*coll-check-limit*)
                                  s/*coll-error-limit* (or (:coll-error-limit opts#) s/*coll-error-limit*)
                                  s/*fspec-iterations* (or (:fspec-iterations opts#) s/*fspec-iterations*)
                                  s/*recursion-limit* (or (:recursion-limit opts#) s/*recursion-limit*)]
                          (st/check ~sym-or-syms opts#))
         checks-passed?# (every? nil? (map :failure check-results#))]
     (if checks-passed?#
       (t/do-report {:type    :pass
                     :message (str "Generative tests pass for "
                                   (str/join ", " (map :sym check-results#)))})
       (let [failed-check# (first (filter #(contains? % :failure) check-results#))
             sym# (:sym failed-check#)
             thrown-ex# (if-let [failure# (get-in failed-check# [:clojure.spec.test.check/ret :result])]
                          failure#
                          (get-in failed-check# [:clojure.spec.test.check/ret :result-data :clojure.test.check.properties/error]))
             passed-args# (-> failed-check# (get-in [:clojure.spec.test.check/ret :fail]) (first))
             explain-data# (-> thrown-ex#
                               (ex-data)
                               (assoc ::st/caller sym#))]
         (t/do-report
           (merge {:type     :fail
                   :expected (:ret (fspec-data sym#))}
                  (case (::s/failure explain-data#)
                    :check-failed {:actual  (if (contains? explain-data# ::s/failure)
                                              (::s/value explain-data#)
                                              thrown-ex#)
                                   :message (format "Function call: \n(%s %s)\n\n"
                                                    sym#
                                                    (str/join " " passed-args#))}
                    {:actual  thrown-ex#
                     :message (.getMessage ^Throwable thrown-ex#)})))))
     checks-passed?#))

(defn- fully-qualified-namespace
  [sym]
  (let [metadata (meta (resolve sym))]
    (when metadata
      (symbol (str (:ns metadata)) (str (:name metadata))))))

(defmethod t/assert-expr 'spec-check
  [msg form]
  (let [[_ sym-form opts] form
        sym (fully-qualified-namespace sym-form)]
    (when-not sym (throw (ex-info "Cannot qualify symbol." {:sym sym-form})))
    `(do-spec-check-report '~sym ~opts)))

(defmacro defspec-test
  ([name sym-or-syms] `(defspec-test ~name ~sym-or-syms nil))
  ([name sym-or-syms opts]
   `(t/deftest ~name (do-spec-check-report ~sym-or-syms ~opts))))