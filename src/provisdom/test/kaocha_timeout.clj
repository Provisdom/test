(ns provisdom.test.kaocha-timeout
  "Kaocha hooks for enforcing a per-test timeout.

   Tests exceeding the timeout are interrupted and marked as errors.
   Default timeout is 100 seconds.

   Usage in tests.edn:
   ```
   #kaocha/v1
   {:plugins [...  :kaocha.plugin/hooks]
    :kaocha.hooks/config   [provisdom.test.kaocha-timeout/config-hook]
    :kaocha.hooks/wrap-run [provisdom.test.kaocha-timeout/wrap-run-hook]
    :kaocha.hooks/post-test [provisdom.test.kaocha-timeout/post-test-hook]
    :provisdom.test.kaocha-timeout/timeout-seconds 100}
   ```"
  (:require
    [kaocha.hierarchy :as hierarchy]))

(defn- test-var?
  "Returns true if testable is a test var (deftest)."
  [testable]
  (hierarchy/leaf? testable))

(defn- run-with-timeout
  "Runs the test function with a timeout. Returns the result or a timeout failure."
  [run testable test-plan timeout-ms]
  (let [result-promise (promise)
        test-thread (doto (Thread.
                            (fn []
                              (try
                                (deliver result-promise (run testable test-plan))
                                (catch Throwable t
                                  (deliver result-promise
                                    (assoc testable
                                      :kaocha.result/count 1
                                      :kaocha.result/error 1
                                      :kaocha.result/fail 0
                                      :kaocha.result/pass 0
                                      :kaocha.result/pending 0
                                      ::timeout-exception t))))))
                      (.setDaemon true))]
    (.start test-thread)
    (let [result (deref result-promise timeout-ms ::timeout)]
      (if (= result ::timeout)
        (do
          (.interrupt test-thread)
          (assoc testable
            :kaocha.result/count 1
            :kaocha.result/error 1
            :kaocha.result/fail 0
            :kaocha.result/pass 0
            :kaocha.result/pending 0
            ::timed-out true
            ::timeout-seconds (/ timeout-ms 1000)))
        result))))

;;;CONFIG
(def ^:private timeout-seconds-atom (atom 100))

(defn config-hook
  "Hook function for :kaocha.hooks/config. Reads timeout from config."
  [config]
  (let [timeout (or (:provisdom.test.kaocha-timeout/timeout-seconds config) 100)]
    (reset! timeout-seconds-atom timeout)
    config))

;;;WRAP-RUN
(defn wrap-run-hook
  "Hook function for :kaocha.hooks/wrap-run. Wraps test execution with timeout."
  [run]
  (let [timeout-seconds @timeout-seconds-atom
        timeout-ms (* timeout-seconds 1000)]
    (fn [testable test-plan]
      (if (test-var? testable)
        (run-with-timeout run testable test-plan timeout-ms)
        (run testable test-plan)))))

;;;POST-TEST
(defn post-test-hook
  "Hook function for :kaocha.hooks/post-test. Reports timeouts."
  [testable _test-plan]
  (when (::timed-out testable)
    (println (str "\n⏱️  TIMEOUT: " (:kaocha.testable/id testable)
               " exceeded " (::timeout-seconds testable) " seconds\n")))
  testable)
