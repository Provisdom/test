# provisdom/test

Testing utilities for Clojure/ClojureScript with enhanced spec integration, approximate equality assertions, and instrumentation management.

## Installation

Add to your `deps.edn`:

```clojure
provisdom/test {:git/url "https://github.com/Provisdom/test.git"
                :sha "..."}
```

## Usage

```clojure
(ns my.app-test
  (:require [provisdom.test.core :as t]))
```

## Features

### Assertion Macros

#### Exact Equality
```clojure
(t/is= expected actual)           ; assert equality
(t/is= expected actual "message") ; with message
(t/is-not= a b)                   ; assert inequality
```

#### Approximate Equality (for numerical code)
```clojure
;; Scalar comparison with default tolerance (1e-6)
(t/is-approx= 1.0 1.0000001)

;; Custom absolute tolerance
(t/is-approx= 1.0 1.001 :tolerance 0.01)

;; Relative tolerance (for values spanning orders of magnitude)
(t/is-approx= 1e10 1.0000001e10 :rel-tolerance 1e-6)

;; NaN handling (by default NaN != NaN)
(t/is-approx= ##NaN ##NaN :nan-equal? true)

;; Nested data structures - compares numbers approximately, non-numbers exactly
(t/is-data-approx= {:x 1.0 :y [2.0 3.0]} {:x 1.0000001 :y [2.0 3.0000001]})
```

#### Boolean Assertions
```clojure
(t/is form)      ; re-exported from clojure.test for convenience
(t/is-not form)  ; assert (not form)
```

#### Exception Assertions
```clojure
;; Assert ex-info thrown with specific data
(t/is-thrown-with-data {:type :validation-error}
  (throw (ex-info "bad" {:type :validation-error :field :name})))
```

### Spec Checking

Run generative tests for spec conformance:

```clojure
;; Basic usage
(t/is-spec-check my-fn)

;; With options
(t/is-spec-check my-fn {:num-tests 100
                        :seed 12345})

;; With custom generators
(t/is-spec-check my-fn {:gen {::my-spec my-custom-gen}})

;; Debug mode - when generators are failing and you don't know why
(t/is-spec-check my-fn {:debug true})

;; With timeout (milliseconds)
(t/is-spec-check my-fn {:timeout 5000})

;; As a deftest
(t/defspec-test my-fn-generative-test `my-fn {:num-tests 50})
```

#### Spec Check Options

| Option | Description |
|--------|-------------|
| `:num-tests` | Number of generative tests to run |
| `:seed` | Seed for reproducible tests |
| `:max-size` | Control size of generated values |
| `:gen` | Map from spec names to generator overrides |
| `:debug` | When true, provides detailed generator diagnostics on failure |
| `:timeout` | Timeout in milliseconds |
| `:coll-check-limit` | Number of elements to validate in collection specs |
| `:fspec-iterations` | Times to test anonymous fns in fspec |
| `:recursion-limit` | Soft limit on recursive spec generation |

### Instrumentation

Temporarily enable spec instrumentation:

```clojure
;; Instrument specific functions
(t/with-instrument `my-fn
  (my-fn arg1 arg2))

;; Instrument all instrumentable functions
(t/with-instrument :all
  (run-tests))

;; Resource-based (Clojure only)
(with-open [_ (t/instrumentation {:instrument :all})]
  (run-tests))

;; Check if a function is instrumented
(t/function-instrumented? `my-fn)
```

Uses [Orchestra](https://github.com/jeaye/orchestra) automatically if available, otherwise falls back to `clojure.spec.test.alpha`.

**CLJS Note:** On ClojureScript, `function-instrumented?` always returns `true` because the internal `instrumented-vars` atom is private. This means `with-instrument` won't unstrument functions afterward on CLJS - they remain instrumented. This is usually fine for tests.

### Spec Validation

```clojure
;; Assert value conforms to spec
(t/is-valid ::my-spec value)

;; Check without assertion (returns nil if valid)
(t/is (t/no-problems ::my-spec value))
```

### Retrieving Failed Arguments

When a spec check fails, retrieve the failing arguments:

```clojure
(t/is-spec-check my-fn)  ; fails
(t/get-failed-args `my-fn)  ; => [arg1 arg2 ...]

;; Pretty-print the failed args for debugging
(t/pprint-failed-args `my-fn)
```

## License

Copyright © 2018-2026 Provisdom Corp.

Distributed under the GNU Lesser General Public License version 3.0.
