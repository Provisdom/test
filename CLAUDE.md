# test

Library-specific notes for `provisdom.test`.

## See Also

- `../../CLAUDE.md` — repo-wide conventions
- `../../rules/testing.md` — testing rules (read the CRITICAL section first)
- `../../rules/file-review.md` — review workflows
- `../../rules/anti-patterns.md` — quick reference card

## Library-specific conventions

- **No dependency on `provisdom.math`.** Generators here use `clojure.spec.gen.alpha` directly — `gen/such-that`, `s/gen*`, `gen/double*` etc., not `m/finite-gen`. Test code can't reference the testing primitives it provides (circular dep), and the math library's wrappers depend on this library transitively. The repo-wide CRITICAL §1 rule "no bounded value-range gens" still applies; only the *implementation* of the generators differs.
- **`deftest`, `is=`, `is-approx=`, `is-data-approx=`, `with-instrument`, `is-spec-check` are compile-time macros** with ClojureScript / Clojure dispatch via `(cljs-env? &env)` at expansion time. They expand to `cljs.test/*` or `clojure.test/*` accordingly. A given test file targets one platform — the macros do not transparently bridge.
- **Kaocha integration: `provisdom.test.kaocha-timeout`** enforces a 100-second hard cap per deftest in CI via three hooks (`config-hook`, `wrap-run-hook`, `post-test-hook`). Tests hitting the cap need to be fixed, not exempted. Configuration in each library's `tests.edn` reads `:provisdom.test.kaocha-timeout/timeout-seconds 100`.
- **CLJS instrumentation cleanup is broken.** On Clojure, `with-instrument` correctly unstruments on exit (the `instrumented-vars` atom is accessible). On ClojureScript, that atom is private, so `function-instrumented?` always returns true and `with-instrument` leaves functions instrumented after the body. CLJS tests effectively share instrumentation state across the suite — usually fine, but worth knowing for debugging.
- **Failure diagnostics**: `failed-args-store` captures failing arguments from `t/is-spec-check`; inspect via `get-failed-args` or `pprint-failed-args`. The `:debug true` option drops `gen/such-that`'s max-tries from 100 → 10, surfacing generator failures faster. `my-gensub` wraps `s/gen*` to attach metadata about non-conformance.
