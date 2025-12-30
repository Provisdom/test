# Change Log

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

### Added
- `is-thrown-with-data` macro for asserting ex-info exceptions with specific data
- `:rel-tolerance` option for `is-approx=` and `is-data-approx=` - relative tolerance as fraction of max(|x1|,|x2|)
- `:timeout` option for `is-spec-check` - timeout in milliseconds for long-running spec checks
- `:debug` option for `is-spec-check` - reduces such-that max-tries for faster failure and better diagnostics
- Shrinking feedback in spec-check failure reports (shows shrink count and pass count before failure)
- Comprehensive README documentation with usage examples

### Changed
- `spec-check-report` now prefers shrunk args when available for more minimal failing cases
- `spec-check-report` now includes pass count and shrink depth in error messages
- Improved docstrings for `function-instrumented?`, `is-approx=`, `is-data-approx=`, `is-spec-check`

### Fixed
- Documented CLJS limitation where `function-instrumented?` always returns true
- `is-valid` macro now properly resolves `is` (was using unqualified symbol)

## [0.1.0] - 2018

### Added
- Initial release with core testing utilities
- `is=`, `is-not=`, `is-approx=`, `is-data-approx=` assertion macros
- `is-spec-check`, `defspec-test` for generative spec testing
- `with-instrument`, `instrumentation` for spec instrumentation management
- `:throws` support in fdef specs
- Orchestra integration for stricter spec checking
