# Local execution and statistical audit

- [x] Reproduce package/startup failures; inspect R and renv configuration.
- [x] Independently audit pregame math and chronological isolation.
- [x] Audit preprocessing, inference, and test coverage for leakage and statistical errors.
- [x] Fix confirmed defects with focused regression tests, preserving existing user changes.
- [x] Run tests and feasible integration checks; review final diff and refactor.

## Scope and verification
Review the R pipeline, input adapters, weighting, bootstrap/pooling, and validation baselines. Compare model predictions with direct weighted least squares and test invariance to future outcomes. Verify startup without downloading all sports feeds where possible. Existing modified data, charts, renv activation, and run_all.R belong to the user and must be preserved. Commit only after review and explicit user authorization.

## Review
Completed code review and focused refactoring. 138 assertions pass; full isolated pipeline succeeds on 177,811 games, producing 11 tables and 10 charts. Fixed Date binding failure discovered by integration. Final adapter inputs verified identical after defensive validation changes. See tasks/audit.md for detailed findings, numerical impact, package limitations, and corrected output location. Existing tracked outputs preserved.

## Confirmed findings and implementation decisions
- Existing suite: 40 passing assertions. Normal renv library lacked testthat; hydrated it and dependencies from working installed packages. Normal make test now succeeds.
- Exact lockfile restore fails on unavailable R 4.4 macOS binaries and Matrix 1.6-5 missing gfortran. Preserve lockfile; provide working regular-library bypass and startup-independent restore command.
- Correct delayed-label handling for MLB suspended games: predict at start, admit final result only after completion date; version parsed cache so old rows cannot bypass fix.
- Require final status for NBA/NHL inputs.
- Correct validation baseline warmup, pooling moments, and refit pooling inside team-cluster bootstrap.
- Prevent future makeshift classification from changing home eligibility; fix DST crossing and unknown attendance reference handling.
- Run full cached integration in isolated temporary copy to preserve pre-existing user outputs.

## Integration re-plan
First full run exposed Date class loss in the custom league row binder. Replaced it with dplyr::bind_rows, retained the completion-date contract, and restarted full integration using the rebuilt versioned MLB cache. Unit tests alone did not establish this cross-league path.

## Commit and publication review

- [x] Review intended source, tests, documentation, and tracked-file inventory.
- [x] Scan tracked and proposed files for secrets/private paths; verify ignore rules.
- [x] Confirm focused tests and independent review.
- [x] Approve the explicit commit manifest after review; commit locally (no push requested).

Exclude pre-existing generated output/data modifications and renv/activate.R changes. Raw downloads, processed audit artifacts, R history, local package libraries, credentials, and environment files remain local. No additional game-level data is included in this commit.

Precommit verification: 51 tracked/proposed text files scanned with no credential-pattern or personal-path findings; no tracked local-only files. Ignore rules verified for credentials, R history/state, caches, and package libraries. All 138 test assertions pass. Independent review found no blockers.
