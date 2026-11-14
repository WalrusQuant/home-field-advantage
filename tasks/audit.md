# Code, math, leakage, and local execution audit

Date: 2026-11-14. Existing modified data, charts, and renv activation were preserved.

## Findings and fixes

| Priority | Finding | Resolution |
| --- | --- | --- |
| High | MLB suspended games entered ratings on their original start date with outcomes finalized later. There are 51 affected Retrosheet games since 2005, 45 since 2010. NYM–MIA on 2021-04-11 finished 2021-08-31. | Preserve start dates for predictions and rest/travel; add `outcome_date`; admit outcomes only on subsequent dates. Preserve exponential age from original start. Rebuild the parsed MLB cache as `mlb_games_v2.rds`. Apply the same availability rule to the naive baseline. |
| Medium | API schedules could admit live NBA/NHL scores. | Require completed NBA status and NHL OFF/FINAL status in both release/API paths. Tests cover nonzero live scores, absent/unknown status, and empty results. |
| Medium | Baseline comparison discarded warmup history, while the model received it. | Compute the historical baseline before restricting the evaluation window. |
| Medium | Between-team variance subtracted an unweighted noise estimate from a weighted population variance. | Use the weighted method-of-moments identity below; preserve legitimate zero within-team variance. |
| Medium | Team-IQR bootstrap treated estimated shrinkage as fixed. | Resample whole home-team clusters and refit pooling in every draw. Repeated teams get separate bootstrap IDs. |
| Medium | Future seasons could change home eligibility for a temporary venue within 50 miles. | Makeshift labeling now applies only to already-off-campus venues. Confirmed production impact: all 81 Tampa Bay 2025 home games at Steinbrenner Field (16.38 miles from the usual location) had incorrectly been excluded. Unknown geography is excluded from the true-home sample independently of future modal metadata. |
| Medium | Local renv library lacked testthat and dependencies; bypass flag was ignored by `.Rprofile`. | Hydrated 14 available test packages into the project library. Honor the bypass at startup, add a profile-independent restore script with explicit project library, and load only chart dependencies in chart-only mode. |
| Low | Exponentially growing prefix weights could overflow; POSIX timestamps silently changed chronology units. | Decay running sufficient statistics between event dates; require valid Date inputs and binary neutral flags. Return NA for an unidentified all-neutral joint home coefficient. |
| Low | Elapsed daylight-saving changes were counted as travel across zones; missing attendance reference was labeled normal. | Compare both zones on arrival date; retain unknown attendance classification when reference is unavailable. |

## Math checked

Pregame coefficients solve `(X'WX + Lambda) beta = X'Wy + Lambda * prior`. Direct per-game weighted least-squares tests independently verify every prediction output, home coefficient, and residual scale. Only outcomes with `outcome_date < game_date` enter each fit; same-day scores are unavailable. Residual variance remains `sum(w * residual^2) / sum(w)` with the existing floor and initialization policy.

For eligible team means, let `N = sum(n)`, `Q = sum(n * (mean_team - weighted_mean)^2)`, `k` be the number of teams, and `s2` the pooled within-team variance. The corrected random-effect variance is:

```
tau2 = max(0, (Q - (k - 1) * s2) / (N - sum(n^2) / N))
```

With equal game counts this reduces to sample variance of team means minus `s2/n`. Independent tests also cover unequal counts and noiseless team differences.

## Local execution

Working commands using the installed regular R library:

```sh
HOME_EDGE_NO_RENV=1 Rscript run_all.R
HOME_EDGE_NO_RENV=1 make test
```

Normal `make test` also passes after local hydration. This does not establish an exact lockfile match. The machine runs R 4.4.3; the lockfile records R 4.3.3 and renv 1.3.1, while installed renv is 1.2.4. An attempted exact restore failed because several pinned versions have no available R 4.4 macOS binaries and Matrix 1.6-5 failed to link against missing gfortran. The lockfile was not repinned. `make restore` now runs independently of the project startup profile and targets the project library; it still needs compatible package artifacts/build tools. See [renv restore documentation](https://rstudio.github.io/renv/reference/restore.html).

## Verification

- Original suite: 40 assertions passed.
- Expanded suite: 138 assertions passed, zero failures/warnings/skips, in both regular-library and normal renv runs.
- Same-day/future outcome perturbations, delayed completion, row permutations, truncation, direct WLS, bootstrap/pooling moments, neutral games, final-status filters, and context regressions covered.
- Full integration: passed on 177,811 games in 1.3 minutes; wrote 11 tables, 10 charts, package versions, and results summary. No pipeline warnings/errors. Corrected artifacts are in `cache/audit-2026-11-14/outputs/`; original tracked outputs remain intact. Final defensive adapter changes were separately verified to produce identical inputs on the actual cached NBA/NHL and live MLB fixtures.
- Chart-only entry point also completed successfully.
- Reviewed changes before completion; whitespace verification uses `core.whitespace=cr-at-eol` because existing R sources use CRLF.

## Remaining interpretation and data limits

- MLB API suspension metadata identifies resumed games but does not establish a verified final-completion date. These games are explicitly excluded rather than backdated. The current 2026 feed excludes two rows; missing suspension metadata stops processing. Retrosheet suspended games are retained with completion dates. This changes coverage and may introduce a small selection effect in API-only seasons.
- Venue coordinates may still fall back to a franchise city when a venue cannot be mapped. Consequently reported coordinate coverage is not a measure of verified venue coverage.
- Attendance reference medians and makeshift sublabels are retrospective descriptive classifications, not pregame features. Crowd/travel comparisons are associations, not causal estimates.
- Normal-CDF win probabilities and a shared residual scale are modeling approximations. Bootstrap intervals condition on the estimated pregame ratings and do not capture all shared-opponent/time dependence or model uncertainty.
- Full-season joint coefficients and pooled team edges are descriptive and must not be reused as pregame predictors.
- Existing tracked numerical results were not overwritten. They predate these corrections and should not be used as corrected estimates.

## Observed numerical impact

- MLB: 14,523 model-window neutral-margin predictions changed by more than 1e-8, maximum absolute change 0.227 runs, from the combined outcome-availability and venue corrections.
- NBA/NFL/NHL: no neutral-margin prediction changed by more than 1e-8; maximum differences were floating-point roundoff.
- NFL naive baseline log loss improves from 0.696098 to 0.687272 when it receives its proper warmup history; model log loss remains 0.631550.
- Corrected model / naive baseline log loss: MLB 0.681895 / 0.690865; NBA 0.615601 / 0.683190; NFL 0.631550 / 0.687272; NHL 0.672838 / 0.689932.
- Pooling point estimates and IQR intervals change across leagues; use the regenerated artifacts rather than the pre-audit tracked tables.
