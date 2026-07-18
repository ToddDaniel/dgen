# CRAN submission comments — dgen 0.1.0

## Test environments

- macOS Sequoia 15.7.4, R 4.5.2 (aarch64-apple-darwin20)
- win-builder (R-devel) — via `devtools::check_win_devel()`

## R CMD check results

Local (macOS): 0 errors | 0 warnings | 1 note

win-builder (R-devel): 0 errors | 0 warnings | 1 note

Both notes are the same single "CRAN incoming feasibility" check, which
contains two expected items for a first submission:

- New submission — standard boilerplate for every first-time package.
- Possibly misspelled words in DESCRIPTION: "McNemar's" — this is
  correctly spelled; it refers to McNemar's test (Quinn McNemar),
  implemented by the package's `mcnemar_gen()` function. Not a typo.

## Downstream dependencies

None. This is a new package with no existing reverse dependencies.

## Notes to CRAN reviewers

- The package uses `MASS::mvrnorm()` (in `Imports`) for multivariate
  normal generation required by `cor_gen()`, `t_paired_gen()`,
  `anova_blocked_gen()`, `make_predictors()`, and `scale_gen()`.

- All functions return results invisibly and emit a concise console
  summary via `message()`, which users can silence with
  `suppressMessages()`. No files are written unless the user opts in
  through the `export_csv` argument (default `FALSE`; `TRUE` or a file
  path enables writing).

- The larger textbook-style example for `anova_blocked_gen()` is wrapped
  in `\donttest{}` because its iterative refinement loop (converging on
  exact target statistics under a within-block correlation constraint)
  takes ~30 seconds. A small example that runs in about a second is
  provided outside `\donttest{}`.
