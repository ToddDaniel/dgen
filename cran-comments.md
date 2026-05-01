# CRAN submission comments — dgen 0.1.0

## Test environments

- macOS Sequoia 15.7.4, R 4.5.2 (aarch64-apple-darwin20)
- win-builder (R-devel) — via `devtools::check_win_devel()`

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None. This is a new package with no existing reverse dependencies.

## Notes to CRAN reviewers

- The package uses `MASS::mvrnorm()` (in `Imports`) for multivariate
  normal generation required by `cor_gen()`, `t_paired_gen()`,
  `anova_blocked_gen()`, `make_predictors()`, and `scale_gen()`.

- All functions return results invisibly and print a concise console
  summary. No files are written unless `export_csv = TRUE` (default
  `FALSE`).

- `anova_blocked_gen()` example takes ~35 seconds in `R CMD check`
  examples due to the iterative refinement loop converging on exact
  target statistics with within-block correlation. This is expected
  behaviour and is within CRAN's time limits.
