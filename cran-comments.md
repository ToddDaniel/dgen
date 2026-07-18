# CRAN submission comments — dgen 0.1.0

## Test environments

- macOS Sequoia 15.7.4, R 4.5.2 (aarch64-apple-darwin20)
- win-builder (R-devel) — via `devtools::check_win_devel()`

## R CMD check results

Local (macOS): 0 errors | 0 warnings | 0 notes

win-builder (R-devel): 0 errors | 0 warnings | 2 notes

- NOTE: New submission — expected for a first submission.
- NOTE: Found files/directories 'lastMisspelling' 'spell-checking in
  progress' in the temp directory — this is a known win-builder server
  artifact unrelated to the package.

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
