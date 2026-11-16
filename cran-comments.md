# CRAN submission comments: dgen 1.0.0

This is a new submission.

## Test environments

- Local: macOS 15.7 (Sequoia), R 4.6.1, aarch64-apple-darwin
- win-builder (R-devel): to be run before submission
- mac-builder (R-release): to be run before submission

## R CMD check results

Local, `R CMD check --as-cran`: 0 errors | 0 warnings | 0 notes

A "New submission" note is expected on CRAN's incoming checks.

## Downstream dependencies

None. This is a new package.

## Notes for the reviewer

- The package has no dependencies beyond base R and the recommended
  packages (`stats`, `utils`).
- No files are written by default. Data are saved to a CSV file only when
  the user sets `export_csv`; examples and tests that do so write to
  `tempdir()` only.
- Summaries are printed through a print method for a small results object
  and can be turned off with `show_results = FALSE`.
- When a `seed` is supplied, functions call `set.seed()` so that an
  instructor can reproduce a data set; with the default (`seed = NULL`) the
  random number generator is left alone.
- The package is inspired by MorseGen (Morse, 2012), which was archived on
  CRAN in 2022. dgen contains no code from MorseGen.
