# dgen: Current Package State

This document describes the `dgen` package as it exists in the source tree
today. It supersedes `COMPLETE_PACKAGE_SUMMARY.txt`, which describes an
earlier, more complex design (including an interactive/guided-tutorial
interface) that has since been intentionally removed. Where the two
disagree, this document and the current code are authoritative.

## 1. Purpose & Scope

`dgen` generates datasets with exact statistical properties — means,
standard deviations, and correlations — for teaching statistics and
validating analytical workflows. It is aimed at statistics instructors who
need reproducible datasets that hit specific target parameters: a lecture
example with a mean of exactly 50 and SD of exactly 10, a homework dataset
with a controlled effect size, or a raw dataset that replicates the
descriptive statistics reported in a published study.

Rather than approximating targets through simple random sampling, every
generator uses iterative refinement (repeated sampling, rescaling, and
rounding until the achieved statistics match the targets to the requested
decimal precision) or exact closed-form construction (e.g., proportion
functions compute integer success counts directly).

Design philosophy:

- **Code-only interface.** Every function is called with arguments and
  returns a value — there is no interactive/guided-tutorial mode, no
  `readline()` prompting, and no branching between a "direct" and an
  "interactive" call path. This mirrors the design of the original
  MorseGen `stat.gen()`/`cor.gen()` functions (see Attribution).
- **Arguments in, list out.** Every function returns a single list
  (invisibly) containing the generated data plus supporting statistics —
  targets, achieved values, test results, effect sizes, and assumption
  checks where applicable. Nothing is written to disk unless
  `export_csv = TRUE` is explicitly passed (default `FALSE` throughout).
- **Narrow, defensible scope.** The package covers a fixed set of teaching
  scenarios (t-tests, ANOVA, chi-square, proportions, regression,
  psychometric scales) rather than a general-purpose simulation framework.

## 2. Function Inventory

All 18 exported functions, grouped by family.

### Core generators

- **`stat_gen(n, m, sd, decimals = 2, neg_data = TRUE, seed = NULL, export_csv = FALSE, var_name = "Variable_X")`**
  Generates a single numeric variable with an exact target mean and SD via
  iterative refinement. `neg_data = FALSE` forces non-negative values (for
  variables like age or income).
- **`cor_gen(n, m, sd, r, var_names = NULL, decimals = 2, seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Generates two or more correlated continuous variables with specified
  means, SDs, and a correlation structure (a single value for two
  variables, or a full correlation matrix for three or more), using
  `MASS::mvrnorm()`.

### T-test family

- **`t_onesample_gen(n, m, sd, test_value = 0, var_name = "Score", decimals = 2, neg_data = TRUE, seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Generates data for a one-sample t-test with exact mean/SD, and (by
  default) runs `t.test()` against `test_value`.
- **`t_indep_gen(n1, mean1, sd1, label1, n2, mean2, sd2, label2, var_name = "Score", group_name = "Group", decimals = 2, seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Generates two independent groups with exact target means/SDs, and runs
  both pooled-variance and Welch's t-tests.
- **`t_paired_gen(n, measure1_mean, measure1_sd, measure2_mean, measure2_sd, correlation = 0.70, output_format = "wide", decimals = 2, seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Generates two correlated repeated measurements per participant with
  exact means, SDs, and within-person correlation, in wide, long, or both
  formats; runs a paired t-test.

### ANOVA family

- **`anova_gen(k, n, means, sds, labels = NULL, dv_name = "Score", factor_name = "Group", decimals = 2, seed = NULL, export_csv = FALSE, output_format = "long", run_analysis = TRUE)`**
  Generates a one-way ANOVA design with `k` (≥ 3) groups at exact target
  means/SDs; computes the ANOVA table, effect sizes (eta², omega²),
  Levene's and Shapiro-Wilk assumption checks, and Fisher/Tukey post-hoc
  comparisons.
- **`anova_twoway_gen(factor_a_levels, factor_a_labels, factor_b_levels, factor_b_labels, n_per_cell = 5, cell_means, cell_sds, dv_name = "Score", decimals = 2, seed = NULL, export_csv = FALSE, output_format = "long", run_analysis = TRUE)`**
  Generates a two-way factorial design with exact per-cell means/SDs;
  computes main effects, interaction, effect sizes, and post-hoc tests
  when the interaction is not significant.
- **`anova_blocked_gen(n_per_block, n_blocks, block_labels, n_treatments, treatment_labels, cell_means, within_cell_sd, within_block_correlation = 0.70, dv_name = "Score", decimals = 2, seed = NULL, export_csv = FALSE, output_format = "long", run_analysis = TRUE)`**
  Generates a randomized block (repeated-measures-style) design with exact
  cell means/SDs and controlled within-block correlation; computes the
  block ANOVA table and post-hoc tests for treatment.

### Chi-square family

- **`chisq_gof_gen(n, categories, expected = "equal", expected_values = NULL, var_name = "Category", seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Generates a single categorical variable and runs a chi-square
  goodness-of-fit test against equal, proportional, or count-based expected
  frequencies.
- **`chisq_indep_gen(n, row_var, row_levels, col_var, col_levels, association = "none", cell_probs = NULL, cramers_v = NULL, seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Generates a two-variable categorical dataset with a specified association
  (none, a target Cramér's V, or custom cell probabilities) and runs a
  chi-square test of independence.
- **`chisq_table_gen(nrows, ncols, row_var, col_var, row_levels, col_levels, frequencies, add_id = TRUE, shuffle_rows = TRUE, seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Expands a summary contingency table (cell frequency counts) into
  individual raw observations, for converting textbook tables into
  student-analyzable data.

### Proportion family

- **`oneprop_gen(n, p, test_value = 0.50, var_name = "Success", category_labels = c("Failure", "Success"), seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Generates a binary outcome variable with an exact sample proportion and
  runs both `prop.test()` and `binom.test()` against `test_value`.
- **`twoprop_gen(n1, p1, label1, n2, p2, label2, var_name = "Success", group_name = "Group", category_labels = c("Failure", "Success"), seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Generates binary outcomes for two independent groups with exact
  proportions; runs a two-sample proportion test and the equivalent
  chi-square test, plus Cohen's h, odds ratio, and risk ratio.

### Regression family

- **`make_predictors(n, ..., correlations = NULL, seed = NULL, export_csv = FALSE)`**
  Generates a set of continuous and/or binary predictor variables in one
  call, with an optional target correlation structure between them.
- **`bin_gen(continuous_var, prevalence = 0.5, target_cor, var_name = "Binary_Var", seed = NULL, export_csv = FALSE)`**
  Generates a binary variable with a specified prevalence and point-biserial
  correlation to an existing continuous variable, via a latent-variable
  threshold method.
- **`reg_gen(predictors, betas, outcome_name = "Y", outcome_type = "continuous", r_squared = 0.50, residual_sd = NULL, prevalence = 0.5, decimals = 2, seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Generates a continuous or binary outcome variable from a data frame of
  predictors and a named vector of regression coefficients, targeting a
  specified R² (continuous) or prevalence (binary); fits `lm()`/`glm()`
  automatically.

### Psychometric scale family

- **`scale_gen(n = 250, n_items = 8, n_bad_items = 2, bad_item_type = "low_r", scale_min = 1, scale_max = 7, scale_mean = NULL, scale_sd = NULL, target_alpha = 0.80, var_prefix = "Item", seed = NULL, export_csv = FALSE)`**
  Generates a multi-item Likert scale with a target Cronbach's alpha among
  "good" items, plus a specified number of intentionally problematic items
  (low inter-item correlation, reverse-coded, random, or low-variance), for
  teaching reliability analysis. Returns an instructor key identifying the
  bad items alongside the student-facing data.
- **`item_gen(composite, n_items = 5, scale_min = 1, scale_max = 10, decimals = 0, var_prefix = "Item", seed = NULL, export_csv = FALSE, run_analysis = TRUE)`**
  Back-solves individual scale items that average to a given composite
  score, for giving students raw item-level data to aggregate.

## 3. What's Deliberately Not Included

- **Interactive / guided-tutorial mode.** Earlier drafts of some of these
  functions (including the pre-package originals `stat.gen()` and
  `cor.gen()`) supported being called with no arguments, at which point
  they would prompt the user via `readline()` through a menu ("Direct mode"
  vs. "Interactive mode"), collect each parameter one at a time, print a
  confirmation summary, and ask for a yes/no to proceed. None of that
  exists anywhere in the current package: there is no `readline()`, no
  menu, no mode-selection branching, and no "if no arguments given, ask the
  user" logic in any of the 18 exported functions. This was a deliberate
  cut, not an oversight — every function now has exactly one calling
  convention (arguments in, list out), matching the CRAN submission's
  narrow-scope goal.
- **A general-purpose simulation framework.** The package does not attempt
  to support arbitrary distributions, arbitrary study designs, or a
  formula-based simulation DSL. It covers a fixed set of named teaching
  scenarios (the families listed above).
- **Console output suppression flag.** Every function currently ends with
  an unconditional `cat()`/`print()` summary of the generated
  data/results, disclosed in `cran-comments.md`. There is no `verbose =`
  argument to silence it. This is a conscious, currently-accepted design
  choice, not an interactive-mode remnant — no user input is read, only
  output is printed — but it is worth flagging as a possible future
  refinement if a reviewer objects.

## 4. Current Status

**R CMD check (`--as-cran`, local, MacTeX + Homebrew `tidy-html5`
installed):** 0 errors, 0 warnings, 2 NOTEs.

1. "New submission" — standard boilerplate CRAN attaches to every
   first-time package submission; not a defect.
2. `anova_blocked_gen` example exceeds 5 seconds (~35s elapsed), a result
   of the iterative-refinement loop converging on exact target statistics
   with a within-block correlation constraint. Already disclosed to
   reviewers in `cran-comments.md`.

Both the PDF and HTML manuals render without error.

**Test suite:** 129 tests across 8 `testthat` files, 0 failures, exercising
all 18 exported functions.

**Documentation:** every exported function has a corresponding `.Rd` file
via roxygen2 (`RoxygenNote: 7.3.3`). Package-level documentation
(`R/dgen-package.R` → `man/dgen-package.Rd`) exists, so `?dgen` and
`package?dgen` resolve to an overview page listing all functions by family,
with author/URL/BugReports information pulled from `DESCRIPTION`.

**CRAN readiness:** the package installs, loads, and unloads cleanly with
no dependency issues (`Imports: MASS`; `Depends: R (>= 4.0.0)`). Based on
the current local check results, the package appears ready for submission.

## 5. Attribution

`dgen` builds on the design of MorseGen, originally written by Dr. Brendan
Morse, a statistics educator at Bridgewater State University until his
passing in 2018. His `stat.gen()` and `cor.gen()` functions — small,
exact-target data generators — are the direct ancestors of this package's
`stat_gen()` and `cor_gen()`, and the model for every other generator's
calling convention. This is reflected in `DESCRIPTION` (`Authors@R` lists
Morse as a contributor), in `README.md` (an "Origins" section), and in
source comments directly above `stat_gen()` and `cor_gen()` in the R code.
