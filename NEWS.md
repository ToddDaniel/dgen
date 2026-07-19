# dgen (development version)

- New `batch_gen()`: generate many unique per-student datasets from one
  set of targets (distinct seeds), with a compiled instructor answer
  key of each dataset's achieved statistics, effect sizes, and test
  results.
- New `dist_gen()`: generate a deliberately non-normal variable with an
  exact mean and SD. Named shapes (`right_skew`, `left_skew`,
  `heavy_tails`, `light_tails`, `bimodal`, `normal`) with
  mild/moderate/severe presets, or numeric skewness/excess-kurtosis
  targets via the Fleishman power method. Reports achieved sample
  skewness/kurtosis and runs normality diagnostics (Shapiro-Wilk) so
  the data visibly fail the checks they are built to fail.
- New `anscombe_gen()`: Anscombe-style "same statistics, different
  graphs" datasets — every panel shares exact means, SDs, correlation,
  and regression line, with shapes `linear`, `curved`, `outlier`,
  `clusters`, and `fan` (heteroscedastic). Long-format output ready for
  faceted plotting.
- New `anova_mixed_gen()`: mixed-design (between x within, split-plot)
  ANOVA data — the classic Group x Time design. Exact per-cell means
  and SDs, compound-symmetric within-subject correlation (sphericity
  satisfied by construction), the standard split-plot error partition
  with both error terms, effect sizes per effect, and Tukey post-hocs
  on marginal means when the interaction is not significant.
- New `dirty_data()`: plant known defects in any dataset for teaching
  data screening and cleaning — missing values, statistically
  plausible outliers, data-entry typos (shifted decimals, transposed
  digits), inconsistent category coding, and duplicated rows — with a
  unified instructor answer key recording every planted defect
  (optional via `answer_key = FALSE`). Accepts a data frame, a dgen
  generator result, or a CSV path; `export_csv` writes only the
  student-facing data, never the key.

# dgen 0.1.0

## Initial release

19 data-generation functions and 4 effect-size helpers for teaching
statistics and validating analytical workflows. Each generator uses
iterative refinement or exact closed-form construction to achieve exact
target means, standard deviations, correlations, and proportions rather
than approximations.

All generators accept `export_csv = FALSE` (default), `TRUE`
(auto-generated filename in the working directory), or a file path.
Console summaries are emitted with `message()` and can be silenced with
`suppressMessages()`.

### Core generators
- `stat_gen()` — single continuous variable with exact mean and SD
- `cor_gen()` — multiple correlated continuous variables

### T-test family
- `t_onesample_gen()` — one-sample t-test data
- `t_indep_gen()` — independent samples t-test data (pooled and Welch)
- `t_paired_gen()` — paired samples t-test data with controlled
  within-person correlation

### ANOVA family
- `anova_gen()` — one-way ANOVA with post-hoc tests (Fisher LSD,
  Tukey HSD)
- `anova_twoway_gen()` — two-way factorial ANOVA with interaction and
  simple effects
- `anova_blocked_gen()` — randomized block design ANOVA with
  within-block correlation control

### Chi-square family
- `chisq_gof_gen()` — chi-square goodness-of-fit data
- `chisq_indep_gen()` — chi-square test of independence data
- `chisq_table_gen()` — expands a frequency table to individual
  observations

### Proportion family
- `oneprop_gen()` — one-sample proportion test data
- `twoprop_gen()` — two-sample proportion test data
- `mcnemar_gen()` — paired proportions (McNemar test) data with exact
  marginal proportions and controllable discordant pairs

### Regression family
- `make_predictors()` — multiple predictors (continuous and binary)
  with optional correlation structure
- `bin_gen()` — binary variable with controlled point-biserial
  correlation
- `reg_gen()` — continuous or binary outcome from specified regression
  coefficients and target R-squared

### Psychometric scale family
- `scale_gen()` — multi-item Likert scale with intentional bad items
  for teaching reliability analysis
- `item_gen()` — back-solves individual scale items from composite
  scores

### Effect-size helpers
- `d_to_means()` — Cohen's d to target means for independent groups
- `d_to_means_paired()` — paired Cohen's d to target means for repeated
  measures
- `f_to_means()` — Cohen's f (or eta-squared) to target group means for
  one-way ANOVA
- `h_to_prop()` — Cohen's h to a second target proportion
