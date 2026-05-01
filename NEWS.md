# dgen 0.1.0

## Initial release

18 data-generation functions for teaching statistics and validating
analytical workflows. Each function uses iterative refinement to achieve
exact target means, standard deviations, and correlations rather than
approximations.

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
