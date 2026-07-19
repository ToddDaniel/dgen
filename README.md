# dgen

Generates datasets with exact statistical properties — means, standard
deviations, and correlations — for teaching statistics and validating
analytical workflows. Uses iterative refinement to hit specified parameters
exactly, rather than approximating them.

## Origins

This package builds on the design of MorseGen, originally written by
Dr. Brendan Morse, a gifted statistics educator who taught at Bridgewater
State University until his passing in 2018. His `stat.gen()` and
`cor.gen()` functions, small, elegant tools for generating data that hit
exact target parameters, are the foundation this package extends. This is
offered in his memory.

## Installation

```r
# install.packages("devtools")
devtools::install_github("ToddDaniel/dgen")
```

## Functions

- **Core generators**: `stat_gen()`, `dist_gen()`, `cor_gen()`
- **T-test family**: `t_onesample_gen()`, `t_indep_gen()`, `t_paired_gen()`
- **ANOVA family**: `anova_gen()`, `anova_twoway_gen()`, `anova_blocked_gen()`,
  `anova_mixed_gen()`
- **Chi-square family**: `chisq_gof_gen()`, `chisq_indep_gen()`, `chisq_table_gen()`
- **Proportion family**: `oneprop_gen()`, `twoprop_gen()`, `mcnemar_gen()`
- **Nonparametric family**: `sign_gen()`, `mannwhitney_gen()`,
  `signedrank_gen()`, `kruskal_gen()`, `friedman_gen()`, `spearman_gen()`
- **Regression family**: `make_predictors()`, `bin_gen()`, `reg_gen()`
- **Psychometric scale family**: `scale_gen()`, `item_gen()`
- **Effect-size helpers**: `d_to_means()`, `d_to_means_paired()`,
  `f_to_means()`, `h_to_prop()`
- **Batch generation**: `batch_gen()` — many unique per-student datasets
  from one set of targets, with a compiled instructor answer key
- **Same stats, different graphs**: `anscombe_gen()` — Anscombe-style
  datasets with identical statistics and regression line but completely
  different shapes
- **Data cleaning**: `dirty_data()` — plant known defects (missing
  values, outliers, typos, inconsistent coding, duplicates) in any
  dataset, with an instructor answer key

## License

MIT
