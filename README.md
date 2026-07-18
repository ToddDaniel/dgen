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

- **Core generators**: `stat_gen()`, `cor_gen()`
- **T-test family**: `t_onesample_gen()`, `t_indep_gen()`, `t_paired_gen()`
- **ANOVA family**: `anova_gen()`, `anova_twoway_gen()`, `anova_blocked_gen()`
- **Chi-square family**: `chisq_gof_gen()`, `chisq_indep_gen()`, `chisq_table_gen()`
- **Proportion family**: `oneprop_gen()`, `twoprop_gen()`
- **Regression family**: `make_predictors()`, `bin_gen()`, `reg_gen()`
- **Psychometric scale family**: `scale_gen()`, `item_gen()`

## License

MIT
