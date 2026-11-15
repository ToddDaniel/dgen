# dgen

Generates datasets with exact statistical properties — means, standard
deviations, and correlations — for teaching statistics and validating
analytical workflows. Uses iterative refinement to hit specified parameters
exactly, rather than approximating them.

## Origins

dgen began as a rescue. In 2012, Dr. Brendan Morse of Bridgewater State
University published MorseGen, a small R package with two elegant functions:
`stat.gen()`, which builds a variable with an exact mean and standard
deviation, and `cor.gen()`, which builds two variables with an exact
correlation. I found it so useful for creating classroom datasets that I kept
coming back to it. After Dr. Morse's death in 2018 the package went
unmaintained, and in 2022 CRAN archived it, so it can no longer be installed
the usual way.

dgen restores that idea and extends it. Its engine generalizes Morse's method
to any number of variables with an exact correlation matrix, keeps every
target exact in the rounded data students actually see, and wraps it in
functions named for the tests instructors teach, from t tests and ANOVA to
chi-square and regression. The code is new; the inspiration is his.

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
- **Proportion family**: `oneprop_gen()`, `twoprop_gen()`, `mcnemar_gen()`
- **Regression family**: `make_predictors()`, `bin_gen()`, `reg_gen()`
- **Psychometric scale family**: `scale_gen()`, `item_gen()`
- **Effect-size helpers**: `d_to_means()`, `d_to_means_paired()`,
  `f_to_means()`, `h_to_prop()`

## License

MIT
