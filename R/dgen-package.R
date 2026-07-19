#' dgen: Data Generator for Statistical Teaching and Testing
#'
#' Generates datasets with exact statistical properties -- means, standard
#' deviations, and correlations -- for teaching statistics and validating
#' analytical workflows. Uses iterative refinement to hit specified
#' parameters exactly, rather than approximating them. Each function
#' generates data and (by default) automatically runs the corresponding
#' statistical test, returning results alongside effect sizes and
#' assumption checks.
#'
#' @section Core generators:
#' \describe{
#'   \item{\code{\link{stat_gen}}}{Single variable with an exact mean and SD}
#'   \item{\code{\link{dist_gen}}}{Single non-normal variable (skewed,
#'     heavy/light-tailed, or bimodal) with an exact mean and SD}
#'   \item{\code{\link{cor_gen}}}{Multiple correlated variables}
#' }
#'
#' @section T-test family:
#' \describe{
#'   \item{\code{\link{t_onesample_gen}}}{One-sample t-test data}
#'   \item{\code{\link{t_indep_gen}}}{Independent samples t-test data}
#'   \item{\code{\link{t_paired_gen}}}{Paired samples t-test data}
#' }
#'
#' @section ANOVA family:
#' \describe{
#'   \item{\code{\link{anova_gen}}}{One-way ANOVA data}
#'   \item{\code{\link{anova_twoway_gen}}}{Two-way factorial ANOVA data}
#'   \item{\code{\link{anova_blocked_gen}}}{Randomized block design ANOVA data}
#' }
#'
#' @section Chi-square family:
#' \describe{
#'   \item{\code{\link{chisq_gof_gen}}}{Goodness-of-fit test data}
#'   \item{\code{\link{chisq_indep_gen}}}{Test of independence data}
#'   \item{\code{\link{chisq_table_gen}}}{Expand a frequency table to raw observations}
#' }
#'
#' @section Proportion family:
#' \describe{
#'   \item{\code{\link{oneprop_gen}}}{One-sample proportion test data}
#'   \item{\code{\link{twoprop_gen}}}{Two-sample proportion test data}
#'   \item{\code{\link{mcnemar_gen}}}{Paired proportions (McNemar test) data}
#' }
#'
#' @section Regression family:
#' \describe{
#'   \item{\code{\link{make_predictors}}}{Predictor variables, continuous and/or binary}
#'   \item{\code{\link{bin_gen}}}{Binary variable correlated with a continuous variable}
#'   \item{\code{\link{reg_gen}}}{Outcome variable from specified regression coefficients}
#' }
#'
#' @section Psychometric scale family:
#' \describe{
#'   \item{\code{\link{scale_gen}}}{Multi-item Likert scale with intentional bad items}
#'   \item{\code{\link{item_gen}}}{Back-solved items from a composite score}
#' }
#'
#' @section Same statistics, different graphs:
#' \describe{
#'   \item{\code{\link{anscombe_gen}}}{Anscombe-style datasets with
#'     identical means, SDs, correlation, and regression line, but
#'     completely different shapes when plotted}
#' }
#'
#' @section Batch generation:
#' \describe{
#'   \item{\code{\link{batch_gen}}}{Many unique per-student datasets from
#'     one set of targets, with a compiled instructor answer key}
#' }
#'
#' @section Effect-size helpers:
#' \describe{
#'   \item{\code{\link{d_to_means}}}{Cohen's d to target means for
#'     independent groups}
#'   \item{\code{\link{d_to_means_paired}}}{Paired Cohen's d to target means
#'     for repeated measures}
#'   \item{\code{\link{f_to_means}}}{Cohen's f (or eta-squared) to target
#'     group means for ANOVA}
#'   \item{\code{\link{h_to_prop}}}{Cohen's h to a second target proportion}
#' }
#'
#' @section Origins:
#' This package builds on the design of MorseGen, originally written by
#' Dr. Brendan Morse. See the README for details.
#'
#' @keywords internal
"_PACKAGE"
