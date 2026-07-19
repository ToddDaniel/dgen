#' Generate Data for a Mann-Whitney U Test
#'
#' Creates two independent groups with EXACT target medians and a
#' deliberately non-normal shape, then runs R's canonical
#' \code{wilcox.test()} — the Mann-Whitney U / Wilcoxon rank-sum test,
#' the nonparametric counterpart of [t_indep_gen()]. The medians are
#' exact (an additive shift moves the sample median precisely onto the
#' target without disturbing the shape); the \code{spread} is an
#' SD-like scale that is approximate by design. Both groups share the
#' same shape and spread, matching the test's similar-shapes assumption
#' for interpreting results as a median comparison.
#'
#' Significance is controlled by the median separation relative to
#' \code{spread} and the sample sizes; the achieved U, z, p, and effect
#' sizes are reported so you can nudge \code{spread} or n to land the
#' result you are teaching toward. Data are integer-valued by default
#' (\code{decimals = 0}), producing the ties real ordinal data have;
#' tie-related warnings from \code{wilcox.test()} are suppressed and the
#' tie-corrected result is reported, matching SPSS and JASP.
#'
#' Effect sizes: \code{r = z / sqrt(N)} (z recovered from the reported
#' p-value, signed by direction) and the rank-biserial correlation
#' \code{2 * U1 / (n1 * n2) - 1} (positive when Group 1 tends larger),
#' as reported by JASP.
#'
#' @param n1 Sample size for Group 1
#' @param median1 Target median for Group 1 (hit exactly)
#' @param label1 Label for Group 1 (default = \code{"Group1"})
#' @param n2 Sample size for Group 2
#' @param median2 Target median for Group 2 (hit exactly)
#' @param label2 Label for Group 2 (default = \code{"Group2"})
#' @param spread SD-like scale of both groups (approximate)
#' @param shape Distribution shape, as in [dist_gen()]
#'   (default = \code{"right_skew"} — the situation that motivates the
#'   test)
#' @param severity Shape severity: \code{"mild"}, \code{"moderate"}
#'   (default), or \code{"severe"}
#' @param scale_min,scale_max Optional bounds (e.g., a 1-100 rating
#'   scale); values are clamped and the target medians must lie strictly
#'   inside
#' @param var_name Name for the outcome variable (default =
#'   \code{"Score"})
#' @param group_name Name for the grouping variable (default =
#'   \code{"Group"})
#' @param decimals Number of decimal places (default = 0 — integer,
#'   tie-rich data). Target medians must be achievable on this grid
#'   (odd n: multiples of the step; even n: half-steps allowed)
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run the Mann-Whitney test automatically?
#'   (default = \code{TRUE})
#'
#' @return (Invisibly) a list with:
#'   \item{data}{Long-format data frame (group factor + outcome)}
#'   \item{group_stats}{n, exact median, mean rank, and IQR per group}
#'   \item{test_results}{The \code{wilcox.test()} result (U statistic
#'     as R's W)}
#'   \item{z}{z recovered from the reported p-value, signed by
#'     direction}
#'   \item{effect_sizes}{\code{r} (= z/sqrt(N)) and
#'     \code{rank_biserial}}
#'   \item{iterations}{Refinement iterations per group}
#'
#' @seealso [t_indep_gen()] for the parametric counterpart,
#'   [kruskal_gen()] for 3+ groups, [dist_gen()] for the shape engine
#'
#' @examples
#' # Right-skewed spending data: urban vs resort properties
#' result <- mannwhitney_gen(
#'   n1 = 12, median1 = 66, label1 = "Urban",
#'   n2 = 14, median2 = 85, label2 = "Resort",
#'   spread = 25, var_name = "Spending", seed = 42
#' )
#' result$test_results
#' result$effect_sizes
#'
#' @concept Mann-Whitney U
#' @concept Wilcoxon rank-sum
#' @concept nonparametric
#'
#' @export
mannwhitney_gen <- function(n1,
                            median1,
                            label1     = "Group1",
                            n2,
                            median2,
                            label2     = "Group2",
                            spread,
                            shape      = "right_skew",
                            severity   = "moderate",
                            scale_min  = NULL,
                            scale_max  = NULL,
                            var_name   = "Score",
                            group_name = "Group",
                            decimals   = 0,
                            seed       = NULL,
                            export_csv = FALSE,
                            run_analysis = TRUE) {

  if (!is.numeric(n1) || n1 < 3 || n1 != round(n1))
    stop("'n1' must be an integer >= 3")
  if (!is.numeric(n2) || n2 < 3 || n2 != round(n2))
    stop("'n2' must be an integer >= 3")
  if (label1 == label2) stop("Group labels must differ")
  npar_validate_common(spread, shape, severity, scale_min, scale_max,
                       decimals)
  npar_validate_median(median1, n1, decimals, scale_min, scale_max)
  npar_validate_median(median2, n2, decimals, scale_min, scale_max)

  if (!is.null(seed)) set.seed(seed)
  draw_fun <- dist_shape_params(shape, severity)$draw

  s1 <- npar_sample(n1, median1, spread, draw_fun, decimals,
                    scale_min, scale_max)
  s2 <- npar_sample(n2, median2, spread, draw_fun, decimals,
                    scale_min, scale_max)

  data_out <- data.frame(
    g = factor(c(rep(label1, n1), rep(label2, n2)),
               levels = c(label1, label2)),
    y = c(s1$x, s2$x),
    stringsAsFactors = FALSE
  )
  colnames(data_out) <- c(group_name, var_name)

  rk <- rank(data_out[[var_name]])
  group_stats <- data.frame(
    Group     = c(label1, label2),
    n         = c(n1, n2),
    Median    = c(stats::median(s1$x), stats::median(s2$x)),
    Mean_Rank = c(mean(rk[seq_len(n1)]), mean(rk[n1 + seq_len(n2)])),
    IQR       = c(stats::IQR(s1$x), stats::IQR(s2$x)),
    stringsAsFactors = FALSE
  )
  colnames(group_stats)[1] <- group_name

  test_results <- NULL
  z <- NULL
  effect_sizes <- NULL

  if (run_analysis) {
    test_results <- suppressWarnings(
      stats::wilcox.test(s1$x, s2$x, correct = TRUE))
    U1 <- as.numeric(test_results$statistic)
    direction <- group_stats$Mean_Rank[1] - group_stats$Mean_Rank[2]
    z <- npar_z_from_p(test_results$p.value,
                       if (direction == 0) 1 else direction)
    effect_sizes <- list(
      r             = z / sqrt(n1 + n2),
      rank_biserial = 2 * U1 / (n1 * n2) - 1
    )
  }

  dgen_export(data_out, export_csv,
              paste0(var_name, "_by_", group_name, "_mannwhitney_dgen.csv"))

  message(sprintf("Mann-Whitney data: %s (n=%d, Mdn=%s) vs %s (n=%d, Mdn=%s)",
                  label1, n1, format(group_stats$Median[1]),
                  label2, n2, format(group_stats$Median[2])))
  if (run_analysis) {
    message(sprintf("wilcox.test: U = %.1f, z = %.3f, p = %s | r = %.3f | rank-biserial = %.3f",
                    as.numeric(test_results$statistic), z,
                    format.pval(test_results$p.value, digits = 3,
                                eps = 0.001),
                    effect_sizes$r, effect_sizes$rank_biserial))
  }

  result <- list(
    data        = data_out,
    group_stats = group_stats,
    iterations  = c(s1$iterations, s2$iterations)
  )
  if (run_analysis) {
    result$test_results <- test_results
    result$z            <- z
    result$effect_sizes <- effect_sizes
  }
  invisible(result)
}
