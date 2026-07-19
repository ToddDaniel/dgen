#' Generate Data for a Wilcoxon Signed-Rank Test
#'
#' Creates paired measurements (e.g., before/after on the same
#' participants) with EXACT per-measure medians, controlled
#' within-person correlation, and a deliberately non-normal shape, then
#' runs R's canonical paired \code{wilcox.test()} — the nonparametric
#' counterpart of [t_paired_gen()]. Named \code{signedrank_gen} rather
#' than "wilcoxon" because the Mann-Whitney U is *also* a Wilcoxon test
#' (rank-sum); this one is the signed-rank test for paired data.
#'
#' Pairs whose rounded difference is zero are dropped by the test (as
#' R, SPSS, and JASP all do); the reported \code{n_used} counts the
#' non-zero differences. Tie- and zero-related warnings from
#' \code{wilcox.test()} are suppressed and the corrected result is
#' reported. Effect sizes: \code{r = z / sqrt(n_used)} and the matched
#' rank-biserial correlation \code{(W+ - W-) / (W+ + W-)} (positive
#' when measure 2 tends larger), as reported by JASP.
#'
#' @param n Number of pairs
#' @param median1 Target median for measurement 1 (hit exactly)
#' @param median2 Target median for measurement 2 (hit exactly)
#' @param measure1_label Label for measurement 1 (default =
#'   \code{"Measure1"})
#' @param measure2_label Label for measurement 2 (default =
#'   \code{"Measure2"})
#' @param correlation Within-person correlation of the underlying draws
#'   (approximate; default = 0.70)
#' @param spread SD-like scale of both measurements (approximate)
#' @param shape Distribution shape, as in [dist_gen()]
#'   (default = \code{"right_skew"})
#' @param severity Shape severity (default = \code{"moderate"})
#' @param scale_min,scale_max Optional bounds; values are clamped and
#'   target medians must lie strictly inside
#' @param var_name Name for the measured variable (default =
#'   \code{"Score"})
#' @param id_name Name for the participant ID (default = \code{"ID"})
#' @param decimals Number of decimal places (default = 0)
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run the signed-rank test automatically?
#'   (default = \code{TRUE})
#'
#' @return (Invisibly) a list with:
#'   \item{data}{Wide-format data frame: ID, measurement 1,
#'     measurement 2}
#'   \item{measure_stats}{Exact median and IQR per measurement, plus
#'     the median difference}
#'   \item{n_used}{Number of non-zero differences the test used}
#'   \item{test_results}{The paired \code{wilcox.test()} result (V =
#'     sum of positive ranks)}
#'   \item{z}{z recovered from the reported p-value, signed by
#'     direction}
#'   \item{effect_sizes}{\code{r} and \code{rank_biserial}}
#'   \item{correlation_achieved}{Achieved correlation between the two
#'     measurements}
#'   \item{iterations}{Joint refinement iterations}
#'
#' @seealso [t_paired_gen()] for the parametric counterpart,
#'   [sign_gen()] for the direction-only paired test
#'
#' @examples
#' # Call handle times before and after a new phone script (right-skewed)
#' result <- signedrank_gen(
#'   n = 18, median1 = 6, median2 = 5,
#'   measure1_label = "Old_Script", measure2_label = "New_Script",
#'   spread = 2, var_name = "Minutes", scale_min = 1, seed = 42
#' )
#' result$test_results
#' result$effect_sizes
#'
#' @concept Wilcoxon signed-rank
#' @concept nonparametric
#' @concept paired data
#'
#' @export
signedrank_gen <- function(n,
                           median1,
                           median2,
                           measure1_label = "Measure1",
                           measure2_label = "Measure2",
                           correlation    = 0.70,
                           spread,
                           shape          = "right_skew",
                           severity       = "moderate",
                           scale_min      = NULL,
                           scale_max      = NULL,
                           var_name       = "Score",
                           id_name        = "ID",
                           decimals       = 0,
                           seed           = NULL,
                           export_csv     = FALSE,
                           run_analysis   = TRUE) {

  if (!is.numeric(n) || n < 5 || n != round(n))
    stop("'n' must be an integer >= 5")
  if (measure1_label == measure2_label)
    stop("Measure labels must differ")
  if (!is.numeric(correlation) || correlation <= -1 || correlation >= 1)
    stop("'correlation' must be strictly between -1 and 1")
  npar_validate_common(spread, shape, severity, scale_min, scale_max,
                       decimals)
  npar_validate_median(median1, n, decimals, scale_min, scale_max)
  npar_validate_median(median2, n, decimals, scale_min, scale_max)

  if (!is.null(seed)) set.seed(seed)
  draw_fun <- dist_shape_params(shape, severity)$draw

  cs <- npar_correlated_sample(n, c(median1, median2), spread,
                               correlation, draw_fun, decimals,
                               scale_min, scale_max)
  m1 <- cs$m[, 1]
  m2 <- cs$m[, 2]

  data_out <- data.frame(id = seq_len(n), a = m1, b = m2,
                         stringsAsFactors = FALSE)
  colnames(data_out) <- c(id_name, measure1_label, measure2_label)

  diffs  <- m2 - m1
  n_used <- sum(diffs != 0)

  measure_stats <- data.frame(
    Measure = c(measure1_label, measure2_label),
    Median  = c(stats::median(m1), stats::median(m2)),
    IQR     = c(stats::IQR(m1), stats::IQR(m2)),
    stringsAsFactors = FALSE
  )
  median_difference <- stats::median(diffs)

  test_results <- NULL
  z <- NULL
  effect_sizes <- NULL

  if (run_analysis) {
    test_results <- suppressWarnings(
      stats::wilcox.test(m2, m1, paired = TRUE, correct = TRUE))
    V <- as.numeric(test_results$statistic)      # W+ for m2 - m1
    total_W <- n_used * (n_used + 1) / 2
    W_minus <- total_W - V
    direction <- V - W_minus
    z <- npar_z_from_p(test_results$p.value,
                       if (direction == 0) 1 else direction)
    effect_sizes <- list(
      r             = z / sqrt(n_used),
      rank_biserial = if (total_W > 0) (V - W_minus) / total_W else 0
    )
  }

  dgen_export(data_out, export_csv,
              paste0(var_name, "_signedrank_dgen.csv"))

  message(sprintf(
    "Signed-rank data: %s (Mdn=%s) vs %s (Mdn=%s), n = %d pairs (%d non-zero)",
    measure1_label, format(measure_stats$Median[1]),
    measure2_label, format(measure_stats$Median[2]), n, n_used))
  if (run_analysis) {
    message(sprintf("wilcox.test (paired): V = %.1f, z = %.3f, p = %s | r = %.3f",
                    as.numeric(test_results$statistic), z,
                    format.pval(test_results$p.value, digits = 3,
                                eps = 0.001),
                    effect_sizes$r))
  }

  result <- list(
    data                 = data_out,
    measure_stats        = measure_stats,
    median_difference    = median_difference,
    n_used               = n_used,
    correlation_achieved = stats::cor(m1, m2),
    iterations           = cs$iterations
  )
  if (run_analysis) {
    result$test_results <- test_results
    result$z            <- z
    result$effect_sizes <- effect_sizes
  }
  invisible(result)
}
