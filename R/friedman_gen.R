#' Generate Data for a Friedman Test
#'
#' Creates repeated-measures data — every subject measured under all k
#' conditions — with EXACT per-condition medians, controlled
#' within-subject correlation, and a deliberately non-normal shape, then
#' runs R's canonical \code{friedman.test()} — the nonparametric
#' counterpart of a one-way repeated-measures ANOVA (see
#' [anova_blocked_gen()] for the parametric analogue).
#'
#' The effect size reported is Kendall's W
#' (\code{chi-squared / (n * (k - 1))}), ranging 0-1, the standard
#' companion to the Friedman test. When the omnibus test is
#' significant, pairwise paired \code{wilcox.test()} comparisons with
#' Holm adjustment are computed — the conventional follow-up.
#'
#' @param n Number of subjects (minimum 5)
#' @param k Number of conditions (minimum 3)
#' @param medians Numeric vector of k target condition medians (hit
#'   exactly)
#' @param labels Character vector of condition labels (default
#'   \code{"Cond1"}, \code{"Cond2"}, ...)
#' @param correlation Within-subject correlation of the underlying
#'   draws (approximate; default = 0.70)
#' @param spread SD-like scale of every condition (approximate)
#' @param shape Distribution shape, as in [dist_gen()]
#'   (default = \code{"right_skew"})
#' @param severity Shape severity (default = \code{"moderate"})
#' @param scale_min,scale_max Optional bounds; values are clamped and
#'   target medians must lie strictly inside
#' @param dv_name Name for the measured variable (default =
#'   \code{"Score"})
#' @param id_name Name for the subject ID (default = \code{"ID"})
#' @param decimals Number of decimal places (default = 0)
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run the Friedman test automatically?
#'   (default = \code{TRUE})
#'
#' @return (Invisibly) a list with:
#'   \item{data}{Wide-format data frame: ID plus one column per
#'     condition}
#'   \item{data_long}{Long-format data frame (ID, condition, score)}
#'   \item{condition_stats}{Exact median and IQR per condition}
#'   \item{test_results}{The \code{friedman.test()} result}
#'   \item{kendalls_w}{Kendall's W effect size}
#'   \item{posthoc}{Holm-adjusted pairwise signed-rank comparisons
#'     (only when the omnibus test is significant)}
#'   \item{correlation_matrix}{Achieved correlations between
#'     conditions}
#'   \item{iterations}{Joint refinement iterations}
#'
#' @seealso [anova_blocked_gen()] for the parametric counterpart,
#'   [signedrank_gen()] for two conditions
#'
#' @examples
#' result <- friedman_gen(
#'   n = 12, k = 3,
#'   medians = c(5, 6, 8),
#'   labels  = c("Baseline", "Midterm", "Final"),
#'   spread  = 2, scale_min = 1, scale_max = 10, seed = 42
#' )
#' result$test_results
#' result$kendalls_w
#'
#' @concept Friedman test
#' @concept nonparametric
#' @concept repeated measures
#'
#' @export
friedman_gen <- function(n,
                         k,
                         medians,
                         labels       = NULL,
                         correlation  = 0.70,
                         spread,
                         shape        = "right_skew",
                         severity     = "moderate",
                         scale_min    = NULL,
                         scale_max    = NULL,
                         dv_name      = "Score",
                         id_name      = "ID",
                         decimals     = 0,
                         seed         = NULL,
                         export_csv   = FALSE,
                         run_analysis = TRUE) {

  if (!is.numeric(n) || n < 5 || n != round(n))
    stop("'n' must be an integer >= 5")
  if (!is.numeric(k) || k < 3 || k != round(k))
    stop("'k' must be an integer >= 3")
  if (length(medians) != k) stop("Length of 'medians' must equal k")
  if (is.null(labels)) labels <- paste0("Cond", seq_len(k))
  if (length(labels) != k) stop("Length of 'labels' must equal k")
  if (anyDuplicated(labels)) stop("'labels' must be unique")
  if (!is.numeric(correlation) || correlation <= -1 || correlation >= 1)
    stop("'correlation' must be strictly between -1 and 1")
  npar_validate_common(spread, shape, severity, scale_min, scale_max,
                       decimals)
  for (j in seq_len(k))
    npar_validate_median(medians[j], n, decimals, scale_min, scale_max)

  if (!is.null(seed)) set.seed(seed)
  draw_fun <- dist_shape_params(shape, severity)$draw

  cs <- npar_correlated_sample(n, medians, spread, correlation,
                               draw_fun, decimals, scale_min, scale_max)
  m <- cs$m
  colnames(m) <- labels

  data_out <- data.frame(id = seq_len(n), m, stringsAsFactors = FALSE,
                         check.names = FALSE)
  colnames(data_out)[1] <- id_name

  data_long <- data.frame(
    id   = rep(seq_len(n), times = k),
    cond = factor(rep(labels, each = n), levels = labels),
    y    = as.vector(m),
    stringsAsFactors = FALSE
  )
  colnames(data_long) <- c(id_name, "Condition", dv_name)

  condition_stats <- data.frame(
    Condition = labels,
    Median    = apply(m, 2, stats::median),
    IQR       = apply(m, 2, stats::IQR),
    stringsAsFactors = FALSE
  )
  rownames(condition_stats) <- NULL

  cor_mat <- stats::cor(m)

  test_results <- NULL
  kendalls_w <- NULL
  posthoc <- NULL

  if (run_analysis) {
    test_results <- stats::friedman.test(m)
    chi2 <- as.numeric(test_results$statistic)
    kendalls_w <- chi2 / (n * (k - 1))
    if (test_results$p.value < 0.05) {
      comps <- utils::combn(k, 2)
      lab <- character(ncol(comps))
      p_raw <- numeric(ncol(comps))
      for (i in seq_len(ncol(comps))) {
        c1 <- comps[1, i]; c2 <- comps[2, i]
        wt <- suppressWarnings(
          stats::wilcox.test(m[, c2], m[, c1], paired = TRUE,
                             correct = TRUE))
        p_raw[i] <- wt$p.value
        lab[i]   <- paste(labels[c1], "vs", labels[c2])
      }
      posthoc <- data.frame(Comparison = lab,
                            p_raw      = p_raw,
                            p_holm     = stats::p.adjust(p_raw,
                                                         method = "holm"),
                            stringsAsFactors = FALSE)
    }
  }

  dgen_export(data_out, export_csv,
              paste0(dv_name, "_friedman_dgen.csv"))

  message(sprintf("Friedman data: %d subjects x %d conditions", n, k))
  message(paste(utils::capture.output(
    print(condition_stats, row.names = FALSE, digits = 4)),
    collapse = "\n"))
  if (run_analysis) {
    message(sprintf("friedman.test: chi2(%d) = %.3f, p = %s | Kendall's W = %.3f",
                    as.integer(test_results$parameter),
                    as.numeric(test_results$statistic),
                    format.pval(test_results$p.value, digits = 3,
                                eps = 0.001),
                    kendalls_w))
    if (!is.null(posthoc))
      message("Pairwise signed-rank post-hoc (Holm) computed; see $posthoc")
  }

  result <- list(
    data               = data_out,
    data_long          = data_long,
    condition_stats    = condition_stats,
    correlation_matrix = cor_mat,
    iterations         = cs$iterations
  )
  if (run_analysis) {
    result$test_results <- test_results
    result$kendalls_w   <- kendalls_w
    result$posthoc      <- posthoc
  }
  invisible(result)
}
