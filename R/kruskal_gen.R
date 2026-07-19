#' Generate Data for a Kruskal-Wallis H Test
#'
#' Creates k independent groups with EXACT target medians and a
#' deliberately non-normal shape, then runs R's canonical
#' \code{kruskal.test()} — the nonparametric counterpart of
#' [anova_gen()]. When the omnibus test is significant, Dunn's post-hoc
#' pairwise comparisons (tie-corrected standard errors, Holm-adjusted
#' p-values) are computed. Group sizes may be unequal. All groups share
#' one shape and spread, matching the similar-shapes assumption for a
#' median interpretation.
#'
#' Effect sizes reported (base R has none built in, so both common
#' conventions are provided): epsilon-squared
#' \code{H * (N + 1) / (N^2 - 1)} and H-based eta-squared
#' \code{(H - k + 1) / (N - k)}, computed from the tie-corrected H that
#' \code{kruskal.test()} returns. Benchmarks 0.01 / 0.06 / 0.14.
#'
#' @param k Number of groups (minimum 3)
#' @param n Sample size per group: a single value (equal n) or a vector
#'   of length \code{k}
#' @param medians Numeric vector of k target medians (hit exactly)
#' @param labels Character vector of group labels (default
#'   \code{"Group1"}, \code{"Group2"}, ...)
#' @param spread SD-like scale shared by all groups (approximate)
#' @param shape Distribution shape, as in [dist_gen()]
#'   (default = \code{"right_skew"})
#' @param severity Shape severity (default = \code{"moderate"})
#' @param scale_min,scale_max Optional bounds; values are clamped and
#'   target medians must lie strictly inside
#' @param dv_name Name for the outcome variable (default =
#'   \code{"Score"})
#' @param factor_name Name for the grouping variable (default =
#'   \code{"Group"})
#' @param decimals Number of decimal places (default = 0)
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run the Kruskal-Wallis test
#'   automatically? (default = \code{TRUE})
#'
#' @return (Invisibly) a list with:
#'   \item{data}{Long-format data frame}
#'   \item{group_stats}{n, exact median, mean rank, and IQR per group}
#'   \item{test_results}{The \code{kruskal.test()} result
#'     (tie-corrected H)}
#'   \item{effect_sizes}{\code{epsilon_squared} and
#'     \code{eta_squared_H}}
#'   \item{posthoc_dunn}{Dunn's pairwise comparisons with Holm
#'     adjustment (only when the omnibus test is significant)}
#'   \item{iterations}{Refinement iterations per group}
#'
#' @seealso [anova_gen()] for the parametric counterpart,
#'   [mannwhitney_gen()] for two groups
#'
#' @examples
#' result <- kruskal_gen(
#'   k = 3, n = 12,
#'   medians = c(60, 66, 78),
#'   labels  = c("Urban", "Suburban", "Resort"),
#'   spread  = 10, dv_name = "Engagement",
#'   scale_min = 1, scale_max = 100, seed = 42
#' )
#' result$test_results
#' result$posthoc_dunn
#'
#' @concept Kruskal-Wallis
#' @concept nonparametric
#' @concept Dunn's test
#'
#' @export
kruskal_gen <- function(k,
                        n,
                        medians,
                        labels      = NULL,
                        spread,
                        shape       = "right_skew",
                        severity    = "moderate",
                        scale_min   = NULL,
                        scale_max   = NULL,
                        dv_name     = "Score",
                        factor_name = "Group",
                        decimals    = 0,
                        seed        = NULL,
                        export_csv  = FALSE,
                        run_analysis = TRUE) {

  if (!is.numeric(k) || k < 3 || k != round(k))
    stop("'k' must be an integer >= 3")
  if (length(n) == 1) n <- rep(n, k)
  if (length(n) != k) stop("Length of 'n' must equal k")
  if (any(n < 3) || any(n != round(n)))
    stop("All group sizes must be integers >= 3")
  if (length(medians) != k) stop("Length of 'medians' must equal k")
  if (is.null(labels)) labels <- paste0("Group", seq_len(k))
  if (length(labels) != k) stop("Length of 'labels' must equal k")
  if (anyDuplicated(labels)) stop("'labels' must be unique")
  npar_validate_common(spread, shape, severity, scale_min, scale_max,
                       decimals)
  for (i in seq_len(k))
    npar_validate_median(medians[i], n[i], decimals, scale_min, scale_max)

  if (!is.null(seed)) set.seed(seed)
  draw_fun <- dist_shape_params(shape, severity)$draw

  samples    <- vector("list", k)
  iterations <- integer(k)
  for (i in seq_len(k)) {
    s <- npar_sample(n[i], medians[i], spread, draw_fun, decimals,
                     scale_min, scale_max)
    samples[[i]]  <- s$x
    iterations[i] <- s$iterations
  }
  names(iterations) <- labels

  data_out <- data.frame(
    g = factor(rep(labels, times = n), levels = labels),
    y = unlist(samples),
    stringsAsFactors = FALSE
  )
  colnames(data_out) <- c(factor_name, dv_name)

  rk <- rank(data_out[[dv_name]])
  group_stats <- data.frame(
    Group     = labels,
    n         = n,
    Median    = vapply(samples, stats::median, numeric(1)),
    Mean_Rank = as.numeric(tapply(rk, data_out[[factor_name]], mean)),
    IQR       = vapply(samples, stats::IQR, numeric(1)),
    stringsAsFactors = FALSE
  )
  colnames(group_stats)[1] <- factor_name

  test_results <- NULL
  effect_sizes <- NULL
  posthoc_dunn <- NULL

  if (run_analysis) {
    test_results <- stats::kruskal.test(data_out[[dv_name]],
                                        data_out[[factor_name]])
    H <- as.numeric(test_results$statistic)
    N <- sum(n)
    effect_sizes <- list(
      epsilon_squared = H * (N + 1) / (N^2 - 1),
      eta_squared_H   = (H - k + 1) / (N - k)
    )
    if (test_results$p.value < 0.05) {
      posthoc_dunn <- npar_dunn(data_out[[dv_name]],
                                data_out[[factor_name]])
    }
  }

  dgen_export(data_out, export_csv,
              paste0(dv_name, "_by_", factor_name, "_kruskal_dgen.csv"))

  message(sprintf("Kruskal-Wallis data: k = %d groups, N = %d", k, sum(n)))
  message(paste(utils::capture.output(
    print(group_stats, row.names = FALSE, digits = 4)), collapse = "\n"))
  if (run_analysis) {
    message(sprintf("kruskal.test: H(%d) = %.3f, p = %s | epsilon^2 = %.3f",
                    as.integer(test_results$parameter),
                    as.numeric(test_results$statistic),
                    format.pval(test_results$p.value, digits = 3,
                                eps = 0.001),
                    effect_sizes$epsilon_squared))
    if (!is.null(posthoc_dunn))
      message("Dunn's post-hoc (Holm-adjusted) computed; see $posthoc_dunn")
  }

  result <- list(
    data        = data_out,
    group_stats = group_stats,
    iterations  = iterations
  )
  if (run_analysis) {
    result$test_results <- test_results
    result$effect_sizes <- effect_sizes
    result$posthoc_dunn <- posthoc_dunn
  }
  invisible(result)
}
