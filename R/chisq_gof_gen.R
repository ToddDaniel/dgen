#' Generate Data for a Chi-Square Goodness-of-Fit Test
#'
#' Creates a categorical variable and optionally runs a chi-square
#' goodness-of-fit test comparing observed frequencies to expected frequencies.
#' Expected frequencies can be equal across categories, specified as
#' proportions, or specified as raw counts.
#'
#' @param n Total sample size (positive integer)
#' @param categories Character vector of category names (length >= 2)
#' @param expected Type of expected frequencies. One of:
#'   \code{"equal"} (default), \code{"proportions"}, or \code{"counts"}.
#' @param expected_values Numeric vector of expected proportions or counts.
#'   Required when \code{expected} is \code{"proportions"} or
#'   \code{"counts"}. Must have the same length as \code{categories}.
#' @param var_name Name for the categorical variable (default =
#'   \code{"Category"})
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run chi-square test automatically?
#'   (default = TRUE)
#'
#' @return A list with:
#'   \item{data}{Data frame with the categorical variable}
#'   \item{contingency_table}{Observed vs. expected count table}
#'   \item{chisq_test_results}{\code{chisq.test()} result}
#'   \item{cramers_v}{Cramér's V effect size}
#'   \item{assumptions}{Assumption check results}
#'
#' @examples
#' # Equal expected frequencies
#' result <- chisq_gof_gen(n = 120, categories = c("A", "B", "C", "D"))
#'
#' # Population proportions
#' result <- chisq_gof_gen(
#'   n = 200,
#'   categories = c("Type1", "Type2", "Type3"),
#'   expected = "proportions",
#'   expected_values = c(0.50, 0.30, 0.20)
#' )
#'
#' @export
chisq_gof_gen <- function(n,
                            categories,
                            expected        = "equal",
                            expected_values = NULL,
                            var_name        = "Category",
                            seed            = NULL,
                            export_csv      = FALSE,
                            run_analysis    = TRUE) {

  if (!is.numeric(n) || n <= 0 || n != round(n)) stop("'n' must be a positive integer")
  if (length(categories) < 2) stop("Must have at least 2 categories")
  if (any(nchar(categories) == 0)) stop("Category names cannot be empty")
  if (any(duplicated(categories))) stop("Category names must be unique")

  k <- length(categories)

  if (!expected %in% c("equal", "proportions", "counts"))
    stop("'expected' must be 'equal', 'proportions', or 'counts'")
  if (expected == "proportions") {
    if (is.null(expected_values)) stop("'expected_values' required when expected = 'proportions'")
    if (length(expected_values) != k) stop("'expected_values' must have same length as 'categories'")
    if (any(expected_values < 0)) stop("Proportions cannot be negative")
    if (abs(sum(expected_values) - 1.0) > 0.01) {
      warning("Proportions sum to ", round(sum(expected_values), 3), ". Normalizing to 1.0")
      expected_values <- expected_values / sum(expected_values)
    }
  }
  if (expected == "counts") {
    if (is.null(expected_values)) stop("'expected_values' required when expected = 'counts'")
    if (length(expected_values) != k) stop("'expected_values' must have same length as 'categories'")
    if (any(expected_values < 0)) stop("Expected counts cannot be negative")
  }

  if (!is.null(seed)) set.seed(seed)

  expected_counts <- switch(expected,
    "equal"       = stats::setNames(rep(n / k, k), categories),
    "proportions" = stats::setNames(n * expected_values, categories),
    "counts"      = stats::setNames(expected_values, categories)
  )

  probs <- expected_counts / sum(expected_counts)
  noise <- stats::runif(k, min = -0.05, max = 0.05)
  pert  <- pmax(probs + noise, 0.01)
  pert  <- pert / sum(pert)

  observed_counts <- as.vector(stats::rmultinom(n = 1, size = n, prob = pert))
  names(observed_counts) <- categories

  vec      <- rep(categories, times = observed_counts)
  vec      <- sample(vec)
  data_out <- data.frame(
    category = factor(vec, levels = categories),
    stringsAsFactors = FALSE
  )
  colnames(data_out) <- var_name

  contingency_table <- data.frame(
    Category   = categories,
    Observed   = observed_counts,
    Expected   = round(expected_counts, 2),
    Difference = observed_counts - expected_counts,
    stringsAsFactors = FALSE
  )

  chisq_test_results <- NULL
  cramers_v          <- NULL
  assumptions        <- NULL

  if (run_analysis) {
    chisq_test_results <- stats::chisq.test(
      x = observed_counts,
      p = expected_counts / sum(expected_counts)
    )
    cramers_v <- as.numeric(
      sqrt(chisq_test_results$statistic / (n * (k - 1)))
    )
    assumptions <- list(
      adequate_n           = n >= 5 * k,
      expected_freq_gt_5   = all(expected_counts >= 5),
      pct_cells_gt_5       = mean(expected_counts >= 5) * 100,
      min_expected         = min(expected_counts),
      all_adequate         = all(expected_counts >= 5)
    )
  }

  dgen_export(data_out, export_csv,
              paste0(var_name, "_GoodnessOfFit_dgen.csv"))

  message(sprintf("Chi-square GOF data generated (n = %d, k = %d categories)",
                  n, k))
  message(paste(utils::capture.output(
    print(contingency_table, row.names = FALSE)), collapse = "\n"))
  if (run_analysis && !is.null(chisq_test_results)) {
    p_val <- chisq_test_results$p.value
    message(sprintf("chi2(%d) = %.3f, p = %s | Cramer's V = %.3f",
                as.integer(chisq_test_results$parameter),
                as.numeric(chisq_test_results$statistic),
                format.pval(p_val, digits = 3, eps = 0.001),
                cramers_v))
  }

  result <- list(
    data              = data_out,
    contingency_table = contingency_table
  )
  if (run_analysis) {
    result$chisq_test_results <- chisq_test_results
    result$cramers_v          <- cramers_v
    result$assumptions        <- assumptions
  }

  invisible(result)
}
