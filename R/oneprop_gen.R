#' Generate Data for a One-Sample Proportion Test
#'
#' Creates a binary outcome variable with an exact specified proportion, then
#' optionally runs both a normal-approximation proportion test
#' (\code{prop.test}) and an exact binomial test (\code{binom.test}).
#'
#' @param n Sample size (positive integer)
#' @param p Target proportion of successes (0 < p < 1)
#' @param test_value Hypothesized proportion to test against (default = 0.50)
#' @param var_name Name for the outcome variable (default = \code{"Success"})
#' @param category_labels Character vector of length 2 for the outcome
#'   categories: \code{c("Failure label", "Success label")}
#'   (default = \code{c("Failure", "Success")})
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Logical; export data to CSV? (default = FALSE)
#' @param run_analysis Logical; run proportion tests automatically?
#'   (default = TRUE)
#'
#' @return A list with:
#'   \item{data}{Data frame with the categorical outcome variable}
#'   \item{sample_proportion}{Achieved sample proportion}
#'   \item{successes}{Number of successes}
#'   \item{failures}{Number of failures}
#'   \item{test_value}{The hypothesized proportion}
#'   \item{cohens_h}{Cohen's h effect size}
#'   \item{difference}{Difference between sample proportion and test value}
#'   \item{prop_test_results}{\code{prop.test()} result (if
#'     \code{run_analysis = TRUE})}
#'   \item{binom_test_results}{\code{binom.test()} result (if
#'     \code{run_analysis = TRUE})}
#'   \item{assumptions}{Assumption check results (if \code{run_analysis = TRUE})}
#'
#' @examples
#' result <- oneprop_gen(n = 100, p = 0.65, test_value = 0.50)
#' result$prop_test_results
#' result$binom_test_results
#'
#' @export
oneprop_gen <- function(n,
                         p,
                         test_value       = 0.50,
                         var_name         = "Success",
                         category_labels  = c("Failure", "Success"),
                         seed             = NULL,
                         export_csv       = FALSE,
                         run_analysis     = TRUE) {

  if (!is.numeric(n) || n <= 0 || n != round(n)) stop("'n' must be a positive integer")
  if (!is.numeric(p) || p <= 0 || p >= 1) stop("'p' must be between 0 and 1 (exclusive)")
  if (!is.numeric(test_value) || test_value <= 0 || test_value >= 1)
    stop("'test_value' must be between 0 and 1 (exclusive)")
  if (nchar(var_name) == 0) stop("'var_name' cannot be empty")
  if (length(category_labels) != 2) stop("'category_labels' must have exactly 2 elements")
  if (any(nchar(category_labels) == 0)) stop("Category labels cannot be empty")

  if (!is.null(seed)) set.seed(seed)

  successes <- round(n * p)
  failures  <- n - successes
  vec       <- sample(c(rep(1, successes), rep(0, failures)))
  data_out  <- data.frame(
    outcome = factor(vec, levels = c(0, 1), labels = category_labels),
    stringsAsFactors = FALSE
  )
  colnames(data_out) <- var_name

  sample_p <- successes / n
  cohens_h <- 2 * (asin(sqrt(sample_p)) - asin(sqrt(test_value)))
  diff_val <- sample_p - test_value

  prop_test_results  <- NULL
  binom_test_results <- NULL
  assumptions        <- NULL

  if (run_analysis) {
    prop_test_results  <- stats::prop.test(x = successes, n = n, p = test_value,
                                            alternative = "two.sided", correct = TRUE)
    binom_test_results <- stats::binom.test(x = successes, n = n, p = test_value,
                                             alternative = "two.sided")
    assumptions <- list(
      sample_size_adequate         = n >= 30,
      expected_successes           = n * test_value,
      expected_failures            = n * (1 - test_value),
      expected_successes_adequate  = (n * test_value) >= 5,
      expected_failures_adequate   = (n * (1 - test_value)) >= 5,
      normal_approximation_valid   = (n >= 30) &&
                                     (n * test_value >= 5) &&
                                     (n * (1 - test_value) >= 5)
    )
  }

  if (export_csv) {
    fn <- paste0(var_name, "_OneProportion_dgen.csv")
    utils::write.csv(data_out, file = fn, row.names = FALSE)
    message("Data saved to: ", file.path(getwd(), fn))
  }

  cat(sprintf("One-sample proportion: p_hat = %.3f (test value = %.3f)\n",
              sample_p, test_value))
  cat(sprintf("Successes = %d | Failures = %d | Cohen's h = %.3f\n",
              successes, failures, cohens_h))
  if (run_analysis) {
    p_val <- prop_test_results$p.value
    cat(sprintf("prop.test: chi2 = %.3f, p = %s\n",
                as.numeric(prop_test_results$statistic),
                format.pval(p_val, digits = 3, eps = 0.001)))
    cat(sprintf("binom.test: p = %s\n",
                format.pval(binom_test_results$p.value, digits = 3, eps = 0.001)))
  }

  result <- list(
    data              = data_out,
    sample_proportion = sample_p,
    successes         = successes,
    failures          = failures,
    test_value        = test_value,
    cohens_h          = cohens_h,
    difference        = diff_val
  )
  if (run_analysis) {
    result$prop_test_results  <- prop_test_results
    result$binom_test_results <- binom_test_results
    result$assumptions        <- assumptions
  }

  invisible(result)
}
