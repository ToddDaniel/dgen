#' Generate Data for a Two-Sample Proportion Test
#'
#' Creates binary outcome data for two independent groups with exact specified
#' proportions, then optionally runs a two-sample proportion test and the
#' equivalent chi-square test of independence. Also calculates Cohen's h, the
#' odds ratio, and the risk ratio.
#'
#' @param n1 Sample size for Group 1
#' @param p1 Proportion of successes in Group 1 (0 < p1 < 1)
#' @param label1 Label for Group 1 (default = \code{"Group1"})
#' @param n2 Sample size for Group 2
#' @param p2 Proportion of successes in Group 2 (0 < p2 < 1)
#' @param label2 Label for Group 2 (default = \code{"Group2"})
#' @param var_name Name for the binary outcome variable (default =
#'   \code{"Success"})
#' @param group_name Name for the grouping variable (default = \code{"Group"})
#' @param category_labels Character vector of length 2 for the outcome
#'   categories (default = \code{c("Failure", "Success")})
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Logical; export data to CSV? (default = FALSE)
#' @param run_analysis Logical; run proportion and chi-square tests
#'   automatically? (default = TRUE)
#'
#' @return A list with:
#'   \item{data}{Long-format data frame}
#'   \item{contingency_table}{2×2 contingency table}
#'   \item{group1_stats}{Proportion and counts for Group 1}
#'   \item{group2_stats}{Proportion and counts for Group 2}
#'   \item{difference}{Difference in proportions (Group 1 - Group 2)}
#'   \item{pooled_proportion}{Pooled proportion under H₀}
#'   \item{cohens_h}{Cohen's h effect size}
#'   \item{odds_ratio}{Odds ratio (Group 1 / Group 2)}
#'   \item{risk_ratio}{Risk ratio (Group 1 / Group 2)}
#'   \item{prop_test_results}{\code{prop.test()} result (if
#'     \code{run_analysis = TRUE})}
#'   \item{chisq_test_results}{\code{chisq.test()} result (if
#'     \code{run_analysis = TRUE})}
#'   \item{assumptions}{Assumption checks (if \code{run_analysis = TRUE})}
#'
#' @examples
#' result <- twoprop_gen(
#'   n1 = 100, p1 = 0.60, label1 = "Treatment",
#'   n2 = 100, p2 = 0.40, label2 = "Control"
#' )
#' result$prop_test_results
#'
#' @export
twoprop_gen <- function(n1,
                         p1,
                         label1          = "Group1",
                         n2,
                         p2,
                         label2          = "Group2",
                         var_name        = "Success",
                         group_name      = "Group",
                         category_labels = c("Failure", "Success"),
                         seed            = NULL,
                         export_csv      = FALSE,
                         run_analysis    = TRUE) {

  if (!is.numeric(n1) || n1 <= 0 || n1 != round(n1)) stop("'n1' must be a positive integer")
  if (!is.numeric(n2) || n2 <= 0 || n2 != round(n2)) stop("'n2' must be a positive integer")
  if (!is.numeric(p1) || p1 <= 0 || p1 >= 1) stop("'p1' must be between 0 and 1 (exclusive)")
  if (!is.numeric(p2) || p2 <= 0 || p2 >= 1) stop("'p2' must be between 0 and 1 (exclusive)")
  if (nchar(label1) == 0 || nchar(label2) == 0) stop("Group labels cannot be empty")
  if (label1 == label2) warning("Group labels are identical")
  if (length(category_labels) != 2) stop("'category_labels' must have exactly 2 elements")

  if (!is.null(seed)) set.seed(seed)

  group_labels <- c(label1, label2)

  successes1 <- round(n1 * p1);  failures1 <- n1 - successes1
  successes2 <- round(n2 * p2);  failures2 <- n2 - successes2
  g1_data    <- sample(c(rep(1, successes1), rep(0, failures1)))
  g2_data    <- sample(c(rep(1, successes2), rep(0, failures2)))

  data_out <- data.frame(
    group   = factor(c(rep(label1, n1), rep(label2, n2)), levels = group_labels),
    outcome = factor(c(g1_data, g2_data), levels = c(0, 1), labels = category_labels),
    stringsAsFactors = FALSE
  )
  colnames(data_out) <- c(group_name, var_name)

  p1_ach   <- successes1 / n1
  p2_ach   <- successes2 / n2
  p_pooled <- (successes1 + successes2) / (n1 + n2)
  diff_val <- p1_ach - p2_ach
  cohens_h <- 2 * (asin(sqrt(p1_ach)) - asin(sqrt(p2_ach)))
  odds1    <- p1_ach / (1 - p1_ach)
  odds2    <- p2_ach / (1 - p2_ach)
  odds_ratio <- odds1 / odds2
  risk_ratio <- p1_ach / p2_ach
  ctab      <- table(data_out[[group_name]], data_out[[var_name]])

  group1_stats <- list(group = label1, n = n1,
                        successes = successes1, failures = failures1,
                        proportion = p1_ach)
  group2_stats <- list(group = label2, n = n2,
                        successes = successes2, failures = failures2,
                        proportion = p2_ach)

  prop_test_results  <- NULL
  chisq_test_results <- NULL
  assumptions        <- NULL

  if (run_analysis) {
    prop_test_results  <- stats::prop.test(x = c(successes1, successes2),
                                            n = c(n1, n2), correct = TRUE)
    chisq_test_results <- stats::chisq.test(ctab, correct = TRUE)
    assumptions <- list(
      n1_adequate         = n1 >= 30,
      n2_adequate         = n2 >= 30,
      expected_g1_success = n1 * p_pooled >= 5,
      expected_g1_failure = n1 * (1 - p_pooled) >= 5,
      expected_g2_success = n2 * p_pooled >= 5,
      expected_g2_failure = n2 * (1 - p_pooled) >= 5
    )
    assumptions$all_adequate <- all(unlist(assumptions))
  }

  if (export_csv) {
    fn <- paste0(var_name, "_by_", group_name, "_dgen.csv")
    utils::write.csv(data_out, file = fn, row.names = FALSE)
    message("Data saved to: ", file.path(getwd(), fn))
  }

  cat(sprintf("Two-sample proportions: %s = %.3f | %s = %.3f\n",
              label1, p1_ach, label2, p2_ach))
  cat(sprintf("Difference = %.3f | Cohen's h = %.3f | OR = %.3f\n",
              diff_val, cohens_h, odds_ratio))
  if (run_analysis) {
    p_val <- prop_test_results$p.value
    cat(sprintf("prop.test: chi2 = %.3f, p = %s\n",
                as.numeric(prop_test_results$statistic),
                format.pval(p_val, digits = 3, eps = 0.001)))
  }

  result <- list(
    data              = data_out,
    contingency_table = ctab,
    group1_stats      = group1_stats,
    group2_stats      = group2_stats,
    difference        = diff_val,
    pooled_proportion = p_pooled,
    cohens_h          = cohens_h,
    odds_ratio        = odds_ratio,
    risk_ratio        = risk_ratio
  )
  if (run_analysis) {
    result$prop_test_results  <- prop_test_results
    result$chisq_test_results <- chisq_test_results
    result$assumptions        <- assumptions
  }

  invisible(result)
}
