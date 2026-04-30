#' Generate Data for an Independent Samples T-Test
#'
#' Creates data for an independent samples t-test with two groups having
#' specified means and standard deviations on a continuous outcome variable.
#' Uses iterative refinement to achieve exact target statistics for each group.
#' Both a pooled-variance t-test and Welch's t-test are run automatically.
#'
#' @param n1 Sample size for Group 1
#' @param mean1 Mean for Group 1
#' @param sd1 Standard deviation for Group 1
#' @param label1 Label for Group 1 (default = \code{"Group1"})
#' @param n2 Sample size for Group 2
#' @param mean2 Mean for Group 2
#' @param sd2 Standard deviation for Group 2
#' @param label2 Label for Group 2 (default = \code{"Group2"})
#' @param var_name Name for the continuous outcome variable (default =
#'   \code{"Score"})
#' @param group_name Name for the grouping variable (default = \code{"Group"})
#' @param decimals Number of decimal places for rounding (default = 2)
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Logical; export data to a CSV file? (default = FALSE)
#' @param run_analysis Logical; run t-tests automatically? (default = TRUE)
#'
#' @return A list with:
#'   \item{data}{Data frame with the outcome and group variables}
#'   \item{group1_stats}{Descriptive statistics for Group 1}
#'   \item{group2_stats}{Descriptive statistics for Group 2}
#'   \item{cohens_d}{Cohen's d effect size}
#'   \item{pooled_sd}{Pooled standard deviation}
#'   \item{iterations}{Iteration counts for each group}
#'   \item{t_test_pooled}{Pooled-variance t-test (if \code{run_analysis = TRUE})}
#'   \item{t_test_welch}{Welch's t-test (if \code{run_analysis = TRUE})}
#'
#' @examples
#' result <- t_indep_gen(
#'   n1 = 30, mean1 = 50, sd1 = 10, label1 = "Control",
#'   n2 = 30, mean2 = 55, sd2 = 10, label2 = "Treatment",
#'   var_name = "Test_Score"
#' )
#' result$t_test_pooled
#' result$t_test_welch
#'
#' @export
t_indep_gen <- function(n1,
                         mean1,
                         sd1,
                         label1     = "Group1",
                         n2,
                         mean2,
                         sd2,
                         label2     = "Group2",
                         var_name   = "Score",
                         group_name = "Group",
                         decimals   = 2,
                         seed       = NULL,
                         export_csv = FALSE,
                         run_analysis = TRUE) {

  # Validation
  if (!is.numeric(n1) || n1 <= 0 || n1 != round(n1)) stop("'n1' must be a positive integer")
  if (!is.numeric(n2) || n2 <= 0 || n2 != round(n2)) stop("'n2' must be a positive integer")
  if (!is.numeric(sd1) || sd1 < 0) stop("'sd1' must be non-negative")
  if (!is.numeric(sd2) || sd2 < 0) stop("'sd2' must be non-negative")
  if (!is.numeric(decimals) || decimals < 0 || decimals != round(decimals))
    stop("'decimals' must be a non-negative integer")
  if (nchar(label1) == 0 || nchar(label2) == 0) stop("Group labels cannot be empty")
  if (label1 == label2) warning("Group labels are identical")

  if (!is.null(seed)) set.seed(seed)

  max_iter <- 100000

  # Generate Group 1 with exact statistics
  target_mean1 <- round(mean1, decimals + 1)
  target_sd1   <- round(sd1,   decimals + 1)
  iteration1   <- 0
  repeat {
    iteration1 <- iteration1 + 1
    z1         <- scale(stats::rnorm(n1, mean = mean1, sd = sd1))
    g1         <- round(z1[, 1] * target_sd1 + target_mean1, decimals)
    if (round(mean(g1), decimals + 1) == target_mean1 &&
        round(stats::sd(g1), decimals + 1) == target_sd1) break
    if (iteration1 >= max_iter) { warning("Max iterations reached for Group 1."); break }
  }

  # Generate Group 2 with exact statistics
  target_mean2 <- round(mean2, decimals + 1)
  target_sd2   <- round(sd2,   decimals + 1)
  iteration2   <- 0
  repeat {
    iteration2 <- iteration2 + 1
    z2         <- scale(stats::rnorm(n2, mean = mean2, sd = sd2))
    g2         <- round(z2[, 1] * target_sd2 + target_mean2, decimals)
    if (round(mean(g2), decimals + 1) == target_mean2 &&
        round(stats::sd(g2), decimals + 1) == target_sd2) break
    if (iteration2 >= max_iter) { warning("Max iterations reached for Group 2."); break }
  }

  group_labels <- c(label1, label2)
  data_out <- data.frame(
    continuous = c(g1, g2),
    group      = c(rep(label1, n1), rep(label2, n2)),
    stringsAsFactors = FALSE
  )
  colnames(data_out) <- c(var_name, group_name)

  m1_ach  <- mean(g1);         m2_ach  <- mean(g2)
  sd1_ach <- stats::sd(g1);    sd2_ach <- stats::sd(g2)
  pooled_sd <- sqrt(((n1 - 1) * sd1_ach^2 + (n2 - 1) * sd2_ach^2) / (n1 + n2 - 2))
  cohens_d  <- (m2_ach - m1_ach) / pooled_sd

  group1_stats <- data.frame(Group = label1, N = n1,
                              Target_Mean = mean1, Achieved_Mean = round(m1_ach, decimals + 2),
                              Target_SD   = sd1,   Achieved_SD   = round(sd1_ach, decimals + 2))
  group2_stats <- data.frame(Group = label2, N = n2,
                              Target_Mean = mean2, Achieved_Mean = round(m2_ach, decimals + 2),
                              Target_SD   = sd2,   Achieved_SD   = round(sd2_ach, decimals + 2))

  t_test_pooled <- NULL
  t_test_welch  <- NULL
  if (run_analysis) {
    fml           <- stats::as.formula(paste(var_name, "~", group_name))
    t_test_pooled <- stats::t.test(fml, data = data_out, var.equal = TRUE)
    t_test_welch  <- stats::t.test(fml, data = data_out, var.equal = FALSE)
  }

  if (export_csv) {
    filename <- paste0(var_name, "_by_", group_name, "_dgen.csv")
    utils::write.csv(data_out, file = filename, row.names = FALSE)
    message("Data saved to: ", file.path(getwd(), filename))
  }

  cat("Independent t-test data generated.\n")
  cat(sprintf("Group 1 (%s): n=%d, M=%.2f, SD=%.2f\n", label1, n1, m1_ach, sd1_ach))
  cat(sprintf("Group 2 (%s): n=%d, M=%.2f, SD=%.2f\n", label2, n2, m2_ach, sd2_ach))
  cat(sprintf("Cohen's d: %.3f | Pooled SD: %.3f\n", cohens_d, pooled_sd))

  if (run_analysis) {
    sd_ratio <- sd2_ach / sd1_ach
    use_welch <- !(sd_ratio >= 0.5 && sd_ratio <= 2.0)
    test_used <- if (use_welch) t_test_welch else t_test_pooled
    p <- test_used$p.value
    cat(sprintf("Recommended test: %s | p = %s\n",
                ifelse(use_welch, "Welch's", "pooled"),
                format.pval(p, digits = 3, eps = 0.001)))
  }

  result <- list(
    data         = data_out,
    group1_stats = group1_stats,
    group2_stats = group2_stats,
    cohens_d     = cohens_d,
    pooled_sd    = pooled_sd,
    iterations   = c(Group1 = iteration1, Group2 = iteration2)
  )
  if (run_analysis) {
    result$t_test_pooled <- t_test_pooled
    result$t_test_welch  <- t_test_welch
  }

  invisible(result)
}
