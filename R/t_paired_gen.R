#' Generate Data for a Paired Samples T-Test
#'
#' Creates data for a paired samples t-test with two correlated measurements
#' on the same participants. Uses \code{MASS::mvrnorm()} with iterative
#' refinement to achieve exact target means, standard deviations, and
#' the specified within-person correlation.
#'
#' @param n Number of participants (pairs)
#' @param measure1_mean Mean for Measurement 1
#' @param measure1_sd Standard deviation for Measurement 1
#' @param measure1_label Label for Measurement 1 (default = \code{"Measure1"})
#' @param measure2_mean Mean for Measurement 2
#' @param measure2_sd Standard deviation for Measurement 2
#' @param measure2_label Label for Measurement 2 (default = \code{"Measure2"})
#' @param correlation Target within-person correlation (default = 0.70)
#' @param var_name Name for the measured variable (default = \code{"Score"})
#' @param measure_name Name for the measurement grouping variable in long
#'   format (default = \code{"Measurement"})
#' @param id_name Name for the participant ID variable (default = \code{"ID"})
#' @param output_format One of \code{"wide"}, \code{"long"}, or \code{"both"}
#'   (default = \code{"wide"})
#' @param decimals Number of decimal places for rounding (default = 2)
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Logical; export data to CSV? (default = FALSE)
#' @param run_analysis Logical; run the paired t-test automatically?
#'   (default = TRUE)
#'
#' @return A list with:
#'   \item{data}{Primary data frame (wide or long, per \code{output_format})}
#'   \item{data_wide}{Wide-format data frame (one row per participant)}
#'   \item{data_long}{Long-format data frame (one row per measurement)}
#'   \item{measure1_stats}{Descriptive statistics for Measurement 1}
#'   \item{measure2_stats}{Descriptive statistics for Measurement 2}
#'   \item{difference_stats}{Statistics for the difference scores}
#'   \item{correlation_achieved}{Achieved within-person correlation}
#'   \item{cohens_d_paired}{Paired Cohen's d}
#'   \item{cohens_d_corrected}{Correlation-corrected Cohen's d}
#'   \item{iterations}{Number of iterations required}
#'   \item{t_test_results}{Paired t-test result (if \code{run_analysis = TRUE})}
#'
#' @examples
#' result <- t_paired_gen(
#'   n = 30,
#'   measure1_mean = 50, measure1_sd = 10, measure1_label = "Pre",
#'   measure2_mean = 55, measure2_sd = 10, measure2_label = "Post",
#'   correlation = 0.70, var_name = "Test_Score"
#' )
#' result$t_test_results
#'
#' @export
t_paired_gen <- function(n,
                          measure1_mean,
                          measure1_sd,
                          measure1_label = "Measure1",
                          measure2_mean,
                          measure2_sd,
                          measure2_label = "Measure2",
                          correlation    = 0.70,
                          var_name       = "Score",
                          measure_name   = "Measurement",
                          id_name        = "ID",
                          output_format  = "wide",
                          decimals       = 2,
                          seed           = NULL,
                          export_csv     = FALSE,
                          run_analysis   = TRUE) {

  # Validation
  if (!is.numeric(n) || n <= 0 || n != round(n)) stop("'n' must be a positive integer")
  if (!is.numeric(measure1_sd) || measure1_sd < 0) stop("'measure1_sd' must be non-negative")
  if (!is.numeric(measure2_sd) || measure2_sd < 0) stop("'measure2_sd' must be non-negative")
  if (!is.numeric(correlation) || correlation < -1 || correlation > 1)
    stop("'correlation' must be between -1 and 1")
  if (!is.numeric(decimals) || decimals < 0 || decimals != round(decimals))
    stop("'decimals' must be a non-negative integer")
  if (!output_format %in% c("wide", "long", "both"))
    stop("'output_format' must be 'wide', 'long', or 'both'")
  if (nchar(measure1_label) == 0 || nchar(measure2_label) == 0)
    stop("Measure labels cannot be empty")
  if (measure1_label == measure2_label) warning("Measure labels are identical")

  if (!is.null(seed)) set.seed(seed)

  target_mean1 <- round(measure1_mean, decimals + 1)
  target_sd1   <- round(measure1_sd,   decimals + 1)
  target_mean2 <- round(measure2_mean, decimals + 1)
  target_sd2   <- round(measure2_sd,   decimals + 1)
  target_cor   <- round(correlation, 3)
  max_iter     <- 100000
  iteration    <- 0

  repeat {
    iteration <- iteration + 1

    cov12 <- correlation * measure1_sd * measure2_sd
    Sigma  <- matrix(c(measure1_sd^2, cov12, cov12, measure2_sd^2), nrow = 2)
    raw    <- MASS::mvrnorm(n = n,
                             mu    = c(measure1_mean, measure2_mean),
                             Sigma = Sigma)

    m1d <- round(scale(raw[, 1])[, 1] * target_sd1 + target_mean1, decimals)
    m2d <- round(scale(raw[, 2])[, 1] * target_sd2 + target_mean2, decimals)

    ok <- round(mean(m1d), decimals + 1) == target_mean1 &&
          round(stats::sd(m1d), decimals + 1) == target_sd1 &&
          round(mean(m2d), decimals + 1) == target_mean2 &&
          round(stats::sd(m2d), decimals + 1) == target_sd2 &&
          round(stats::cor(m1d, m2d), 3) == target_cor
    if (ok) break
    if (iteration >= max_iter) {
      warning("Maximum iterations reached. Results may not exactly match targets.")
      break
    }
  }

  # Wide format
  data_wide <- data.frame(
    ID = seq_len(n), M1 = m1d, M2 = m2d,
    stringsAsFactors = FALSE
  )
  colnames(data_wide) <- c(id_name, measure1_label, measure2_label)

  # Long format
  data_long <- data.frame(
    ID          = rep(seq_len(n), times = 2),
    Measurement = rep(c(measure1_label, measure2_label), each = n),
    Score       = c(m1d, m2d),
    stringsAsFactors = FALSE
  )
  colnames(data_long) <- c(id_name, measure_name, var_name)

  # Statistics
  diff_scores <- m2d - m1d
  mean_diff   <- mean(diff_scores)
  sd_diff     <- stats::sd(diff_scores)
  cor_ach     <- stats::cor(m1d, m2d)
  d_paired    <- mean_diff / sd_diff
  d_corrected <- d_paired / sqrt(2 * (1 - cor_ach))

  measure1_stats <- data.frame(Measure = measure1_label, N = n,
                                Target_Mean = measure1_mean,
                                Achieved_Mean = round(mean(m1d), decimals + 2),
                                Target_SD = measure1_sd,
                                Achieved_SD = round(stats::sd(m1d), decimals + 2))
  measure2_stats <- data.frame(Measure = measure2_label, N = n,
                                Target_Mean = measure2_mean,
                                Achieved_Mean = round(mean(m2d), decimals + 2),
                                Target_SD = measure2_sd,
                                Achieved_SD = round(stats::sd(m2d), decimals + 2))
  difference_stats <- data.frame(
    Mean_Difference = round(mean_diff, decimals + 2),
    SD_Difference   = round(sd_diff,   decimals + 2),
    Min_Difference  = round(min(diff_scores), decimals),
    Max_Difference  = round(max(diff_scores), decimals)
  )

  t_test_results <- if (run_analysis) {
    stats::t.test(data_wide[[measure1_label]],
                  data_wide[[measure2_label]],
                  paired = TRUE)
  } else NULL

  if (export_csv) {
    if (output_format %in% c("wide", "both")) {
      fn <- paste0(var_name, "_paired_wide_dgen.csv")
      utils::write.csv(data_wide, file = fn, row.names = FALSE)
      message("Wide data saved to: ", file.path(getwd(), fn))
    }
    if (output_format %in% c("long", "both")) {
      fn <- paste0(var_name, "_paired_long_dgen.csv")
      utils::write.csv(data_long, file = fn, row.names = FALSE)
      message("Long data saved to: ", file.path(getwd(), fn))
    }
  }

  cat("Paired t-test data generated in", iteration, "iterations.\n")
  cat(sprintf("Correlation: target = %.3f, achieved = %.3f\n", correlation, cor_ach))
  cat(sprintf("Mean difference: %.3f | Cohen's d (paired): %.3f\n", mean_diff, d_paired))

  if (run_analysis && !is.null(t_test_results)) {
    p <- t_test_results$p.value
    cat(sprintf("t(%d) = %.3f, p = %s\n",
                as.integer(t_test_results$parameter),
                as.numeric(t_test_results$statistic),
                format.pval(p, digits = 3, eps = 0.001)))
  }

  result <- list(
    measure1_stats       = measure1_stats,
    measure2_stats       = measure2_stats,
    difference_stats     = difference_stats,
    correlation_achieved = cor_ach,
    cohens_d_paired      = d_paired,
    cohens_d_corrected   = d_corrected,
    iterations           = iteration
  )

  if (output_format == "wide") {
    result$data      <- data_wide
    result$data_wide <- data_wide
  } else if (output_format == "long") {
    result$data      <- data_long
    result$data_long <- data_long
  } else {
    result$data_wide <- data_wide
    result$data_long <- data_long
  }

  if (run_analysis) result$t_test_results <- t_test_results

  invisible(result)
}
