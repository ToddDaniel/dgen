#' Generate Data for a One-Sample T-Test
#'
#' Creates a numeric variable with an exact specified mean and standard
#' deviation, then optionally runs a one-sample t-test comparing the sample
#' mean to a hypothesized value. The iterative refinement engine ensures the
#' rounded data matches the target parameters exactly.
#'
#' @param n Sample size (positive integer)
#' @param m Target mean
#' @param sd Target standard deviation (non-negative)
#' @param test_value Hypothesized population mean to test against (default = 0)
#' @param var_name Name of the generated variable (default = \code{"Score"})
#' @param decimals Number of decimal places for rounding (default = 2)
#' @param neg_data Logical; allow negative values? (default = TRUE)
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Logical; export data to a CSV file? (default = FALSE)
#' @param run_analysis Logical; run one-sample t-test automatically?
#'   (default = TRUE)
#'
#' @return A list with:
#'   \item{data}{Data frame with the generated variable}
#'   \item{stats}{Summary statistics (target vs. achieved)}
#'   \item{cohens_d}{Cohen's d effect size}
#'   \item{iterations}{Number of iterations required}
#'   \item{t_test_results}{One-sample t-test result (if \code{run_analysis = TRUE})}
#'
#' @examples
#' result <- t_onesample_gen(n = 100, m = 50, sd = 10, test_value = 45)
#' result <- t_onesample_gen(n = 200, m = 0, sd = 1, test_value = 0, seed = 42)
#'
#' @export
t_onesample_gen <- function(n,
                             m,
                             sd,
                             test_value = 0,
                             var_name   = "Score",
                             decimals   = 2,
                             neg_data   = TRUE,
                             seed       = NULL,
                             export_csv = FALSE,
                             run_analysis = TRUE) {

  # Validation
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != round(n))
    stop("'n' must be a positive integer")
  if (!is.numeric(m)  || length(m)  != 1) stop("'m' must be a single numeric value")
  if (!is.numeric(sd) || length(sd) != 1 || sd < 0) stop("'sd' must be non-negative")
  if (!is.numeric(test_value) || length(test_value) != 1)
    stop("'test_value' must be a single numeric value")
  if (!is.numeric(decimals) || decimals < 0 || decimals != round(decimals))
    stop("'decimals' must be a non-negative integer")
  if (nchar(var_name) == 0) stop("'var_name' cannot be empty")

  if (!is.null(seed)) set.seed(seed)

  tolerance      <- 10^(-(decimals + 1))
  max_iterations <- 100000
  iter_count     <- 0
  converged      <- FALSE
  x_final        <- NULL

  while (!converged && iter_count < max_iterations) {
    iter_count <- iter_count + 1

    x            <- stats::rnorm(n, mean = m, sd = sd)
    x_std        <- (x - mean(x)) / stats::sd(x)
    x_rescaled   <- x_std * sd + m
    x_rounded    <- round(x_rescaled, decimals)

    if (abs(mean(x_rounded) - m)  < tolerance &&
        abs(stats::sd(x_rounded) - sd) < tolerance) {
      converged <- TRUE
      x_final   <- x_rounded
    }
  }

  if (!converged) {
    warning("Maximum iterations reached. Results may not be exact.")
    x_final <- x_rounded
  }

  if (!neg_data && any(x_final < 0)) x_final <- abs(x_final)

  data_out <- data.frame(x_final, stringsAsFactors = FALSE)
  names(data_out) <- var_name

  achieved_mean <- mean(data_out[[var_name]])
  achieved_sd   <- stats::sd(data_out[[var_name]])
  cohens_d      <- (achieved_mean - test_value) / achieved_sd

  t_test_results <- if (run_analysis) {
    stats::t.test(data_out[[var_name]], mu = test_value)
  } else NULL

  if (export_csv) {
    filename <- paste0(var_name, "_dgen.csv")
    utils::write.csv(data_out, file = filename, row.names = FALSE)
    message("Data saved to: ", file.path(getwd(), filename))
  }

  cat("One-sample t-test data generated in", iter_count, "iterations.\n")
  cat("Target  : Mean =", m, "| SD =", sd, "\n")
  cat("Achieved: Mean =", round(achieved_mean, decimals + 2),
      "| SD =", round(achieved_sd, decimals + 2), "\n")
  cat("Cohen's d:", round(cohens_d, 3), "\n")

  if (run_analysis && !is.null(t_test_results)) {
    p <- t_test_results$p.value
    cat(sprintf("t(%d) = %.3f, p = %s\n",
                as.integer(t_test_results$parameter),
                as.numeric(t_test_results$statistic),
                format.pval(p, digits = 3, eps = 0.001)))
    cat(sprintf("Interpretation (two-tailed): %s at alpha = .05\n",
                ifelse(p < .05, "SIGNIFICANT", "not significant")))
  }

  output <- list(
    data       = data_out,
    stats      = data.frame(N = n,
                            Target_Mean   = m,
                            Achieved_Mean = achieved_mean,
                            Target_SD     = sd,
                            Achieved_SD   = achieved_sd,
                            Test_Value    = test_value,
                            Difference    = achieved_mean - test_value),
    cohens_d   = cohens_d,
    iterations = iter_count
  )
  if (run_analysis) output$t_test_results <- t_test_results

  invisible(output)
}
