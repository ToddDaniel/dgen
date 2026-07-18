#' Generate a Single Variable with Exact Mean and SD
#'
#' Creates a numeric vector with an exact specified mean and standard deviation
#' using iterative refinement. Unlike simple sampling, this function iterates
#' until the rounded data matches the target parameters precisely.
#'
#' @param n Sample size (positive integer)
#' @param m Target mean
#' @param sd Target standard deviation (non-negative)
#' @param decimals Number of decimal places for rounding (default = 2)
#' @param neg_data Logical; allow negative values? (default = TRUE). Set to
#'   \code{FALSE} for variables like age, income, or counts.
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Logical; export data to a CSV file? (default = FALSE)
#' @param var_name Name of the generated variable (default = "Variable_X")
#'
#' @return A list with:
#'   \item{data}{Data frame with the generated variable}
#'   \item{targets}{Data frame of target parameters}
#'   \item{achieved}{Data frame of achieved parameters}
#'   \item{iterations}{Number of iterations required}
#'
#' @examples
#' result <- stat_gen(n = 100, m = 5, sd = 1.2)
#' result <- stat_gen(n = 250, m = 2.8, sd = 0.9, var_name = "FP", seed = 42)
#' result <- stat_gen(n = 100, m = 25, sd = 5, neg_data = FALSE, var_name = "Age")
#'
#' @export
# ---
# Adapted from MorseGen, originally written by Brendan Morse (1982-2018).
# ---
stat_gen <- function(n,
                     m,
                     sd,
                     decimals = 2,
                     neg_data = TRUE,
                     seed = NULL,
                     export_csv = FALSE,
                     var_name = "Variable_X") {

  # Input validation
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != round(n))
    stop("'n' must be a positive integer")
  if (!is.numeric(m) || length(m) != 1)
    stop("'m' must be a single numeric value")
  if (!is.numeric(sd) || length(sd) != 1 || sd < 0)
    stop("'sd' must be a non-negative numeric value")
  if (!is.numeric(decimals) || decimals < 0 || decimals != round(decimals))
    stop("'decimals' must be a non-negative integer")
  if (nchar(var_name) == 0)
    stop("'var_name' cannot be empty")

  if (!is.null(seed)) set.seed(seed)

  target_m  <- round(m,  decimals + 1)
  target_sd <- round(sd, decimals + 1)
  dif <- target_m - target_sd
  max_iterations <- 100000
  iteration <- 0

  repeat {
    iteration <- iteration + 1

    if (!neg_data && dif <= 0.5) {
      raw_data <- stats::rexp(n)
    } else {
      raw_data <- stats::rnorm(n, mean = m, sd = sd)
    }

    z_scores  <- scale(raw_data)
    rescaled  <- z_scores[, 1] * target_sd + target_m
    if (!neg_data) rescaled <- abs(rescaled)
    final_data <- round(rescaled, decimals)

    if (round(mean(final_data), decimals + 1) == target_m &&
        round(stats::sd(final_data), decimals + 1) == target_sd) break

    if (iteration >= max_iterations) {
      warning("Maximum iterations reached. Results may not exactly match targets.")
      break
    }
  }

  data_out <- data.frame(final_data, stringsAsFactors = FALSE)
  colnames(data_out) <- var_name

  targets <- data.frame(
    Variable   = var_name,
    Target_Mean = m,
    Target_SD   = sd,
    Neg_Allowed = neg_data,
    stringsAsFactors = FALSE
  )

  achieved <- data.frame(
    Variable      = var_name,
    Achieved_Mean = round(mean(data_out[[var_name]]), decimals + 2),
    Achieved_SD   = round(stats::sd(data_out[[var_name]]), decimals + 2),
    Min           = round(min(data_out[[var_name]]), decimals),
    Max           = round(max(data_out[[var_name]]), decimals),
    stringsAsFactors = FALSE
  )

  if (export_csv) {
    filename <- paste0(var_name, "_dgen.csv")
    utils::write.csv(data_out, file = filename, row.names = FALSE)
    message("Data saved to: ", file.path(getwd(), filename))
  }

  cat("Variable generated in", iteration, "iterations.\n")
  cat("Target  : Mean =", m,  "| SD =", sd, "\n")
  cat("Achieved: Mean =", achieved$Achieved_Mean, "| SD =", achieved$Achieved_SD, "\n")

  invisible(list(
    data       = data_out,
    targets    = targets,
    achieved   = achieved,
    iterations = iteration
  ))
}
