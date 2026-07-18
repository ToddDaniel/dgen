#' Generate Multiple Correlated Variables
#'
#' Creates two or more correlated continuous variables with specified means,
#' standard deviations, and correlation structure. Uses
#' \code{MASS::mvrnorm()} with a Cholesky decomposition to achieve the
#' specified population correlation matrix.
#'
#' @param n Sample size (positive integer)
#' @param m Numeric vector of target means, one per variable
#' @param sd Numeric vector of target standard deviations, one per variable
#' @param r Correlation structure. For two variables, a single numeric value.
#'   For three or more variables, a symmetric correlation matrix with 1s on the
#'   diagonal.
#' @param var_names Character vector of variable names. Defaults to
#'   \code{"Var1"}, \code{"Var2"}, etc.
#' @param decimals Number of decimal places for rounding (default = 2)
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Logical; export data to a CSV file? (default = FALSE)
#' @param run_analysis Logical; run pairwise correlation tests? (default = TRUE)
#'
#' @return A list with:
#'   \item{data}{Data frame with generated variables}
#'   \item{targets}{Data frame of target means and SDs}
#'   \item{achieved}{Data frame of achieved means and SDs}
#'   \item{target_cors}{Target correlation matrix}
#'   \item{achieved_cors}{Achieved correlation matrix}
#'   \item{correlation_tests}{Pairwise test results (if \code{run_analysis = TRUE})}
#'   \item{assumptions}{Shapiro-Wilk normality tests (if \code{run_analysis = TRUE})}
#'
#' @examples
#' # Two correlated variables
#' result <- cor_gen(n = 250, m = c(2.8, 2.6), sd = c(0.9, 0.85),
#'                   r = 0.40, var_names = c("FP", "MD"))
#'
#' # Three variables with a correlation matrix
#' cm <- matrix(c(1.0, 0.4, 0.3,
#'                0.4, 1.0, 0.2,
#'                0.3, 0.2, 1.0), nrow = 3, byrow = TRUE)
#' result <- cor_gen(n = 250, m = c(2.8, 2.6, 3.0), sd = c(0.9, 0.85, 1.0),
#'                   r = cm, var_names = c("FP", "MD", "Age"))
#'
#' @export
# ---
# Adapted from MorseGen, originally written by Brendan Morse (1982-2018).
# ---
cor_gen <- function(n,
                    m,
                    sd,
                    r,
                    var_names = NULL,
                    decimals = 2,
                    seed = NULL,
                    export_csv = FALSE,
                    run_analysis = TRUE) {

  # Input validation
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != round(n))
    stop("'n' must be a positive integer")
  if (length(m) != length(sd))
    stop("'m' and 'sd' must have the same length")
  if (any(sd < 0))
    stop("All values in 'sd' must be non-negative")
  if (!is.numeric(decimals) || decimals < 0 || decimals != round(decimals))
    stop("'decimals' must be a non-negative integer")

  n_vars <- length(m)

  if (is.null(var_names)) {
    var_names <- paste0("Var", seq_len(n_vars))
  }
  if (length(var_names) != n_vars)
    stop("'var_names' must have the same length as 'm'")

  # Build correlation matrix
  if (n_vars == 2 && length(r) == 1) {
    cor_matrix <- matrix(c(1, r, r, 1), nrow = 2, byrow = TRUE)
  } else if (is.matrix(r)) {
    cor_matrix <- r
  } else {
    stop("For 3+ variables, 'r' must be a correlation matrix")
  }

  if (nrow(cor_matrix) != n_vars || ncol(cor_matrix) != n_vars)
    stop("Correlation matrix dimensions must match the number of variables")
  if (!all(abs(diag(cor_matrix) - 1) < 1e-10))
    stop("Correlation matrix diagonal must be all 1s")
  if (!isSymmetric(cor_matrix))
    stop("Correlation matrix must be symmetric")

  eigenvalues <- eigen(cor_matrix, only.values = TRUE)$values
  if (any(eigenvalues <= 1e-10)) {
    stop(paste(
      "The correlation matrix is not positive definite.",
      "The specified correlations may be mathematically impossible.",
      "Try reducing some correlations and check the triangle inequality.",
      sep = "\n"
    ))
  }

  if (!is.null(seed)) set.seed(seed)

  # Generate data
  D          <- diag(sd)
  cov_matrix <- D %*% cor_matrix %*% D
  raw_data   <- MASS::mvrnorm(n = n, mu = m, Sigma = cov_matrix)
  data_out   <- as.data.frame(round(raw_data, decimals), stringsAsFactors = FALSE)
  colnames(data_out) <- var_names

  achieved_means <- colMeans(data_out)
  achieved_sds   <- apply(data_out, 2, stats::sd)
  achieved_cors  <- stats::cor(data_out)

  targets <- data.frame(
    Variable    = var_names,
    Target_Mean = m,
    Target_SD   = sd,
    stringsAsFactors = FALSE
  )

  achieved <- data.frame(
    Variable      = var_names,
    Achieved_Mean = round(achieved_means, decimals + 2),
    Achieved_SD   = round(achieved_sds,   decimals + 2),
    stringsAsFactors = FALSE
  )

  # Optional correlation tests
  correlation_tests <- NULL
  assumptions       <- NULL

  if (run_analysis) {
    n_pairs <- n_vars * (n_vars - 1) / 2
    correlation_tests <- data.frame(
      Pair        = character(n_pairs),
      r_target    = numeric(n_pairs),
      r_achieved  = numeric(n_pairs),
      t_statistic = numeric(n_pairs),
      df          = integer(n_pairs),
      p_value     = numeric(n_pairs),
      CI_lower    = numeric(n_pairs),
      CI_upper    = numeric(n_pairs),
      stringsAsFactors = FALSE
    )

    idx <- 1
    for (i in seq_len(n_vars - 1)) {
      for (j in (i + 1):n_vars) {
        ct <- stats::cor.test(data_out[[var_names[i]]], data_out[[var_names[j]]])
        correlation_tests$Pair[idx]        <- paste(var_names[i], "x", var_names[j])
        correlation_tests$r_target[idx]    <- cor_matrix[i, j]
        correlation_tests$r_achieved[idx]  <- as.numeric(ct$estimate)
        correlation_tests$t_statistic[idx] <- as.numeric(ct$statistic)
        correlation_tests$df[idx]          <- as.integer(ct$parameter)
        correlation_tests$p_value[idx]     <- ct$p.value
        correlation_tests$CI_lower[idx]    <- ct$conf.int[1]
        correlation_tests$CI_upper[idx]    <- ct$conf.int[2]
        idx <- idx + 1
      }
    }

    if (n <= 5000) {
      normality_tests <- data.frame(
        Variable  = var_names,
        Shapiro_W = numeric(n_vars),
        Shapiro_p = numeric(n_vars),
        stringsAsFactors = FALSE
      )
      for (i in seq_len(n_vars)) {
        sw <- stats::shapiro.test(data_out[[var_names[i]]])
        normality_tests$Shapiro_W[i] <- as.numeric(sw$statistic)
        normality_tests$Shapiro_p[i] <- sw$p.value
      }
      assumptions <- list(normality = normality_tests)
    } else {
      assumptions <- list(normality = list(note = "n > 5000: Shapiro-Wilk not computed"))
    }
  }

  if (export_csv) {
    filename <- if (n_vars == 2) {
      paste0(var_names[1], "_", var_names[2], "_dgen.csv")
    } else {
      paste0(var_names[1], "_", var_names[2], "_plus", n_vars - 2, "_dgen.csv")
    }
    utils::write.csv(data_out, file = filename, row.names = FALSE)
    message("Data saved to: ", file.path(getwd(), filename))
  }

  cat("Correlated variables generated (n =", n, ",", n_vars, "variables)\n")
  cat("Target correlation matrix:\n"); print(round(cor_matrix, 3))
  cat("Achieved correlation matrix:\n"); print(round(achieved_cors, 3))

  result <- list(
    data          = data_out,
    targets       = targets,
    achieved      = achieved,
    target_cors   = cor_matrix,
    achieved_cors = achieved_cors
  )
  if (run_analysis) {
    result$correlation_tests <- correlation_tests
    result$assumptions       <- assumptions
  }

  invisible(result)
}
