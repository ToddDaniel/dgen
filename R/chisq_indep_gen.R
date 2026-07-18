#' Generate Data for a Chi-Square Test of Independence
#'
#' Creates a two-variable categorical dataset and optionally runs a chi-square
#' test of independence. The association between variables can be set to none
#' (independence), driven by a target Cramér's V, or specified via custom cell
#' probabilities.
#'
#' @param n Total sample size (positive integer)
#' @param row_var Name of the row variable (default = \code{"Row_Variable"})
#' @param row_levels Character vector of row category labels
#' @param col_var Name of the column variable (default = \code{"Col_Variable"})
#' @param col_levels Character vector of column category labels
#' @param association Type of association. One of: \code{"none"} (variables
#'   are independent, default), \code{"effect_size"} (specify target Cramér's
#'   V), or \code{"custom"} (provide exact cell probabilities).
#' @param cell_probs Matrix of cell probabilities (rows × cols). Required when
#'   \code{association = "custom"}.
#' @param cramers_v Target Cramér's V (0–1). Required when
#'   \code{association = "effect_size"}.
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run chi-square test automatically?
#'   (default = TRUE)
#'
#' @return A list with:
#'   \item{data}{Long-format data frame}
#'   \item{contingency_table}{Observed contingency table}
#'   \item{observed_counts}{Matrix of observed cell counts}
#'   \item{chisq_test_results}{\code{chisq.test()} result (if
#'     \code{run_analysis = TRUE})}
#'   \item{cramers_v}{Achieved Cramér's V (if \code{run_analysis = TRUE})}
#'   \item{phi}{Phi coefficient for 2×2 tables (if applicable)}
#'   \item{assumptions}{Assumption checks (if \code{run_analysis = TRUE})}
#'
#' @examples
#' # Independence (no association)
#' result <- chisq_indep_gen(
#'   n = 200,
#'   row_var = "Gender", row_levels = c("Male", "Female"),
#'   col_var = "Preference", col_levels = c("A", "B", "C")
#' )
#'
#' # Target Cramer's V = 0.30
#' result <- chisq_indep_gen(
#'   n = 300,
#'   row_var = "Treatment", row_levels = c("Control", "Treated"),
#'   col_var = "Outcome", col_levels = c("Fail", "Pass"),
#'   association = "effect_size", cramers_v = 0.30
#' )
#'
#' @export
chisq_indep_gen <- function(n,
                              row_var    = "Row_Variable",
                              row_levels,
                              col_var    = "Col_Variable",
                              col_levels,
                              association = "none",
                              cell_probs  = NULL,
                              cramers_v   = NULL,
                              seed        = NULL,
                              export_csv  = FALSE,
                              run_analysis = TRUE) {

  if (!is.numeric(n) || n <= 0 || n != round(n)) stop("'n' must be a positive integer")
  if (length(row_levels) < 2) stop("Must have at least 2 row levels")
  if (length(col_levels) < 2) stop("Must have at least 2 column levels")
  if (any(nchar(row_levels) == 0)) stop("Row category names cannot be empty")
  if (any(nchar(col_levels) == 0)) stop("Column category names cannot be empty")
  if (any(duplicated(row_levels))) stop("Row category names must be unique")
  if (any(duplicated(col_levels))) stop("Column category names must be unique")
  if (!association %in% c("none", "effect_size", "custom"))
    stop("'association' must be 'none', 'effect_size', or 'custom'")
  if (association == "effect_size") {
    if (is.null(cramers_v)) stop("'cramers_v' required when association = 'effect_size'")
    if (cramers_v < 0 || cramers_v > 1) stop("'cramers_v' must be between 0 and 1")
  }
  if (association == "custom") {
    if (is.null(cell_probs)) stop("'cell_probs' required when association = 'custom'")
    if (!is.matrix(cell_probs)) stop("'cell_probs' must be a matrix")
    if (nrow(cell_probs) != length(row_levels) || ncol(cell_probs) != length(col_levels))
      stop("'cell_probs' dimensions must match row_levels x col_levels")
    if (any(cell_probs < 0)) stop("Cell probabilities cannot be negative")
    if (abs(sum(cell_probs) - 1) > 0.01) {
      warning("Cell probabilities sum to ", round(sum(cell_probs), 3), ". Normalizing.")
      cell_probs <- cell_probs / sum(cell_probs)
    }
  }

  if (!is.null(seed)) set.seed(seed)

  r <- length(row_levels)
  c <- length(col_levels)
  df <- (r - 1) * (c - 1)

  # Build cell probability matrix
  if (association == "none") {
    expected_probs <- outer(rep(1/r, r), rep(1/c, c))
    dimnames(expected_probs) <- list(row_levels, col_levels)

  } else if (association == "effect_size") {
    # Approximate target V by perturbing the independence matrix
    base_probs <- outer(rep(1/r, r), rep(1/c, c))
    t_val <- min(r - 1, c - 1)
    # Scale of perturbation roughly tied to target V
    strength <- cramers_v * sqrt(t_val)
    noise <- matrix(stats::runif(r * c, -strength/2, strength/2), r, c)
    expected_probs <- pmax(base_probs + noise * base_probs, 1e-4)
    expected_probs <- expected_probs / sum(expected_probs)
    dimnames(expected_probs) <- list(row_levels, col_levels)

  } else {
    expected_probs <- cell_probs
    dimnames(expected_probs) <- list(row_levels, col_levels)
  }

  # Add small random noise for realistic variation
  pv    <- as.vector(t(expected_probs))
  noise <- stats::runif(length(pv), -0.02, 0.02)
  pert  <- pmax(pv + noise, 0.001)
  pert  <- pert / sum(pert)

  obs_vec <- as.vector(stats::rmultinom(n = 1, size = n, prob = pert))
  obs_mat <- matrix(obs_vec, nrow = r, ncol = c, byrow = TRUE)
  dimnames(obs_mat) <- list(row_levels, col_levels)

  # Expand to individual observations
  rows_list <- list()
  idx <- 1
  for (i in seq_len(r)) {
    for (j in seq_len(c)) {
      cnt <- obs_mat[i, j]
      if (cnt > 0) {
        rows_list[[idx]] <- data.frame(
          rv = rep(row_levels[i], cnt),
          cv = rep(col_levels[j], cnt),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
  }

  data_out <- do.call(rbind, rows_list)
  data_out$rv <- factor(data_out$rv, levels = row_levels)
  data_out$cv <- factor(data_out$cv, levels = col_levels)
  colnames(data_out) <- c(row_var, col_var)
  data_out <- data_out[sample(nrow(data_out)), ]
  rownames(data_out) <- NULL

  ctab <- table(data_out[[row_var]], data_out[[col_var]])

  chisq_test_results <- NULL
  cramers_v_ach      <- NULL
  phi                <- NULL
  assumptions        <- NULL

  if (run_analysis) {
    chisq_test_results <- stats::chisq.test(ctab)
    chi_sq  <- as.numeric(chisq_test_results$statistic)
    t_val   <- min(r - 1, c - 1)
    cramers_v_ach <- sqrt(chi_sq / (n * t_val))
    if (r == 2 && c == 2) phi <- sqrt(chi_sq / n)

    exp_f <- chisq_test_results$expected
    assumptions <- list(
      adequate_n         = n >= 5 * r * c,
      expected_freq_gt_5 = all(exp_f >= 5),
      pct_cells_gt_5     = mean(exp_f >= 5) * 100,
      min_expected       = min(exp_f),
      all_adequate       = all(exp_f >= 5)
    )
  }

  dgen_export(data_out, export_csv,
              paste0(row_var, "_by_", col_var, "_dgen.csv"))

  message(sprintf("Chi-square independence data: %dx%d table, n = %d", r, c, n))
  message(paste(utils::capture.output(stats::addmargins(ctab)), collapse = "\n"))
  if (run_analysis && !is.null(chisq_test_results)) {
    p_val <- chisq_test_results$p.value
    message(sprintf("chi2(%d) = %.3f, p = %s | Cramer's V = %.3f",
                df,
                as.numeric(chisq_test_results$statistic),
                format.pval(p_val, digits = 3, eps = 0.001),
                cramers_v_ach))
  }

  result <- list(
    data              = data_out,
    contingency_table = ctab,
    observed_counts   = obs_mat,
    expected_probs    = expected_probs,
    n                 = n,
    dimensions        = list(nrow = r, ncol = c),
    df                = df
  )
  if (run_analysis) {
    result$chisq_test_results <- chisq_test_results
    result$cramers_v          <- cramers_v_ach
    if (!is.null(phi)) result$phi <- phi
    result$assumptions        <- assumptions
  }

  invisible(result)
}
