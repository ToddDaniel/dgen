#' Generate Individual Observations from Chi-Square Frequency Table
#'
#' Creates long-format data from a contingency table (frequency counts).
#' Takes observed frequencies for each cell and generates individual
#' observations. Useful for converting summary tables to raw data for
#' student analysis.
#'
#' @param nrows Number of rows in contingency table
#' @param ncols Number of columns in contingency table
#' @param row_var Name for row variable (default = \code{"Row_Variable"})
#' @param col_var Name for column variable (default = \code{"Col_Variable"})
#' @param row_levels Character vector of row category names
#' @param col_levels Character vector of column category names
#' @param frequencies Matrix or vector of observed frequencies (by row)
#' @param add_id Add ID column? (default = \code{TRUE})
#' @param shuffle_rows Randomize row order? (default = \code{TRUE})
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run chi-square test automatically?
#'   (default = \code{TRUE})
#'
#' @return A list with:
#'   \item{data}{Data frame with individual observations (long format)}
#'   \item{contingency_table}{r x c contingency table}
#'   \item{observed_counts}{Matrix of observed frequencies}
#'   \item{n}{Total sample size}
#'   \item{dimensions}{List with nrow, ncol}
#'   \item{df}{Degrees of freedom}
#'   \item{chisq_test_results}{\code{chisq.test()} result (if
#'     \code{run_analysis = TRUE})}
#'   \item{cramers_v}{Cramer's V effect size (if \code{run_analysis = TRUE})}
#'   \item{phi}{Phi coefficient for 2x2 tables (if applicable)}
#'   \item{row_proportions}{Proportions within each row}
#'   \item{col_proportions}{Proportions within each column}
#'   \item{standardized_residuals}{Standardized residuals}
#'   \item{assumptions}{Assumption check results}
#'
#' @examples
#' result <- chisq_table_gen(
#'   nrows = 3,
#'   ncols = 2,
#'   row_var = "Building_Material",
#'   col_var = "Survived_Wolf_Attack",
#'   row_levels = c("Straw", "Sticks", "Bricks"),
#'   col_levels = c("Yes", "No"),
#'   frequencies = c(0, 15, 1, 24, 40, 0),
#'   seed = 123
#' )
#'
#' @export
chisq_table_gen <- function(nrows,
                             ncols,
                             row_var        = "Row_Variable",
                             col_var        = "Col_Variable",
                             row_levels,
                             col_levels,
                             frequencies,
                             add_id         = TRUE,
                             shuffle_rows   = TRUE,
                             seed           = NULL,
                             export_csv     = FALSE,
                             run_analysis   = TRUE) {

  if (!is.numeric(nrows) || nrows < 1 || nrows != round(nrows))
    stop("'nrows' must be a positive integer")
  if (!is.numeric(ncols) || ncols < 1 || ncols != round(ncols))
    stop("'ncols' must be a positive integer")
  if (length(row_levels) != nrows)
    stop("Length of 'row_levels' must equal 'nrows'")
  if (length(col_levels) != ncols)
    stop("Length of 'col_levels' must equal 'ncols'")
  if (any(row_levels == "")) stop("Row level names cannot be empty")
  if (any(col_levels == "")) stop("Column level names cannot be empty")
  if (any(duplicated(row_levels))) stop("Row level names must be unique")
  if (any(duplicated(col_levels))) stop("Column level names must be unique")

  total_cells <- nrows * ncols
  if (length(frequencies) != total_cells)
    stop("Length of 'frequencies' must equal nrows x ncols")
  if (any(frequencies < 0)) stop("Frequencies cannot be negative")
  if (any(frequencies != round(frequencies)))
    stop("Frequencies must be whole numbers")

  n <- sum(frequencies)
  if (n == 0) stop("Total sample size cannot be zero")

  if (!is.null(seed)) set.seed(seed)

  freq_matrix <- matrix(frequencies, nrow = nrows, ncol = ncols, byrow = TRUE)
  rownames(freq_matrix) <- row_levels
  colnames(freq_matrix) <- col_levels

  # Expand frequency matrix to individual observations
  data_list <- list()
  idx <- 1
  for (i in seq_len(nrows)) {
    for (j in seq_len(ncols)) {
      count <- freq_matrix[i, j]
      if (count > 0) {
        data_list[[idx]] <- data.frame(
          row_var = rep(row_levels[i], count),
          col_var = rep(col_levels[j], count),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
  }

  data <- do.call(rbind, data_list)
  data$row_var <- factor(data$row_var, levels = row_levels)
  data$col_var <- factor(data$col_var, levels = col_levels)
  colnames(data) <- c(row_var, col_var)

  if (add_id) {
    data <- data.frame(ID = seq_len(n), data)
  }

  if (shuffle_rows) {
    data <- data[sample(nrow(data)), ]
    rownames(data) <- NULL
  }

  contingency_table <- table(data[[row_var]], data[[col_var]])

  chisq_test_results <- NULL
  cramers_v          <- NULL
  phi                <- NULL
  row_proportions    <- NULL
  col_proportions    <- NULL
  standardized_residuals <- NULL
  assumptions        <- NULL

  if (run_analysis) {
    chisq_test_results <- stats::chisq.test(freq_matrix)

    chi_sq   <- as.numeric(chisq_test_results$statistic)
    t_min    <- min(nrows - 1, ncols - 1)
    cramers_v <- sqrt(chi_sq / (n * t_min))

    if (nrows == 2 && ncols == 2) {
      phi <- sqrt(chi_sq / n)
    }

    standardized_residuals <- chisq_test_results$stdres
    row_proportions        <- prop.table(freq_matrix, margin = 1)
    col_proportions        <- prop.table(freq_matrix, margin = 2)

    expected_freqs <- chisq_test_results$expected
    assumptions <- list(
      expected_freq_all_ge_5    = all(expected_freqs >= 5),
      pct_cells_ge_5            = mean(expected_freqs >= 5) * 100,
      min_expected              = min(expected_freqs),
      cells_below_5             = sum(expected_freqs < 5),
      total_cells               = total_cells
    )
  }

  dgen_export(data, export_csv,
              paste0(row_var, "_by_", col_var, "_dgen.csv"))

  message(sprintf("chisq_table_gen: %d x %d table, N = %d", nrows, ncols, n))
  if (run_analysis) {
    chi_s <- as.numeric(chisq_test_results$statistic)
    df_v  <- (nrows - 1) * (ncols - 1)
    message(sprintf("chi2(%d) = %.3f, p = %s, Cramer's V = %.3f",
                df_v, chi_s,
                format.pval(chisq_test_results$p.value, digits = 3, eps = 0.001),
                cramers_v))
  }

  result <- list(
    data               = data,
    contingency_table  = contingency_table,
    observed_counts    = freq_matrix,
    n                  = n,
    dimensions         = list(nrow = nrows, ncol = ncols),
    df                 = (nrows - 1) * (ncols - 1)
  )

  if (run_analysis) {
    result$chisq_test_results    <- chisq_test_results
    result$cramers_v             <- cramers_v
    if (!is.null(phi)) result$phi <- phi
    result$row_proportions       <- row_proportions
    result$col_proportions       <- col_proportions
    result$standardized_residuals <- standardized_residuals
    result$assumptions           <- assumptions
  }

  invisible(result)
}
