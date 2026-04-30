#' Generate Data for Two-Way (Factorial) ANOVA
#'
#' Creates data for a two-way factorial ANOVA with two independent variables
#' (factors) and their interaction effect on a continuous dependent variable.
#' Uses iterative refinement to achieve EXACT target cell means and standard
#' deviations.
#'
#' @param factor_a_levels Integer. Number of levels in Factor A (minimum 2)
#' @param factor_a_labels Character vector of labels for Factor A levels
#' @param factor_a_name Name for Factor A (default = \code{"FactorA"})
#' @param factor_b_levels Integer. Number of levels in Factor B (minimum 2)
#' @param factor_b_labels Character vector of labels for Factor B levels
#' @param factor_b_name Name for Factor B (default = \code{"FactorB"})
#' @param n_per_cell Sample size per cell (default = 5)
#' @param cell_means Matrix of means (factor_a_levels x factor_b_levels,
#'   rows = Factor A, columns = Factor B)
#' @param cell_sds Standard deviation(s). Either a single numeric value (same
#'   SD for all cells) or a matrix matching \code{cell_means} dimensions
#' @param dv_name Name for the dependent variable (default = \code{"Score"})
#' @param decimals Number of decimal places for rounding DV (default = 2)
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Logical; export data to CSV? (default = \code{FALSE})
#' @param output_format \code{"wide"}, \code{"long"}, or \code{"both"}
#'   (default = \code{"long"})
#' @param run_analysis Logical; run two-way ANOVA automatically?
#'   (default = \code{TRUE})
#'
#' @return A list with:
#'   \item{data_wide}{Wide-format data frame (Factor A groups as rows,
#'     Factor B levels as columns)}
#'   \item{data_long}{Long-format data frame (DV, FactorA, FactorB columns)}
#'   \item{cell_stats}{Descriptive statistics for each cell}
#'   \item{anova_table}{ANOVA summary table (if \code{run_analysis = TRUE})}
#'   \item{effect_sizes}{Eta-squared and omega-squared for each effect}
#'   \item{assumptions}{Levene's test and variance ratio}
#'   \item{posthoc_a}{Tukey HSD for Factor A marginal means (if applicable)}
#'   \item{posthoc_b}{Tukey HSD for Factor B marginal means (if applicable)}
#'   \item{iterations}{Matrix of iteration counts per cell}
#'
#' @examples
#' result <- anova_twoway_gen(
#'   factor_a_levels = 3,
#'   factor_a_labels = c("Arabica", "Robusta", "Liberica"),
#'   factor_a_name   = "Bean",
#'   factor_b_levels = 3,
#'   factor_b_labels = c("Ancient", "Equal", "Perkatory"),
#'   factor_b_name   = "Location",
#'   n_per_cell      = 5,
#'   cell_means      = matrix(c(7.8, 9.0, 11.0,
#'                               2.2, 15.0, 14.8,
#'                              14.0,  8.2,  8.0),
#'                            nrow = 3, byrow = TRUE),
#'   cell_sds        = 3.0,
#'   dv_name         = "Yield",
#'   seed            = 12345
#' )
#' result$anova_table
#'
#' @export
anova_twoway_gen <- function(factor_a_levels,
                              factor_a_labels,
                              factor_a_name   = "FactorA",
                              factor_b_levels,
                              factor_b_labels,
                              factor_b_name   = "FactorB",
                              n_per_cell      = 5,
                              cell_means,
                              cell_sds,
                              dv_name         = "Score",
                              decimals        = 2,
                              seed            = NULL,
                              export_csv      = FALSE,
                              output_format   = "long",
                              run_analysis    = TRUE) {

  if (!is.numeric(factor_a_levels) || factor_a_levels < 2 ||
      factor_a_levels != round(factor_a_levels))
    stop("'factor_a_levels' must be an integer >= 2")
  if (!is.numeric(factor_b_levels) || factor_b_levels < 2 ||
      factor_b_levels != round(factor_b_levels))
    stop("'factor_b_levels' must be an integer >= 2")
  if (length(factor_a_labels) != factor_a_levels)
    stop("Length of 'factor_a_labels' must equal 'factor_a_levels'")
  if (length(factor_b_labels) != factor_b_levels)
    stop("Length of 'factor_b_labels' must equal 'factor_b_levels'")
  if (!is.matrix(cell_means) || nrow(cell_means) != factor_a_levels ||
      ncol(cell_means) != factor_b_levels)
    stop("'cell_means' must be a matrix with dimensions ",
         factor_a_levels, " x ", factor_b_levels)
  if (!output_format %in% c("wide", "long", "both"))
    stop("'output_format' must be 'wide', 'long', or 'both'")

  # Expand cell_sds to matrix if single value
  if (length(cell_sds) == 1) {
    cell_sds <- matrix(cell_sds, nrow = factor_a_levels, ncol = factor_b_levels)
  }
  if (!is.matrix(cell_sds) || nrow(cell_sds) != factor_a_levels ||
      ncol(cell_sds) != factor_b_levels)
    stop("'cell_sds' dimensions must match 'cell_means'")
  if (any(cell_sds < 0)) stop("All SD values must be non-negative")

  if (!is.null(seed)) set.seed(seed)

  n_cells      <- factor_a_levels * factor_b_levels
  N            <- n_cells * n_per_cell
  target_means <- round(cell_means, decimals + 1)
  target_sds   <- round(cell_sds,   decimals + 1)
  max_iter     <- 100000

  cell_data_list    <- vector("list", n_cells)
  iterations_matrix <- matrix(0L, nrow = factor_a_levels,
                                   ncol = factor_b_levels)
  cell_index <- 1L

  for (i in seq_len(factor_a_levels)) {
    for (j in seq_len(factor_b_levels)) {
      iteration <- 0L

      repeat {
        iteration <- iteration + 1L
        raw_data  <- stats::rnorm(n_per_cell, cell_means[i, j], cell_sds[i, j])
        z_scores  <- scale(raw_data)
        rescaled  <- z_scores[, 1] * target_sds[i, j] + target_means[i, j]
        cell_data <- round(rescaled, decimals)

        if (round(mean(cell_data), decimals + 1) == target_means[i, j] &&
            round(stats::sd(cell_data), decimals + 1) == target_sds[i, j])
          break

        if (iteration >= max_iter) {
          warning("Max iterations reached for cell [",
                  factor_a_labels[i], ", ", factor_b_labels[j], "]")
          break
        }
      }

      cell_data_list[[cell_index]] <- cell_data
      iterations_matrix[i, j]      <- iteration
      cell_index                    <- cell_index + 1L
    }
  }

  # Wide format
  data_wide <- data.frame(
    matrix(NA_real_, nrow = factor_a_levels * n_per_cell,
           ncol = factor_b_levels + 1))
  colnames(data_wide) <- c(factor_a_name, factor_b_labels)

  row_idx <- 1L
  for (i in seq_len(factor_a_levels)) {
    for (rep in seq_len(n_per_cell)) {
      data_wide[row_idx, 1] <- factor_a_labels[i]
      for (j in seq_len(factor_b_levels)) {
        ci <- (i - 1) * factor_b_levels + j
        data_wide[row_idx, j + 1] <- cell_data_list[[ci]][rep]
      }
      row_idx <- row_idx + 1L
    }
  }

  # Long format
  dv_vals  <- numeric(N)
  fa_vals  <- character(N)
  fb_vals  <- character(N)
  obs_idx  <- 1L
  ci       <- 1L
  for (i in seq_len(factor_a_levels)) {
    for (j in seq_len(factor_b_levels)) {
      for (rep in seq_len(n_per_cell)) {
        dv_vals[obs_idx] <- cell_data_list[[ci]][rep]
        fa_vals[obs_idx] <- factor_a_labels[i]
        fb_vals[obs_idx] <- factor_b_labels[j]
        obs_idx <- obs_idx + 1L
      }
      ci <- ci + 1L
    }
  }

  data_long <- data.frame(dv = dv_vals, fa = fa_vals, fb = fb_vals,
                           stringsAsFactors = FALSE)
  colnames(data_long) <- c(dv_name, factor_a_name, factor_b_name)
  data_long[[factor_a_name]] <- factor(data_long[[factor_a_name]],
                                        levels = factor_a_labels)
  data_long[[factor_b_name]] <- factor(data_long[[factor_b_name]],
                                        levels = factor_b_labels)

  # Cell stats
  cell_stats <- data.frame(
    FA         = rep(factor_a_labels, each  = factor_b_levels),
    FB         = rep(factor_b_labels, times = factor_a_levels),
    n          = n_per_cell,
    Mean       = numeric(n_cells),
    SD         = numeric(n_cells),
    Variance   = numeric(n_cells),
    stringsAsFactors = FALSE
  )
  colnames(cell_stats)[1:2] <- c(factor_a_name, factor_b_name)
  for (k in seq_len(n_cells)) {
    cell_stats$Mean[k]     <- mean(cell_data_list[[k]])
    cell_stats$SD[k]       <- stats::sd(cell_data_list[[k]])
    cell_stats$Variance[k] <- stats::var(cell_data_list[[k]])
  }

  anova_table   <- NULL
  effect_sizes  <- NULL
  assumptions   <- NULL
  posthoc_a     <- NULL
  posthoc_b     <- NULL

  if (run_analysis) {
    # Cell means (actual)
    cmact <- matrix(0, nrow = factor_a_levels, ncol = factor_b_levels)
    ci <- 1L
    for (i in seq_len(factor_a_levels)) {
      for (j in seq_len(factor_b_levels)) {
        cmact[i, j] <- mean(cell_data_list[[ci]])
        ci <- ci + 1L
      }
    }

    grand_mean  <- mean(dv_vals)
    marginal_a  <- rowMeans(cmact)
    marginal_b  <- colMeans(cmact)

    SSA  <- factor_b_levels * n_per_cell * sum((marginal_a - grand_mean)^2)
    SSB  <- factor_a_levels * n_per_cell * sum((marginal_b - grand_mean)^2)
    SSab_cells <- n_per_cell * sum((cmact - grand_mean)^2)
    SSAB <- SSab_cells - SSA - SSB
    SSE  <- 0
    ci   <- 1L
    for (i in seq_len(factor_a_levels)) {
      for (j in seq_len(factor_b_levels)) {
        SSE <- SSE + sum((cell_data_list[[ci]] - cmact[i, j])^2)
        ci  <- ci + 1L
      }
    }
    SST <- SSA + SSB + SSAB + SSE

    df_A     <- factor_a_levels - 1
    df_B     <- factor_b_levels - 1
    df_AB    <- df_A * df_B
    df_Error <- factor_a_levels * factor_b_levels * (n_per_cell - 1)
    df_Total <- N - 1

    MSA  <- SSA  / df_A
    MSB  <- SSB  / df_B
    MSAB <- SSAB / df_AB
    MSE  <- SSE  / df_Error

    F_A  <- MSA  / MSE
    F_B  <- MSB  / MSE
    F_AB <- MSAB / MSE

    p_A  <- stats::pf(F_A,  df_A,  df_Error, lower.tail = FALSE)
    p_B  <- stats::pf(F_B,  df_B,  df_Error, lower.tail = FALSE)
    p_AB <- stats::pf(F_AB, df_AB, df_Error, lower.tail = FALSE)

    anova_table <- data.frame(
      Source = c(factor_a_name, factor_b_name,
                 paste(factor_a_name, "x", factor_b_name),
                 "Error", "Total"),
      SS     = c(SSA, SSB, SSAB, SSE, SST),
      df     = c(df_A, df_B, df_AB, df_Error, df_Total),
      MS     = c(MSA, MSB, MSAB, MSE, NA),
      F      = c(F_A, F_B, F_AB, NA, NA),
      p      = c(p_A, p_B, p_AB, NA, NA),
      stringsAsFactors = FALSE
    )

    effect_sizes <- list(
      factor_a = list(
        eta_squared          = SSA  / SST,
        eta_squared_partial  = SSA  / (SSA  + SSE),
        omega_squared        = (SSA  - df_A  * MSE) / (SST + MSE),
        omega_squared_partial = (df_A  * (MSA  - MSE)) / (SSA  + (N - df_A)  * MSE)
      ),
      factor_b = list(
        eta_squared          = SSB  / SST,
        eta_squared_partial  = SSB  / (SSB  + SSE),
        omega_squared        = (SSB  - df_B  * MSE) / (SST + MSE),
        omega_squared_partial = (df_B  * (MSB  - MSE)) / (SSB  + (N - df_B)  * MSE)
      ),
      interaction = list(
        eta_squared          = SSAB / SST,
        eta_squared_partial  = SSAB / (SSAB + SSE),
        omega_squared        = (SSAB - df_AB * MSE) / (SST + MSE),
        omega_squared_partial = (df_AB * (MSAB - MSE)) / (SSAB + (N - df_AB) * MSE)
      )
    )

    # Levene's test (median-based)
    cell_medians  <- sapply(cell_data_list, stats::median)
    abs_devs      <- lapply(seq_len(n_cells), function(k)
      abs(cell_data_list[[k]] - cell_medians[k]))
    mean_abs_dev  <- sapply(abs_devs, mean)
    grand_mad     <- mean(unlist(abs_devs))
    SSB_lev       <- n_per_cell * sum((mean_abs_dev - grand_mad)^2)
    SSW_lev       <- sum(sapply(abs_devs, function(x) sum((x - mean(x))^2)))
    F_lev         <- (SSB_lev / (n_cells - 1)) / (SSW_lev / (N - n_cells))
    p_lev         <- stats::pf(F_lev, n_cells - 1, N - n_cells, lower.tail = FALSE)
    cell_vars     <- sapply(cell_data_list, stats::var)
    var_ratio     <- max(cell_vars) / min(cell_vars)

    assumptions <- list(levene_F = F_lev, levene_p = p_lev,
                        variance_ratio = var_ratio)

    # Post-hoc tests (main effects only when interaction NOT significant)
    if (p_AB >= 0.05) {
      if (p_A < 0.05 && factor_a_levels > 2) {
        q_crit    <- stats::qtukey(0.95, factor_a_levels, df_Error)
        comps     <- utils::combn(factor_a_levels, 2)
        posthoc_a <- data.frame(
          Comparison  = paste(factor_a_labels[comps[1, ]], "vs",
                              factor_a_labels[comps[2, ]]),
          Mean_Diff   = abs(marginal_a[comps[1, ]] - marginal_a[comps[2, ]]),
          HSD         = q_crit * sqrt(MSE / (factor_b_levels * n_per_cell)),
          stringsAsFactors = FALSE
        )
        posthoc_a$Significant <- posthoc_a$Mean_Diff > posthoc_a$HSD
      }

      if (p_B < 0.05 && factor_b_levels > 2) {
        q_crit    <- stats::qtukey(0.95, factor_b_levels, df_Error)
        comps     <- utils::combn(factor_b_levels, 2)
        posthoc_b <- data.frame(
          Comparison  = paste(factor_b_labels[comps[1, ]], "vs",
                              factor_b_labels[comps[2, ]]),
          Mean_Diff   = abs(marginal_b[comps[1, ]] - marginal_b[comps[2, ]]),
          HSD         = q_crit * sqrt(MSE / (factor_a_levels * n_per_cell)),
          stringsAsFactors = FALSE
        )
        posthoc_b$Significant <- posthoc_b$Mean_Diff > posthoc_b$HSD
      }
    }
  }

  if (export_csv) {
    if (output_format %in% c("long", "both")) {
      fn <- paste0(dv_name, "_twoway_long_dgen.csv")
      utils::write.csv(data_long, file = fn, row.names = FALSE)
      message("Long data saved to: ", file.path(getwd(), fn))
    }
    if (output_format %in% c("wide", "both")) {
      fn <- paste0(dv_name, "_twoway_wide_dgen.csv")
      utils::write.csv(data_wide, file = fn, row.names = FALSE)
      message("Wide data saved to: ", file.path(getwd(), fn))
    }
  }

  cat(sprintf("anova_twoway_gen: %d x %d design, n_per_cell = %d, N = %d\n",
              factor_a_levels, factor_b_levels, n_per_cell, N))
  if (run_analysis) {
    cat(sprintf("%s: F(%d,%d) = %.3f, p = %s\n",
                factor_a_name, df_A, df_Error, F_A,
                format.pval(p_A, digits = 3, eps = 0.001)))
    cat(sprintf("%s: F(%d,%d) = %.3f, p = %s\n",
                factor_b_name, df_B, df_Error, F_B,
                format.pval(p_B, digits = 3, eps = 0.001)))
    cat(sprintf("Interaction: F(%d,%d) = %.3f, p = %s\n",
                df_AB, df_Error, F_AB,
                format.pval(p_AB, digits = 3, eps = 0.001)))
  }

  result <- list(
    cell_stats        = cell_stats,
    iterations        = iterations_matrix
  )

  if (output_format == "wide") {
    result$data_wide <- data_wide
  } else if (output_format == "long") {
    result$data_long <- data_long
  } else {
    result$data_wide <- data_wide
    result$data_long <- data_long
  }

  if (run_analysis) {
    result$anova_table  <- anova_table
    result$effect_sizes <- effect_sizes
    result$assumptions  <- assumptions
    if (!is.null(posthoc_a)) result$posthoc_a <- posthoc_a
    if (!is.null(posthoc_b)) result$posthoc_b <- posthoc_b
  }

  invisible(result)
}
