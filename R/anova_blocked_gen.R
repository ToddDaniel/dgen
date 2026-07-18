#' Generate Data for Randomized Block (Repeated-Measures) ANOVA
#'
#' Creates data for a randomized block (two-factor without replication) ANOVA
#' where each block is exposed to all treatments. Uses iterative refinement
#' with \code{MASS::mvrnorm()} to achieve EXACT target means and SDs while
#' maintaining within-block correlation.
#'
#' This design is the randomized-block analogue of a one-way repeated-measures
#' (within-subjects) ANOVA: blocks play the role of subjects, and every
#' block/subject is measured under all treatment conditions.
#'
#' @param n_per_block Number of replications per block (minimum 2)
#' @param n_blocks Number of blocks (minimum 2)
#' @param block_labels Character vector of block names (length \code{n_blocks})
#' @param block_name Name for the blocking factor (default = \code{"Block"})
#' @param n_treatments Number of treatment levels (minimum 2)
#' @param treatment_labels Character vector of treatment names (length
#'   \code{n_treatments})
#' @param treatment_name Name for the treatment factor
#'   (default = \code{"Treatment"})
#' @param cell_means Matrix of means (n_blocks x n_treatments)
#' @param within_cell_sd Within-cell standard deviation(s). Either a single
#'   value or a matrix matching \code{cell_means} dimensions
#' @param within_block_correlation Correlation between treatments within the
#'   same block (default = 0.70)
#' @param dv_name Name for the dependent variable (default = \code{"Score"})
#' @param decimals Number of decimal places for rounding DV (default = 2)
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination.
#'   When \code{output_format = "both"} and a file path is supplied,
#'   \code{_wide}/\code{_long} is inserted before the extension
#' @param output_format \code{"wide"}, \code{"long"}, or \code{"both"}
#'   (default = \code{"long"})
#' @param run_analysis Logical; run randomized block ANOVA automatically?
#'   (default = \code{TRUE})
#'
#' @return A list with:
#'   \item{data_wide}{Wide-format data frame (blocks as rows, treatments as
#'     columns)}
#'   \item{data_long}{Long-format data frame}
#'   \item{cell_stats}{Descriptive statistics for each cell}
#'   \item{marginal_means}{Treatment and block marginal means}
#'   \item{anova_table}{ANOVA summary table (if \code{run_analysis = TRUE})}
#'   \item{effect_sizes}{Eta-squared and omega-squared for treatment and block}
#'   \item{assumptions}{Levene's test and variance ratio}
#'   \item{posthoc}{Tukey HSD for treatments (if significant and > 2 levels)}
#'   \item{correlation_matrix}{Average within-block correlation matrix}
#'   \item{iterations}{Vector of iteration counts per block}
#'
#' @examples
#' # Small 2-block x 2-treatment design that converges quickly
#' quick <- anova_blocked_gen(
#'   n_per_block      = 5,
#'   n_blocks         = 2,
#'   block_labels     = c("Morning", "Evening"),
#'   n_treatments     = 2,
#'   treatment_labels = c("Method_A", "Method_B"),
#'   cell_means       = matrix(c(10, 12,
#'                               14, 16), nrow = 2, byrow = TRUE),
#'   within_cell_sd   = 2,
#'   seed             = 42
#' )
#' quick$anova_table
#'
#' \donttest{
#' # Larger textbook-style example. The iterative refinement needed to hit
#' # exact cell means/SDs under the within-block correlation constraint can
#' # take ~30 seconds for a design of this size.
#' result <- anova_blocked_gen(
#'   n_per_block          = 3,
#'   n_blocks             = 4,
#'   block_labels         = c("Atlanta-Seattle", "NYC-LA",
#'                             "Cleveland-Orlando", "Dallas-Indy"),
#'   block_name           = "Route",
#'   n_treatments         = 3,
#'   treatment_labels     = c("Expedia", "TripAdvisor", "Priceline"),
#'   treatment_name       = "Website",
#'   cell_means           = matrix(c(176, 166, 175.8,
#'                                   195, 195, 206.2,
#'                                    77,  72,  76.2,
#'                                   149, 149, 148.2),
#'                                 nrow = 4, byrow = TRUE),
#'   within_cell_sd       = 5,
#'   within_block_correlation = 0.75,
#'   dv_name              = "Cost",
#'   seed                 = 12345
#' )
#' result$anova_table
#' }
#'
#' @concept repeated measures ANOVA
#' @concept within-subjects design
#' @concept randomized block design
#'
#' @export
anova_blocked_gen <- function(n_per_block,
                               n_blocks,
                               block_labels,
                               block_name               = "Block",
                               n_treatments,
                               treatment_labels,
                               treatment_name           = "Treatment",
                               cell_means,
                               within_cell_sd,
                               within_block_correlation = 0.70,
                               dv_name                  = "Score",
                               decimals                 = 2,
                               seed                     = NULL,
                               export_csv               = FALSE,
                               output_format            = "long",
                               run_analysis             = TRUE) {

  if (!is.numeric(n_per_block) || n_per_block < 2 ||
      n_per_block != round(n_per_block))
    stop("'n_per_block' must be an integer >= 2")
  if (!is.numeric(n_blocks) || n_blocks < 2 || n_blocks != round(n_blocks))
    stop("'n_blocks' must be an integer >= 2")
  if (!is.numeric(n_treatments) || n_treatments < 2 ||
      n_treatments != round(n_treatments))
    stop("'n_treatments' must be an integer >= 2")
  if (length(block_labels) != n_blocks)
    stop("Length of 'block_labels' must equal 'n_blocks'")
  if (length(treatment_labels) != n_treatments)
    stop("Length of 'treatment_labels' must equal 'n_treatments'")
  if (!is.matrix(cell_means) || nrow(cell_means) != n_blocks ||
      ncol(cell_means) != n_treatments)
    stop("'cell_means' must be a matrix with dimensions ",
         n_blocks, " x ", n_treatments)
  if (!is.numeric(within_block_correlation) ||
      within_block_correlation < -1 || within_block_correlation > 1)
    stop("'within_block_correlation' must be between -1 and 1")
  if (!output_format %in% c("wide", "long", "both"))
    stop("'output_format' must be 'wide', 'long', or 'both'")

  if (length(within_cell_sd) == 1) {
    within_cell_sd <- matrix(within_cell_sd, nrow = n_blocks,
                              ncol = n_treatments)
  }
  if (!is.matrix(within_cell_sd) || nrow(within_cell_sd) != n_blocks ||
      ncol(within_cell_sd) != n_treatments)
    stop("'within_cell_sd' dimensions must match 'cell_means'")
  if (any(within_cell_sd < 0)) stop("All SD values must be non-negative")

  if (!is.null(seed)) set.seed(seed)

  N            <- n_blocks * n_treatments * n_per_block
  target_means <- round(cell_means,    decimals + 1)
  target_sds   <- round(within_cell_sd, decimals + 1)
  target_cor   <- round(within_block_correlation, 3)
  max_iter     <- 100000

  block_data_list    <- vector("list", n_blocks)
  iterations_vector  <- integer(n_blocks)

  for (block_i in seq_len(n_blocks)) {
    iteration <- 0L

    repeat {
      iteration <- iteration + 1L

      means_blk <- cell_means[block_i, ]
      sds_blk   <- within_cell_sd[block_i, ]

      Sigma_cor <- matrix(within_block_correlation,
                           nrow = n_treatments, ncol = n_treatments)
      diag(Sigma_cor) <- 1
      D         <- diag(sds_blk)
      Sigma_cov <- D %*% Sigma_cor %*% D

      raw_data  <- MASS::mvrnorm(n = n_per_block,
                                  mu    = means_blk,
                                  Sigma = Sigma_cov)

      std_data <- apply(raw_data, 2, function(x) (x - mean(x)) / stats::sd(x))
      rescaled <- matrix(0, nrow = n_per_block, ncol = n_treatments)
      for (j in seq_len(n_treatments)) {
        rescaled[, j] <- std_data[, j] * target_sds[block_i, j] +
                         target_means[block_i, j]
      }
      blk_data <- round(rescaled, decimals)

      ach_means <- apply(blk_data, 2,
                          function(x) round(mean(x), decimals + 1))
      ach_sds   <- apply(blk_data, 2,
                          function(x) round(stats::sd(x), decimals + 1))
      off_cors  <- stats::cor(blk_data)[lower.tri(stats::cor(blk_data))]
      ach_cor   <- round(mean(off_cors), 3)

      if (all(ach_means == target_means[block_i, ]) &&
          all(ach_sds   == target_sds[block_i, ]) &&
          ach_cor == target_cor) break

      if (iteration >= max_iter) {
        warning("Max iterations reached for block '", block_labels[block_i], "'")
        break
      }
    }

    block_data_list[[block_i]]  <- blk_data
    iterations_vector[block_i]  <- iteration
  }

  # Wide format: each row = one replication within a block
  total_rows <- n_blocks * n_per_block
  data_wide  <- data.frame(matrix(NA_real_, nrow = total_rows,
                                   ncol = n_treatments + 1))
  colnames(data_wide) <- c(block_name, treatment_labels)
  row_idx <- 1L
  for (block_i in seq_len(n_blocks)) {
    for (rep in seq_len(n_per_block)) {
      data_wide[row_idx, 1] <- block_labels[block_i]
      for (j in seq_len(n_treatments)) {
        data_wide[row_idx, j + 1] <- block_data_list[[block_i]][rep, j]
      }
      row_idx <- row_idx + 1L
    }
  }

  # Long format
  dv_vals    <- numeric(N)
  blk_vals   <- character(N)
  treat_vals <- character(N)
  obs_idx    <- 1L
  for (block_i in seq_len(n_blocks)) {
    for (j in seq_len(n_treatments)) {
      for (rep in seq_len(n_per_block)) {
        dv_vals[obs_idx]    <- block_data_list[[block_i]][rep, j]
        blk_vals[obs_idx]   <- block_labels[block_i]
        treat_vals[obs_idx] <- treatment_labels[j]
        obs_idx <- obs_idx + 1L
      }
    }
  }

  data_long <- data.frame(dv = dv_vals, blk = blk_vals, trt = treat_vals,
                           stringsAsFactors = FALSE)
  colnames(data_long) <- c(dv_name, block_name, treatment_name)
  data_long[[block_name]]     <- factor(data_long[[block_name]],
                                         levels = block_labels)
  data_long[[treatment_name]] <- factor(data_long[[treatment_name]],
                                         levels = treatment_labels)

  n_cells   <- n_blocks * n_treatments
  cell_stats <- data.frame(
    BLK       = rep(block_labels, each  = n_treatments),
    TRT       = rep(treatment_labels, times = n_blocks),
    n         = n_per_block,
    Mean      = numeric(n_cells),
    SD        = numeric(n_cells),
    Variance  = numeric(n_cells),
    stringsAsFactors = FALSE
  )
  colnames(cell_stats)[1:2] <- c(block_name, treatment_name)
  k <- 1L
  for (block_i in seq_len(n_blocks)) {
    for (j in seq_len(n_treatments)) {
      cell_stats$Mean[k]     <- mean(block_data_list[[block_i]][, j])
      cell_stats$SD[k]       <- stats::sd(block_data_list[[block_i]][, j])
      cell_stats$Variance[k] <- stats::var(block_data_list[[block_i]][, j])
      k <- k + 1L
    }
  }

  # Average within-block correlation matrix
  cor_mats       <- lapply(block_data_list, function(bd) {
    m <- stats::cor(bd)
    rownames(m) <- colnames(m) <- treatment_labels
    m
  })
  correlation_matrix <- Reduce("+", cor_mats) / n_blocks

  anova_table   <- NULL
  effect_sizes  <- NULL
  assumptions   <- NULL
  posthoc       <- NULL
  marginal_means <- NULL

  if (run_analysis) {
    cm_ach <- matrix(0, nrow = n_blocks, ncol = n_treatments)
    for (block_i in seq_len(n_blocks)) {
      for (j in seq_len(n_treatments)) {
        cm_ach[block_i, j] <- mean(block_data_list[[block_i]][, j])
      }
    }

    grand_mean       <- mean(dv_vals)
    marginal_treat   <- colMeans(cm_ach)
    marginal_block   <- rowMeans(cm_ach)

    marginal_means <- list(
      treatment = stats::setNames(
        data.frame(treatment_labels, marginal_treat,
                   stringsAsFactors = FALSE),
        c(treatment_name, "Mean")),
      block = stats::setNames(
        data.frame(block_labels, marginal_block,
                   stringsAsFactors = FALSE),
        c(block_name, "Mean"))
    )

    SSTreat  <- n_blocks * n_per_block * sum((marginal_treat - grand_mean)^2)
    SSBlocks <- n_treatments * n_per_block * sum((marginal_block - grand_mean)^2)
    SSE_val  <- 0
    for (block_i in seq_len(n_blocks)) {
      for (j in seq_len(n_treatments)) {
        cell_d <- block_data_list[[block_i]][, j]
        SSE_val <- SSE_val + sum((cell_d - cm_ach[block_i, j])^2)
      }
    }
    SST <- sum((dv_vals - grand_mean)^2)

    df_treat  <- n_treatments - 1
    df_blocks <- n_blocks - 1
    df_error  <- (n_blocks * n_treatments - 1) - df_treat - df_blocks
    df_total  <- N - 1

    MSTreat  <- SSTreat  / df_treat
    MSBlocks <- SSBlocks / df_blocks
    MSE_v    <- SSE_val  / df_error

    F_treat  <- MSTreat  / MSE_v
    F_blocks <- MSBlocks / MSE_v
    p_treat  <- stats::pf(F_treat,  df_treat,  df_error, lower.tail = FALSE)
    p_blocks <- stats::pf(F_blocks, df_blocks, df_error, lower.tail = FALSE)

    anova_table <- data.frame(
      Source = c(treatment_name, block_name, "Error", "Total"),
      SS     = c(SSTreat, SSBlocks, SSE_val, SST),
      df     = c(df_treat, df_blocks, df_error, df_total),
      MS     = c(MSTreat, MSBlocks, MSE_v, NA),
      F      = c(F_treat, F_blocks, NA, NA),
      p      = c(p_treat, p_blocks, NA, NA),
      stringsAsFactors = FALSE
    )

    effect_sizes <- list(
      treatment = list(
        eta_squared           = SSTreat  / SST,
        eta_squared_partial   = SSTreat  / (SSTreat  + SSE_val),
        omega_squared         = (SSTreat  - df_treat  * MSE_v) / (SST + MSE_v),
        omega_squared_partial = (df_treat  * (MSTreat  - MSE_v)) /
                                (SSTreat  + (N - df_treat)  * MSE_v)
      ),
      blocks = list(
        eta_squared           = SSBlocks / SST,
        eta_squared_partial   = SSBlocks / (SSBlocks + SSE_val),
        omega_squared         = (SSBlocks - df_blocks * MSE_v) / (SST + MSE_v),
        omega_squared_partial = (df_blocks * (MSBlocks - MSE_v)) /
                                (SSBlocks + (N - df_blocks) * MSE_v)
      )
    )

    # Levene's test (cell-level)
    abs_devs_list <- vector("list", n_cells)
    k <- 1L
    for (block_i in seq_len(n_blocks)) {
      for (j in seq_len(n_treatments)) {
        cd <- block_data_list[[block_i]][, j]
        abs_devs_list[[k]] <- abs(cd - stats::median(cd))
        k <- k + 1L
      }
    }
    mad_means  <- sapply(abs_devs_list, mean)
    grand_mad  <- mean(unlist(abs_devs_list))
    SSB_lev    <- n_per_block * sum((mad_means - grand_mad)^2)
    SSW_lev    <- sum(sapply(abs_devs_list, function(x) sum((x - mean(x))^2)))
    F_lev      <- (SSB_lev / (n_cells - 1)) / (SSW_lev / (N - n_cells))
    p_lev      <- stats::pf(F_lev, n_cells - 1, N - n_cells, lower.tail = FALSE)
    cell_vars_v <- sapply(abs_devs_list, stats::var)
    cell_vars_v <- cell_vars_v[!is.na(cell_vars_v) & cell_vars_v > 0]
    var_ratio_v <- if (length(cell_vars_v) > 0)
      max(cell_vars_v) / min(cell_vars_v) else NA

    assumptions <- list(levene_F = F_lev, levene_p = p_lev,
                        variance_ratio = var_ratio_v)

    # Post-hoc for treatment (Tukey HSD)
    if (p_treat < 0.05 && n_treatments > 2) {
      q_crit <- stats::qtukey(0.95, n_treatments, df_error)
      comps  <- utils::combn(n_treatments, 2)
      posthoc <- data.frame(
        Comparison  = paste(treatment_labels[comps[1, ]], "vs",
                            treatment_labels[comps[2, ]]),
        Mean_Diff   = abs(marginal_treat[comps[1, ]] -
                          marginal_treat[comps[2, ]]),
        HSD         = q_crit * sqrt(MSE_v / (n_blocks * n_per_block)),
        stringsAsFactors = FALSE
      )
      posthoc$Significant <- posthoc$Mean_Diff > posthoc$HSD
    }
  }

  dgen_export_pair(data_wide, data_long, output_format, export_csv,
                   paste0(dv_name, "_blocked_wide_dgen.csv"),
                   paste0(dv_name, "_blocked_long_dgen.csv"))

  message(sprintf(
    "anova_blocked_gen: %d blocks x %d treatments, n_per_block = %d, N = %d",
    n_blocks, n_treatments, n_per_block, N))
  if (run_analysis) {
    message(sprintf("%s: F(%d,%d) = %.3f, p = %s",
                    treatment_name, df_treat, df_error, F_treat,
                    format.pval(p_treat, digits = 3, eps = 0.001)))
    message(sprintf("%s: F(%d,%d) = %.3f, p = %s",
                    block_name, df_blocks, df_error, F_blocks,
                    format.pval(p_blocks, digits = 3, eps = 0.001)))
  }

  result <- list(
    cell_stats         = cell_stats,
    correlation_matrix = correlation_matrix,
    iterations         = iterations_vector
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
    result$anova_table   <- anova_table
    result$effect_sizes  <- effect_sizes
    result$assumptions   <- assumptions
    result$marginal_means <- marginal_means
    if (!is.null(posthoc)) result$posthoc <- posthoc
  }

  invisible(result)
}
