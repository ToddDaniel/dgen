#' Generate Data for a One-Way ANOVA
#'
#' Creates data for a one-way ANOVA design with \code{k} groups, each having
#' exact specified means and standard deviations. Uses iterative refinement for
#' precision. Automatically computes the ANOVA table, effect sizes, assumption
#' checks, and post-hoc tests when \code{run_analysis = TRUE}.
#'
#' @param k Number of groups (integer >= 3)
#' @param n Per-group sample size. Either a single integer (equal group sizes)
#'   or a vector of length \code{k} (unequal sizes).
#' @param means Numeric vector of length \code{k} specifying target group means
#' @param sds Numeric vector of length \code{k} specifying target group SDs
#' @param labels Character vector of length \code{k} specifying group labels.
#'   Defaults to \code{"Group1"}, \code{"Group2"}, etc.
#' @param dv_name Name for the dependent variable column (default =
#'   \code{"Score"})
#' @param factor_name Name for the grouping factor column (default =
#'   \code{"Group"})
#' @param decimals Number of decimal places for rounding (default = 2)
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Logical; export data to CSV? (default = FALSE)
#' @param output_format One of \code{"wide"}, \code{"long"}, or \code{"both"}
#'   (default = \code{"long"})
#' @param run_analysis Logical; compute ANOVA and related statistics?
#'   (default = TRUE)
#'
#' @return A list with:
#'   \item{data}{Primary data frame (per \code{output_format})}
#'   \item{data_wide}{Wide-format data (if \code{output_format} is
#'     \code{"wide"} or \code{"both"})}
#'   \item{data_long}{Long-format data (if \code{output_format} is
#'     \code{"long"} or \code{"both"})}
#'   \item{group_stats}{Descriptive statistics for each group}
#'   \item{iterations}{Iteration counts per group}
#'   \item{anova_table}{ANOVA source table (if \code{run_analysis = TRUE})}
#'   \item{effect_sizes}{eta², partial eta², omega², partial omega²}
#'   \item{assumptions}{Levene's test and Shapiro-Wilk results}
#'   \item{posthoc_fisher}{Fisher's LSD pairwise comparisons}
#'   \item{posthoc_tukey}{Tukey's HSD pairwise comparisons}
#'
#' @examples
#' result <- anova_gen(
#'   k = 3, n = 30,
#'   means = c(50, 55, 60), sds = c(10, 10, 10),
#'   labels = c("Control", "Low", "High"),
#'   dv_name = "Score"
#' )
#' result$anova_table
#' result$posthoc_tukey
#'
#' @export
anova_gen <- function(k,
                       n,
                       means,
                       sds,
                       labels       = NULL,
                       dv_name      = "Score",
                       factor_name  = "Group",
                       decimals     = 2,
                       seed         = NULL,
                       export_csv   = FALSE,
                       output_format = "long",
                       run_analysis = TRUE) {

  # Expand scalar n to vector
  if (length(n) == 1) n <- rep(n, k)

  # Default labels
  if (is.null(labels)) labels <- paste0("Group", seq_len(k))

  # Validation
  if (!is.numeric(k) || k < 3 || k != round(k))
    stop("'k' must be an integer >= 3 (use t_indep_gen() for 2 groups)")
  if (length(means) != k)  stop("Length of 'means' must equal k")
  if (length(sds) != k)    stop("Length of 'sds' must equal k")
  if (length(n) != k)      stop("Length of 'n' must equal k")
  if (length(labels) != k) stop("Length of 'labels' must equal k")
  if (any(n <= 0) || any(n != round(n))) stop("All values in 'n' must be positive integers")
  if (any(sds < 0))  stop("All values in 'sds' must be non-negative")
  if (!is.numeric(decimals) || decimals < 0 || decimals != round(decimals))
    stop("'decimals' must be a non-negative integer")
  if (any(labels == "")) stop("Group labels cannot be empty strings")
  if (length(unique(labels)) != k) stop("Group labels must be unique")
  if (!output_format %in% c("wide", "long", "both"))
    stop("'output_format' must be 'wide', 'long', or 'both'")

  if (!is.null(seed)) set.seed(seed)

  target_means <- round(means, decimals + 1)
  target_sds   <- round(sds,   decimals + 1)
  max_iter     <- 100000
  group_data   <- list()
  iterations   <- numeric(k)

  for (i in seq_len(k)) {
    iter <- 0
    repeat {
      iter  <- iter + 1
      z     <- scale(stats::rnorm(n[i], mean = means[i], sd = sds[i]))
      gd    <- round(z[, 1] * target_sds[i] + target_means[i], decimals)
      if (round(mean(gd), decimals + 1) == target_means[i] &&
          round(stats::sd(gd), decimals + 1) == target_sds[i]) break
      if (iter >= max_iter) {
        warning(paste("Max iterations reached for", labels[i]))
        break
      }
    }
    group_data[[i]] <- gd
    iterations[i]   <- iter
  }

  # Wide format
  max_n    <- max(n)
  data_wide <- as.data.frame(
    matrix(NA, nrow = max_n, ncol = k),
    stringsAsFactors = FALSE
  )
  colnames(data_wide) <- labels
  for (i in seq_len(k)) data_wide[seq_len(n[i]), i] <- group_data[[i]]

  # Long format
  data_long <- data.frame(
    dv     = unlist(group_data),
    factor = rep(labels, times = n),
    stringsAsFactors = FALSE
  )
  colnames(data_long) <- c(dv_name, factor_name)
  data_long[[factor_name]] <- factor(data_long[[factor_name]], levels = labels)

  group_stats <- data.frame(
    Group    = labels,
    n        = n,
    Mean     = sapply(group_data, mean),
    SD       = sapply(group_data, stats::sd),
    Variance = sapply(group_data, stats::var),
    Min      = sapply(group_data, min),
    Max      = sapply(group_data, max),
    stringsAsFactors = FALSE
  )

  anova_table    <- NULL
  effect_sizes   <- NULL
  assumptions    <- NULL
  posthoc_fisher <- NULL
  posthoc_tukey  <- NULL

  if (run_analysis) {
    N          <- sum(n)
    grand_mean <- sum(means * n) / N
    group_means <- sapply(group_data, mean)
    SSB  <- sum(n * (group_means - grand_mean)^2)
    SSW  <- sum(sapply(seq_len(k), function(i)
      sum((group_data[[i]] - group_means[i])^2)))
    SST  <- SSB + SSW
    dfB  <- k - 1;  dfW <- N - k;  dfT <- N - 1
    MSB  <- SSB / dfB;  MSW <- SSW / dfW
    Fstat <- MSB / MSW
    pval  <- stats::pf(Fstat, dfB, dfW, lower.tail = FALSE)

    anova_table <- data.frame(
      Source = c("Between", "Within", "Total"),
      SS = c(SSB, SSW, SST),
      df = c(dfB, dfW, dfT),
      MS = c(MSB, MSW, NA),
      F  = c(Fstat, NA, NA),
      p  = c(pval, NA, NA)
    )

    eta_sq   <- SSB / SST
    omega_sq <- (SSB - dfB * MSW) / (SST + MSW)
    effect_sizes <- list(
      eta_squared         = eta_sq,
      eta_squared_partial = SSB / (SSB + SSW),
      omega_squared       = omega_sq,
      omega_squared_partial = (dfB * (MSB - MSW)) / (SSB + (N - dfB) * MSW)
    )

    # Levene's test (median-based)
    group_medians <- sapply(group_data, stats::median)
    abs_devs <- lapply(seq_len(k), function(i) abs(group_data[[i]] - group_medians[i]))
    mean_ad  <- sapply(abs_devs, mean)
    gm_ad    <- sum(n * mean_ad) / N
    Fl <- (sum(n * (mean_ad - gm_ad)^2) / dfB) /
          (sum(sapply(seq_len(k), function(i) sum((abs_devs[[i]] - mean_ad[i])^2))) / dfW)
    pl <- stats::pf(Fl, dfB, dfW, lower.tail = FALSE)

    sw_results <- lapply(seq_len(k), function(i) {
      ni <- n[i]
      if (ni >= 3 && ni <= 5000) {
        sw <- stats::shapiro.test(group_data[[i]])
        list(W = as.numeric(sw$statistic), p = sw$p.value)
      } else list(W = NA, p = NA)
    })

    assumptions <- list(
      levene_F       = Fl,
      levene_p       = pl,
      shapiro_results = sw_results,
      variance_ratio  = max(sapply(group_data, stats::var)) /
                        min(sapply(group_data, stats::var))
    )

    # Post-hoc
    comps <- utils::combn(k, 2)
    nc    <- ncol(comps)
    t_crit <- stats::qt(0.975, dfW)
    q_crit <- stats::qtukey(0.95, k, dfW)

    fisher_df <- data.frame(Comparison = character(nc), Mean_Diff = numeric(nc),
                             LSD = numeric(nc), Significant = logical(nc),
                             stringsAsFactors = FALSE)
    tukey_df  <- data.frame(Comparison = character(nc), Mean_Diff = numeric(nc),
                             HSD = numeric(nc), Significant = logical(nc),
                             stringsAsFactors = FALSE)

    for (i in seq_len(nc)) {
      g1 <- comps[1, i];  g2 <- comps[2, i]
      md  <- abs(group_means[g1] - group_means[g2])
      lsd <- t_crit * sqrt(MSW * (1/n[g1] + 1/n[g2]))
      n_h <- 2 / (1/n[g1] + 1/n[g2])
      hsd <- q_crit * sqrt(MSW / n_h)
      fisher_df[i, ] <- list(paste(labels[g1], "vs", labels[g2]), md, lsd, md > lsd)
      tukey_df[i, ]  <- list(paste(labels[g1], "vs", labels[g2]), md, hsd, md > hsd)
    }
    posthoc_fisher <- fisher_df
    posthoc_tukey  <- tukey_df
  }

  if (export_csv) {
    if (output_format %in% c("wide", "both")) {
      fn <- paste0(dv_name, "_by_", factor_name, "_wide_dgen.csv")
      utils::write.csv(data_wide, file = fn, row.names = FALSE)
      message("Wide data saved to: ", file.path(getwd(), fn))
    }
    if (output_format %in% c("long", "both")) {
      fn <- paste0(dv_name, "_by_", factor_name, "_long_dgen.csv")
      utils::write.csv(data_long, file = fn, row.names = FALSE)
      message("Long data saved to: ", file.path(getwd(), fn))
    }
  }

  cat("One-way ANOVA data generated (k =", k, ", N =", sum(n), ")\n")
  print(group_stats[, c("Group", "n", "Mean", "SD")], row.names = FALSE,
        digits = decimals + 2)
  if (run_analysis && !is.null(anova_table)) {
    at <- anova_table
    at$SS <- round(at$SS, 2); at$MS <- round(at$MS, 2); at$F <- round(at$F, 3)
    at$p  <- format.pval(at$p, digits = 3, eps = 0.001)
    cat("\nANOVA table:\n"); print(at, row.names = FALSE)
    cat(sprintf("\nomega^2 = %.3f | eta^2 = %.3f\n",
                effect_sizes$omega_squared, effect_sizes$eta_squared))
  }

  result <- list(group_stats = group_stats, iterations = iterations)
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
  if (run_analysis) {
    result$anova_table    <- anova_table
    result$effect_sizes   <- effect_sizes
    result$assumptions    <- assumptions
    result$posthoc_fisher <- posthoc_fisher
    result$posthoc_tukey  <- posthoc_tukey
  }

  invisible(result)
}
