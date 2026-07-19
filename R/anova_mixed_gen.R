#' Generate Data for Mixed (Between x Within) ANOVA
#'
#' Creates data for a mixed-design (split-plot) ANOVA: subjects are
#' nested in independent between-subjects groups, and every subject is
#' measured at every level of a within-subjects factor — the classic
#' Group x Time design (e.g., Treatment vs. Control measured at Pre,
#' Post, and Follow-up). Uses iterative refinement with
#' \code{MASS::mvrnorm()} to achieve EXACT per-cell means and SDs while
#' maintaining a controlled within-subject correlation.
#'
#' The within-subject covariance is compound symmetric: one constant
#' correlation between every pair of within-subject measurements
#' (\code{within_subject_correlation}). Because compound symmetry
#' implies sphericity, the sphericity assumption is satisfied by
#' construction and no Mauchly correction is needed — the assumptions
#' output says so explicitly. Group sizes are equal by design
#' (\code{n_per_group} subjects in every group).
#'
#' The ANOVA uses the standard split-plot error partition: the
#' between-subjects effect is tested against subjects-within-groups,
#' while the within-subjects effect and the interaction are tested
#' against the within-subjects error (the Time x Subjects-within-groups
#' term). Following [anova_twoway_gen()]'s convention, Tukey HSD
#' post-hoc comparisons on the marginal means are computed only when the
#' interaction is not significant (each using its own error term); a
#' significant interaction calls for simple-effects follow-up instead.
#'
#' @param n_per_group Subjects per between-subjects group (minimum 2;
#'   equal across groups)
#' @param between_levels Number of between-subjects groups (minimum 2)
#' @param between_labels Character vector of group names (length
#'   \code{between_levels})
#' @param between_name Name for the between-subjects factor
#'   (default = \code{"Group"})
#' @param within_levels Number of within-subjects measurements
#'   (minimum 2)
#' @param within_labels Character vector of measurement names (length
#'   \code{within_levels})
#' @param within_name Name for the within-subjects factor
#'   (default = \code{"Time"})
#' @param cell_means Matrix of target means (between_levels x
#'   within_levels; rows = groups, columns = measurements)
#' @param within_cell_sd Within-cell standard deviation(s). Either a
#'   single value or a matrix matching \code{cell_means} dimensions
#' @param within_subject_correlation Correlation between any two
#'   measurements on the same subject (default = 0.70). Must exceed
#'   \code{-1/(within_levels - 1)} for a valid covariance matrix
#' @param dv_name Name for the dependent variable (default =
#'   \code{"Score"})
#' @param id_name Name for the subject identifier (default =
#'   \code{"ID"})
#' @param decimals Number of decimal places for rounding the DV
#'   (default = 2)
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination.
#'   When \code{output_format = "both"} and a file path is supplied,
#'   \code{_wide}/\code{_long} is inserted before the extension
#' @param output_format \code{"wide"}, \code{"long"}, or \code{"both"}
#'   (default = \code{"long"})
#' @param run_analysis Logical; run the mixed ANOVA automatically?
#'   (default = \code{TRUE})
#'
#' @return A list with:
#'   \item{data}{Primary data frame (long or wide, per
#'     \code{output_format})}
#'   \item{data_wide}{One row per subject: ID, group, one column per
#'     measurement}
#'   \item{data_long}{One row per observation: ID, group, measurement,
#'     score}
#'   \item{cell_stats}{Descriptive statistics for each group x
#'     measurement cell}
#'   \item{marginal_means}{Between-group and within-measurement marginal
#'     means}
#'   \item{anova_table}{Split-plot ANOVA table with both error terms
#'     (if \code{run_analysis = TRUE})}
#'   \item{effect_sizes}{Eta-squared, partial eta-squared, and partial
#'     omega-squared for each effect, each using its own error term}
#'   \item{assumptions}{Levene's test on subject means across groups,
#'     cell-variance ratio, and the sphericity-by-construction note}
#'   \item{posthoc_between}{Tukey HSD for group marginal means (when
#'     the interaction is not significant, the effect is significant,
#'     and there are more than 2 groups)}
#'   \item{posthoc_within}{Tukey HSD for measurement marginal means
#'     (same conditions, within-subjects error term)}
#'   \item{correlation_matrix}{Average achieved within-subject
#'     correlation matrix}
#'   \item{iterations}{Refinement iterations per group}
#'
#' @examples
#' # Small 2 x 2 design that converges quickly
#' quick <- anova_mixed_gen(
#'   n_per_group    = 5,
#'   between_levels = 2,
#'   between_labels = c("Treatment", "Control"),
#'   within_levels  = 2,
#'   within_labels  = c("Pre", "Post"),
#'   cell_means     = matrix(c(10, 16,
#'                             10, 11), nrow = 2, byrow = TRUE),
#'   within_cell_sd = 2,
#'   seed           = 42
#' )
#' quick$anova_table
#'
#' \donttest{
#' # Classic therapy-outcome design: 2 groups x 3 time points. The
#' # iterative refinement needed to hit exact cell statistics under the
#' # within-subject correlation constraint can take noticeably longer
#' # for larger designs.
#' result <- anova_mixed_gen(
#'   n_per_group    = 8,
#'   between_levels = 2,
#'   between_labels = c("CBT", "Waitlist"),
#'   within_levels  = 3,
#'   within_labels  = c("Baseline", "Week6", "Week12"),
#'   cell_means     = matrix(c(25, 18, 14,
#'                             25, 24, 23), nrow = 2, byrow = TRUE),
#'   within_cell_sd = 4,
#'   within_subject_correlation = 0.60,
#'   dv_name        = "Anxiety",
#'   seed           = 42
#' )
#' result$anova_table
#' result$effect_sizes$interaction
#' }
#'
#' @concept mixed ANOVA
#' @concept split-plot design
#' @concept between-within design
#' @concept repeated measures ANOVA
#'
#' @export
anova_mixed_gen <- function(n_per_group,
                            between_levels,
                            between_labels,
                            between_name  = "Group",
                            within_levels,
                            within_labels,
                            within_name   = "Time",
                            cell_means,
                            within_cell_sd,
                            within_subject_correlation = 0.70,
                            dv_name       = "Score",
                            id_name       = "ID",
                            decimals      = 2,
                            seed          = NULL,
                            export_csv    = FALSE,
                            output_format = "long",
                            run_analysis  = TRUE) {

  if (!is.numeric(n_per_group) || n_per_group < 2 ||
      n_per_group != round(n_per_group))
    stop("'n_per_group' must be an integer >= 2")
  if (!is.numeric(between_levels) || between_levels < 2 ||
      between_levels != round(between_levels))
    stop("'between_levels' must be an integer >= 2")
  if (!is.numeric(within_levels) || within_levels < 2 ||
      within_levels != round(within_levels))
    stop("'within_levels' must be an integer >= 2")
  if (length(between_labels) != between_levels)
    stop("Length of 'between_labels' must equal 'between_levels'")
  if (length(within_labels) != within_levels)
    stop("Length of 'within_labels' must equal 'within_levels'")
  if (!is.matrix(cell_means) || nrow(cell_means) != between_levels ||
      ncol(cell_means) != within_levels)
    stop("'cell_means' must be a matrix with dimensions ",
         between_levels, " x ", within_levels)
  if (!is.numeric(within_subject_correlation) ||
      within_subject_correlation >= 1 ||
      within_subject_correlation <= -1 / (within_levels - 1))
    stop("'within_subject_correlation' must be less than 1 and greater ",
         "than -1/(within_levels - 1) = ",
         round(-1 / (within_levels - 1), 3),
         " for a valid covariance matrix")
  if (!output_format %in% c("wide", "long", "both"))
    stop("'output_format' must be 'wide', 'long', or 'both'")

  if (length(within_cell_sd) == 1) {
    within_cell_sd <- matrix(within_cell_sd, nrow = between_levels,
                             ncol = within_levels)
  }
  if (!is.matrix(within_cell_sd) || nrow(within_cell_sd) != between_levels ||
      ncol(within_cell_sd) != within_levels)
    stop("'within_cell_sd' dimensions must match 'cell_means'")
  if (any(within_cell_sd <= 0)) stop("All SD values must be positive")

  if (!is.null(seed)) set.seed(seed)

  a <- between_levels
  k <- within_levels
  n <- n_per_group
  n_subj <- a * n
  N      <- n_subj * k

  target_means <- round(cell_means,     decimals + 1)
  target_sds   <- round(within_cell_sd, decimals + 1)
  target_cor   <- round(within_subject_correlation, 3)
  max_iter     <- 100000

  group_data_list   <- vector("list", a)
  iterations_vector <- integer(a)

  for (g in seq_len(a)) {
    iteration <- 0L

    repeat {
      iteration <- iteration + 1L

      Sigma_cor <- matrix(within_subject_correlation, nrow = k, ncol = k)
      diag(Sigma_cor) <- 1
      D         <- diag(within_cell_sd[g, ], nrow = k)
      Sigma_cov <- D %*% Sigma_cor %*% D

      raw_data <- MASS::mvrnorm(n = n, mu = cell_means[g, ],
                                Sigma = Sigma_cov)

      std_data <- apply(raw_data, 2, function(x) (x - mean(x)) / stats::sd(x))
      rescaled <- matrix(0, nrow = n, ncol = k)
      for (j in seq_len(k)) {
        rescaled[, j] <- std_data[, j] * target_sds[g, j] +
                         target_means[g, j]
      }
      grp_data <- round(rescaled, decimals)

      ach_means <- apply(grp_data, 2,
                         function(x) round(mean(x), decimals + 1))
      ach_sds   <- apply(grp_data, 2,
                         function(x) round(stats::sd(x), decimals + 1))
      cm        <- stats::cor(grp_data)
      ach_cor   <- round(mean(cm[lower.tri(cm)]), 3)

      if (all(ach_means == target_means[g, ]) &&
          all(ach_sds   == target_sds[g, ]) &&
          ach_cor == target_cor) break

      if (iteration >= max_iter) {
        warning("Max iterations reached for group '", between_labels[g], "'")
        break
      }
    }

    group_data_list[[g]]  <- grp_data
    iterations_vector[g]  <- iteration
  }
  names(iterations_vector) <- between_labels

  # Wide format: one row per subject
  data_wide <- data.frame(
    id  = seq_len(n_subj),
    grp = rep(between_labels, each = n),
    stringsAsFactors = FALSE
  )
  for (j in seq_len(k)) {
    data_wide[[within_labels[j]]] <-
      unlist(lapply(group_data_list, function(gd) gd[, j]))
  }
  colnames(data_wide)[1:2] <- c(id_name, between_name)
  data_wide[[between_name]] <- factor(data_wide[[between_name]],
                                      levels = between_labels)

  # Long format: one row per observation
  data_long <- data.frame(
    id    = rep(seq_len(n_subj), times = k),
    grp   = rep(rep(between_labels, each = n), times = k),
    within = rep(within_labels, each = n_subj),
    dv    = unlist(lapply(seq_len(k), function(j)
      unlist(lapply(group_data_list, function(gd) gd[, j])))),
    stringsAsFactors = FALSE
  )
  colnames(data_long) <- c(id_name, between_name, within_name, dv_name)
  data_long[[between_name]] <- factor(data_long[[between_name]],
                                      levels = between_labels)
  data_long[[within_name]]  <- factor(data_long[[within_name]],
                                      levels = within_labels)

  # Cell statistics
  cell_stats <- data.frame(
    G = rep(between_labels, each = k),
    W = rep(within_labels, times = a),
    n = n,
    Mean = numeric(a * k), SD = numeric(a * k), Variance = numeric(a * k),
    stringsAsFactors = FALSE
  )
  colnames(cell_stats)[1:2] <- c(between_name, within_name)
  idx <- 1L
  for (g in seq_len(a)) {
    for (j in seq_len(k)) {
      x <- group_data_list[[g]][, j]
      cell_stats$Mean[idx]     <- mean(x)
      cell_stats$SD[idx]       <- stats::sd(x)
      cell_stats$Variance[idx] <- stats::var(x)
      idx <- idx + 1L
    }
  }

  # Achieved within-subject correlation matrix (average across groups)
  cor_mats <- lapply(group_data_list, function(gd) {
    m <- stats::cor(gd)
    rownames(m) <- colnames(m) <- within_labels
    m
  })
  correlation_matrix <- Reduce("+", cor_mats) / a

  # Marginal means
  cell_mean_mat <- matrix(cell_stats$Mean, nrow = a, ncol = k, byrow = TRUE)
  marginal_between <- rowMeans(cell_mean_mat)
  marginal_within  <- colMeans(cell_mean_mat)
  names(marginal_between) <- between_labels
  names(marginal_within)  <- within_labels
  marginal_means <- list(between = marginal_between,
                         within  = marginal_within)

  anova_table     <- NULL
  effect_sizes    <- NULL
  assumptions     <- NULL
  posthoc_between <- NULL
  posthoc_within  <- NULL

  if (run_analysis) {
    grand_mean <- mean(data_long[[dv_name]])

    # Subject means (rows of each group matrix)
    subj_means <- lapply(group_data_list, rowMeans)

    # Split-plot sums of squares
    SS_A   <- n * k * sum((marginal_between - grand_mean)^2)
    SS_swg <- k * sum(unlist(lapply(seq_len(a), function(g)
      (subj_means[[g]] - marginal_between[g])^2)))
    SS_T   <- a * n * sum((marginal_within - grand_mean)^2)
    SS_AT  <- n * sum((cell_mean_mat -
                         outer(marginal_between, rep(1, k)) -
                         outer(rep(1, a), marginal_within) +
                         grand_mean)^2)
    SST    <- sum((data_long[[dv_name]] - grand_mean)^2)
    SS_wserr <- SST - SS_A - SS_swg - SS_T - SS_AT

    df_A     <- a - 1
    df_swg   <- a * (n - 1)
    df_T     <- k - 1
    df_AT    <- (a - 1) * (k - 1)
    df_wserr <- a * (n - 1) * (k - 1)
    df_total <- N - 1

    MS_A     <- SS_A     / df_A
    MS_swg   <- SS_swg   / df_swg
    MS_T     <- SS_T     / df_T
    MS_AT    <- SS_AT    / df_AT
    MS_wserr <- SS_wserr / df_wserr

    F_A  <- MS_A  / MS_swg
    F_T  <- MS_T  / MS_wserr
    F_AT <- MS_AT / MS_wserr
    p_A  <- stats::pf(F_A,  df_A,  df_swg,   lower.tail = FALSE)
    p_T  <- stats::pf(F_T,  df_T,  df_wserr, lower.tail = FALSE)
    p_AT <- stats::pf(F_AT, df_AT, df_wserr, lower.tail = FALSE)

    interaction_label <- paste(between_name, "x", within_name)
    anova_table <- data.frame(
      Source = c(between_name, "Subjects within Groups",
                 within_name, interaction_label,
                 "Within-Subjects Error", "Total"),
      SS = c(SS_A, SS_swg, SS_T, SS_AT, SS_wserr, SST),
      df = c(df_A, df_swg, df_T, df_AT, df_wserr, df_total),
      MS = c(MS_A, MS_swg, MS_T, MS_AT, MS_wserr, NA),
      F  = c(F_A, NA, F_T, F_AT, NA, NA),
      p  = c(p_A, NA, p_T, p_AT, NA, NA),
      stringsAsFactors = FALSE
    )

    es_one <- function(SS_eff, df_eff, MS_eff, SS_err, MS_err) {
      list(
        eta_squared           = SS_eff / SST,
        eta_squared_partial   = SS_eff / (SS_eff + SS_err),
        omega_squared_partial = (df_eff * (MS_eff - MS_err)) /
                                (SS_eff + (N - df_eff) * MS_err)
      )
    }
    effect_sizes <- list(
      between     = es_one(SS_A,  df_A,  MS_A,  SS_swg,   MS_swg),
      within      = es_one(SS_T,  df_T,  MS_T,  SS_wserr, MS_wserr),
      interaction = es_one(SS_AT, df_AT, MS_AT, SS_wserr, MS_wserr)
    )

    # Levene's test (median-based) on subject means across groups —
    # homogeneity for the between-subjects test
    subj_medians <- sapply(subj_means, stats::median)
    abs_devs <- lapply(seq_len(a), function(g)
      abs(subj_means[[g]] - subj_medians[g]))
    mean_abs_dev <- sapply(abs_devs, mean)
    grand_mad    <- mean(unlist(abs_devs))
    SSB_lev <- n * sum((mean_abs_dev - grand_mad)^2)
    SSW_lev <- sum(sapply(abs_devs, function(x) sum((x - mean(x))^2)))
    F_lev   <- (SSB_lev / (a - 1)) / (SSW_lev / (n_subj - a))
    p_lev   <- stats::pf(F_lev, a - 1, n_subj - a, lower.tail = FALSE)
    var_ratio <- max(cell_stats$Variance) / min(cell_stats$Variance)

    assumptions <- list(
      levene_F       = F_lev,
      levene_p       = p_lev,
      variance_ratio = var_ratio,
      sphericity     = paste("Compound symmetry imposed by construction;",
                             "sphericity is satisfied and no Mauchly",
                             "correction is needed")
    )

    # Post-hoc tests (main effects only when interaction NOT significant)
    if (p_AT >= 0.05) {
      if (p_A < 0.05 && a > 2) {
        q_crit <- stats::qtukey(0.95, a, df_swg)
        comps  <- utils::combn(a, 2)
        posthoc_between <- data.frame(
          Comparison = paste(between_labels[comps[1, ]], "vs",
                             between_labels[comps[2, ]]),
          Mean_Diff  = abs(marginal_between[comps[1, ]] -
                           marginal_between[comps[2, ]]),
          HSD        = q_crit * sqrt(MS_swg / (n * k)),
          stringsAsFactors = FALSE
        )
        posthoc_between$Significant <-
          posthoc_between$Mean_Diff > posthoc_between$HSD
      }
      if (p_T < 0.05 && k > 2) {
        q_crit <- stats::qtukey(0.95, k, df_wserr)
        comps  <- utils::combn(k, 2)
        posthoc_within <- data.frame(
          Comparison = paste(within_labels[comps[1, ]], "vs",
                             within_labels[comps[2, ]]),
          Mean_Diff  = abs(marginal_within[comps[1, ]] -
                           marginal_within[comps[2, ]]),
          HSD        = q_crit * sqrt(MS_wserr / (a * n)),
          stringsAsFactors = FALSE
        )
        posthoc_within$Significant <-
          posthoc_within$Mean_Diff > posthoc_within$HSD
      }
    }
  }

  dgen_export_pair(data_wide, data_long, output_format, export_csv,
                   paste0(dv_name, "_mixed_wide_dgen.csv"),
                   paste0(dv_name, "_mixed_long_dgen.csv"))

  message(sprintf(
    "anova_mixed_gen: %d between x %d within, n_per_group = %d, %d subjects (%d observations)",
    a, k, n, n_subj, N))
  if (run_analysis) {
    message(sprintf("%s: F(%d,%d) = %.3f, p = %s",
                    between_name, df_A, df_swg, F_A,
                    format.pval(p_A, digits = 3, eps = 0.001)))
    message(sprintf("%s: F(%d,%d) = %.3f, p = %s",
                    within_name, df_T, df_wserr, F_T,
                    format.pval(p_T, digits = 3, eps = 0.001)))
    message(sprintf("%s: F(%d,%d) = %.3f, p = %s",
                    paste(between_name, "x", within_name),
                    df_AT, df_wserr, F_AT,
                    format.pval(p_AT, digits = 3, eps = 0.001)))
  }

  result <- list(
    cell_stats         = cell_stats,
    marginal_means     = marginal_means,
    correlation_matrix = correlation_matrix,
    iterations         = iterations_vector
  )
  result$data      <- if (output_format == "wide") data_wide else data_long
  result$data_wide <- data_wide
  result$data_long <- data_long
  if (run_analysis) {
    result$anova_table     <- anova_table
    result$effect_sizes    <- effect_sizes
    result$assumptions     <- assumptions
    result$posthoc_between <- posthoc_between
    result$posthoc_within  <- posthoc_within
  }

  invisible(result)
}
