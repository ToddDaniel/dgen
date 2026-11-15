# The exact engine: data whose means, SDs and correlations match the targets
# after rounding. Every dgen generator calls it.
# Method: take a random draw, remove its own means, SDs and correlations,
# build in the targets, round, repair each mean, and redraw until the rounded
# data also match every SD and correlation.
# Method inspired by Morse (2012), MorseGen.

# n rows of k variables (k = length(m)). `r` is one correlation for two
# variables or a k x k correlation table. Returns the data as a matrix and
# the number of tries it took.
# The try limit is a fixed amount of work, so giving up takes about 3
# seconds whatever the size of the request: about 73,000 tries for 15 rows of
# 3 variables, 13,000 for 500 rows of 5. Small samples are quick to try and
# are the ones that can need many tries.
exact_mv <- function(n, m, sd, r = NULL, decimals = 2, r_digits = 2,
                     max_iter = round(4e7 / (500 + n * length(m)), -3)) {

  # 1. Check inputs, then check that the targets can be reached at all
  target_r     <- check_targets(n, m, sd, r, decimals, r_digits)
  target_total <- check_feasible(n, m, sd, decimals)

  # 2. Work in rounding units, so every rounded value is a whole number
  #    (with decimals = 1, a value of 75.4 is 754 units)
  n_variables   <- length(m)
  units_per_one <- 10^decimals
  mean_units    <- m * units_per_one
  sd_units      <- sd * units_per_one

  # Multiplying clean columns (SDs of 1, correlations of 0) by the Cholesky
  # factor of the target table gives exactly the target correlations; the
  # second factor then stretches each column to its target SD.
  add_correlations <- if (n_variables > 1) chol(target_r) else diag(1)
  add_targets      <- add_correlations %*% diag(sd_units, n_variables)

  # 3. Draw, make exact, round, repair, check; redraw if a check fails
  for (tries in seq_len(max_iter)) {
    draw <- matrix(stats::rnorm(n * n_variables), n, n_variables)
    draw <- draw - rep(colMeans(draw), each = n)

    # Whitening: dividing by the Cholesky factor of the draw's own covariance
    # leaves every column with SD exactly 1 and every correlation exactly 0.
    # backsolve() does that division and adds the targets in one step.
    own_covariance <- chol(crossprod(draw) / (n - 1))
    exact   <- draw %*% backsolve(own_covariance, add_targets) +
      rep(mean_units, each = n)
    rounded <- round(exact)
    for (j in seq_len(n_variables)) {
      rounded[, j] <- repair_total(rounded[, j], exact[, j], target_total[j])
    }

    if (hits_targets(rounded, sd_units, target_r, r_digits)) {
      return(list(data  = round(rounded / units_per_one, decimals),
                  tries = tries))
    }
  }

  # 4. Nothing found: say so, with advice
  stop("dgen could not find data matching every target after ",
       format(max_iter, big.mark = ","), " tries. The rounding is probably ",
       "too coarse for these targets. Try more decimals",
       if (n_variables > 1) ", a larger n, or fewer variables."
       else " or a larger n.",
       call. = FALSE)
}

# Mean repair. Rounding leaves the column total a few units off. Move the
# fewest values by one unit each, choosing those that were closest to
# rounding the other way, so the total (and so the mean) is exact.
repair_total <- function(rounded, exact, target_total) {
  shortfall <- target_total - sum(rounded)
  if (shortfall == 0) return(rounded)

  direction       <- sign(shortfall)
  rounding_error  <- (exact - rounded) * direction
  nearly_the_other_way <- order(rounding_error, decreasing = TRUE)
  to_move         <- nearly_the_other_way[seq_len(abs(shortfall))]
  rounded[to_move] <- rounded[to_move] + direction
  rounded
}

# TRUE when every SD and correlation of the rounded data reports as its
# target. Means are already exact after repair_total().
hits_targets <- function(rounded, sd_units, target_r, r_digits) {
  n <- nrow(rounded)
  deviations  <- rounded - rep(colMeans(rounded), each = n)
  achieved_sd <- sqrt(colSums(deviations^2) / (n - 1))
  if (any(abs(achieved_sd - sd_units) >= half_step - tie_margin)) {
    return(FALSE)
  }
  if (is.null(target_r)) return(TRUE)

  pairs      <- upper.tri(target_r)
  achieved_r <- stats::cor(rounded)
  all(abs(achieved_r[pairs] - target_r[pairs]) <
        0.5 * 10^-r_digits - tie_margin)
}

# ---- Independent groups ------------------------------------------------------

# One exact variable per group, generated separately, for t tests and ANOVA.
# `n` is one size for every group or one size per group. Returns a list with
# one vector of values per group. `part` is what a group is called in
# messages ("Group 2: ...").
exact_groups <- function(n, m, sd, decimals = 2, part = "Group") {

  # 1. One mean and one SD per group
  n_groups <- length(m)
  if (!is_number(m) || !is_number(sd) || length(sd) != n_groups) {
    stop("m and sd must be numbers, with one SD for each mean. You gave ",
         length(m), " mean(s) and ", length(sd), " SD(s).", call. = FALSE)
  }
  if (!is_number(n) || !(length(n) %in% c(1, n_groups))) {
    stop("n must be one sample size for all groups, or one for each of the ",
         n_groups, " groups.", call. = FALSE)
  }
  n <- rep_len(n, n_groups)

  # 2. Each group is its own one-variable problem; a message from the engine
  #    is passed on with the group it belongs to
  lapply(seq_len(n_groups), function(group) {
    tryCatch(
      exact_mv(n[group], m[group], sd[group], decimals = decimals)$data[, 1],
      error = function(problem) {
        stop(if (n_groups > 1) paste0(part, " ", group, ": "),
             conditionMessage(problem), call. = FALSE)
      }
    )
  })
}

# ---- Categories ---------------------------------------------------------------

# Turns counts into raw data: one row per observation, in random order.
# A vector of counts gives one column; a table (matrix) of counts gives two.
# Counts are reproduced exactly because nothing is drawn at random except
# the order of the rows.
exact_counts <- function(counts) {

  # 1. Check the counts
  if (!is_whole(counts) || any(counts < 0) || sum(counts) < 1) {
    stop("counts must be whole numbers of 0 or more, such as ",
         "c(Yes = 56, No = 24).", call. = FALSE)
  }

  # 2. Repeat each category (or each cell of the table) count times
  if (is.matrix(counts)) {
    row_labels    <- category_labels(rownames(counts), nrow(counts))
    column_labels <- category_labels(colnames(counts), ncol(counts))
    cell   <- rep(seq_along(counts), times = as.vector(counts))
    in_row <- as.vector(row(counts))[cell]
    in_col <- as.vector(col(counts))[cell]
    data <- data.frame(row    = factor(row_labels[in_row], row_labels),
                       column = factor(column_labels[in_col], column_labels))
  } else {
    labels <- category_labels(names(counts), length(counts))
    data <- data.frame(category = factor(rep(labels, times = counts), labels))
  }

  # 3. Shuffle the rows
  data <- data[sample.int(nrow(data)), , drop = FALSE]
  rownames(data) <- NULL
  data
}

category_labels <- function(labels, n_categories) {
  if (is.null(labels)) return(as.character(seq_len(n_categories)))
  if (anyDuplicated(labels) || any(is.na(labels)) || any(labels == "")) {
    stop("Every category needs its own name.", call. = FALSE)
  }
  labels
}
