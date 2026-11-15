# The exact engine: data whose means, SDs and correlations match the targets
# after rounding. Every dgen generator calls it.
# Method: take a random draw, remove its own means, SDs and correlations,
# build in the targets, round, repair each mean, and redraw until the rounded
# data also match every SD and correlation.
# Method inspired by Morse (2012), MorseGen.

# n rows of k variables (k = length(m)). `r` is one correlation for two
# variables or a k x k correlation table. Returns the data as a matrix and
# the number of tries it took.
exact_mv <- function(n, m, sd, r = NULL, decimals = 2, r_digits = 2,
                     max_iter = 10000) {

  # 1. Check inputs, then check that the targets can be reached at all
  target_r     <- check_targets(n, m, sd, r, decimals, r_digits)
  target_total <- check_feasible(n, m, sd, decimals)

  # 2. Work in rounding units, so every rounded value is a whole number
  #    (with decimals = 1, a value of 75.4 is 754 units)
  n_variables   <- length(m)
  units_per_one <- 10^decimals
  mean_units    <- m * units_per_one
  sd_units      <- sd * units_per_one
  add_correlations <- if (n_variables > 1) chol(target_r) else diag(1)

  # 3. Draw, make exact, round, repair, check; redraw if a check fails
  for (tries in seq_len(max_iter)) {
    draw <- matrix(stats::rnorm(n * n_variables), n, n_variables)
    draw <- sweep(draw, 2, colMeans(draw))

    # Whitening: dividing by the Cholesky factor of the draw's own covariance
    # leaves every column with SD exactly 1 and every correlation exactly 0.
    draw <- draw %*% solve(chol(crossprod(draw) / (n - 1)))

    # With SDs of 1 and correlations of 0, multiplying by the Cholesky factor
    # of the target table gives exactly the target correlations.
    draw <- draw %*% add_correlations

    exact   <- sweep(sweep(draw, 2, sd_units, "*"), 2, mean_units, "+")
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
  achieved_sd <- apply(rounded, 2, stats::sd)
  if (any(abs(achieved_sd - sd_units) >= half_step - tie_margin)) {
    return(FALSE)
  }
  if (is.null(target_r)) return(TRUE)

  pairs      <- upper.tri(target_r)
  achieved_r <- stats::cor(rounded)
  all(abs(achieved_r[pairs] - target_r[pairs]) <
        0.5 * 10^-r_digits - tie_margin)
}
