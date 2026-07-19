# Internal helpers shared by the nonparametric family
# (sign_gen, mannwhitney_gen, signedrank_gen, kruskal_gen, friedman_gen,
# spearman_gen). Not exported.

# Validate that a target median is achievable for the given rounding
# grid, sample size parity, and optional bounds. With decimals d the
# data live on a grid of step 10^-d; an odd-n sample median must sit on
# that grid, an even-n median on the half-step grid.
npar_validate_median <- function(target, n, decimals, scale_min, scale_max,
                                 what = "median") {
  step <- 10^(-decimals)
  unit <- if (n %% 2 == 0) step / 2 else step
  if (abs(target / unit - round(target / unit)) > 1e-8)
    stop("Target ", what, " ", target, " is not achievable with n = ", n,
         " and decimals = ", decimals, ": with ",
         if (n %% 2 == 0) "even" else "odd", " n the sample ", what,
         " must be a multiple of ", unit)
  if (!is.null(scale_min) && target <= scale_min)
    stop("Target ", what, " ", target, " must exceed scale_min (",
         scale_min, ")")
  if (!is.null(scale_max) && target >= scale_max)
    stop("Target ", what, " ", target, " must be below scale_max (",
         scale_max, ")")
  invisible(TRUE)
}

# Shared validation of the spread / shape / severity / bounds arguments.
npar_validate_common <- function(spread, shape, severity, scale_min,
                                 scale_max, decimals) {
  if (!is.numeric(spread) || length(spread) != 1 || spread <= 0)
    stop("'spread' must be a single positive number (an SD-like scale)")
  shapes <- c("normal", "right_skew", "left_skew", "heavy_tails",
              "light_tails", "bimodal")
  if (!shape %in% shapes)
    stop("'shape' must be one of: ", paste(shapes, collapse = ", "))
  if (!severity %in% c("mild", "moderate", "severe"))
    stop("'severity' must be 'mild', 'moderate', or 'severe'")
  if (!is.null(scale_min) && !is.null(scale_max) && scale_min >= scale_max)
    stop("'scale_min' must be below 'scale_max'")
  if (!is.numeric(decimals) || decimals < 0 || decimals != round(decimals))
    stop("'decimals' must be a non-negative integer")
  invisible(TRUE)
}

# Draw one sample with an EXACT median: shaped draw, standardized,
# scaled by 'spread', shifted so the continuous median equals the
# target, rounded, clamped to bounds, iterating until the rounded
# (and clamped) sample median still equals the target exactly.
npar_sample <- function(n, target_median, spread, draw_fun, decimals,
                        scale_min = NULL, scale_max = NULL,
                        max_iter = 100000) {
  for (iter in seq_len(max_iter)) {
    x <- draw_fun(n)
    x <- as.vector(scale(x)) * spread
    x <- x - stats::median(x) + target_median
    x <- round(x, decimals)
    if (!is.null(scale_min)) x <- pmax(x, scale_min)
    if (!is.null(scale_max)) x <- pmin(x, scale_max)
    if (round(stats::median(x), decimals + 2) ==
        round(target_median, decimals + 2))
      return(list(x = x, iterations = iter))
  }
  warning("Maximum iterations reached; the sample median may not ",
          "exactly match the target")
  list(x = x, iterations = max_iter)
}

# Draw an n x k matrix of correlated standardized shape draws
# (compound-symmetry-like: every column shares the same base component).
npar_correlated_matrix <- function(n, k, correlation, draw_fun) {
  base <- as.vector(scale(draw_fun(n)))
  sapply(seq_len(k), function(j) {
    correlation * base +
      sqrt(1 - correlation^2) * as.vector(scale(draw_fun(n)))
  })
}

# Draw correlated columns and shift each to an exact per-column median,
# iterating jointly until every column's rounded median is exact.
npar_correlated_sample <- function(n, medians, spread, correlation,
                                   draw_fun, decimals,
                                   scale_min = NULL, scale_max = NULL,
                                   max_iter = 100000) {
  k <- length(medians)
  for (iter in seq_len(max_iter)) {
    m <- npar_correlated_matrix(n, k, correlation, draw_fun)
    for (j in seq_len(k)) {
      xj <- m[, j] * spread
      xj <- xj - stats::median(xj) + medians[j]
      xj <- round(xj, decimals)
      if (!is.null(scale_min)) xj <- pmax(xj, scale_min)
      if (!is.null(scale_max)) xj <- pmin(xj, scale_max)
      m[, j] <- xj
    }
    ok <- all(vapply(seq_len(k), function(j)
      round(stats::median(m[, j]), decimals + 2) ==
        round(medians[j], decimals + 2), logical(1)))
    if (ok) return(list(m = m, iterations = iter))
  }
  warning("Maximum iterations reached; sample medians may not exactly ",
          "match the targets")
  list(m = m, iterations = max_iter)
}

# Convert a (two-sided) p-value into the |z| the rest of the world
# reports alongside it, signed by the observed direction of effect.
npar_z_from_p <- function(p, direction) {
  z <- stats::qnorm(p / 2, lower.tail = FALSE)
  z * sign(direction)
}

# Dunn's post-hoc test for a significant Kruskal-Wallis: pairwise z
# tests on mean ranks with the tie-corrected standard error and Holm
# adjustment. 'values' is the numeric vector, 'groups' a factor.
npar_dunn <- function(values, groups) {
  N     <- length(values)
  rk    <- rank(values)
  lv    <- levels(groups)
  ni    <- tapply(rk, groups, length)
  rbar  <- tapply(rk, groups, mean)
  ties  <- table(values)
  tie_term <- sum(ties^3 - ties) / (12 * (N - 1))
  base_var <- N * (N + 1) / 12 - tie_term
  comps <- utils::combn(length(lv), 2)
  z <- p_raw <- numeric(ncol(comps))
  lab <- character(ncol(comps))
  for (i in seq_len(ncol(comps))) {
    g1 <- comps[1, i]; g2 <- comps[2, i]
    se <- sqrt(base_var * (1 / ni[g1] + 1 / ni[g2]))
    z[i]     <- (rbar[g1] - rbar[g2]) / se
    p_raw[i] <- 2 * stats::pnorm(-abs(z[i]))
    lab[i]   <- paste(lv[g1], "vs", lv[g2])
  }
  data.frame(Comparison = lab,
             z          = z,
             p_raw      = p_raw,
             p_holm     = stats::p.adjust(p_raw, method = "holm"),
             stringsAsFactors = FALSE)
}
