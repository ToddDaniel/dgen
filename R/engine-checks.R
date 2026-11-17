# Checks that run before the engine generates anything.
# check_targets() makes sure the request is well formed; check_feasible()
# makes sure rounded data can actually reach each mean and SD. Both stop at
# once with a plain-language message, so impossible requests never loop.

# A statistic matches its target when it is closer than half a step of the
# reported precision (decimals + 1), which is 0.05 rounding units. Sitting
# exactly on the half is a tie and does not count.
half_step  <- 0.05
tie_margin <- 1e-9

# ---- 1. Is the request well formed? ---------------------------------------

check_targets <- function(n, m, sd, r, decimals, r_digits,
                          derived_means = FALSE) {

  # 1. Sample size and rounding
  if (!is_whole(n) || length(n) != 1 || n < 1) {
    stop("n (the sample size) must be a single whole number, such as 30.",
         call. = FALSE)
  }
  check_decimals(decimals)

  # 2. Means and SDs
  if (!is_number(m) || !is_number(sd) || length(m) != length(sd)) {
    stop("m and sd must be numbers, with one SD for each mean. You gave ",
         length(m), " mean(s) and ", length(sd), " SD(s).", call. = FALSE)
  }
  if (any(sd <= 0)) {
    stop("Every SD must be greater than 0.", call. = FALSE)
  }
  n_variables <- length(m)
  if (n < n_variables + 2) {
    if (n_variables == 1) stop("n must be at least 3.", call. = FALSE)
    stop("You asked for ", n_variables, " variables but only ", n, " rows. ",
         "n must be at least ", n_variables + 2, " for ", n_variables,
         " variables.", call. = FALSE)
  }

  # 3. Targets cannot be finer than the precision they are checked at
  if (!derived_means) check_precision(m, decimals + 1, decimals, "means")
  check_precision(sd, decimals + 1, decimals, "SDs")

  # 4. Correlations
  if (n_variables == 1) {
    if (!is.null(r)) {
      stop("A correlation needs at least two variables.", call. = FALSE)
    }
    return(NULL)
  }
  check_correlations(r, n_variables, r_digits)
}

check_decimals <- function(decimals) {
  if (!is_whole(decimals) || length(decimals) != 1 ||
      decimals < 0 || decimals > 6) {
    stop("decimals must be a whole number from 0 to 6. Use 0 for whole-number ",
         "data.", call. = FALSE)
  }
}

check_precision <- function(targets, digits, decimals, what) {
  too_fine <- abs(targets * 10^digits - round(targets * 10^digits)) > 1e-6
  if (!any(too_fine)) return(invisible(TRUE))

  asked <- targets[too_fine][1]
  below <- floor(asked * 10^digits) / 10^digits
  above <- ceiling(asked * 10^digits) / 10^digits
  stop("With decimals = ", decimals, ", ", what, " are matched to ", digits,
       " decimal place", if (digits > 1) "s", ", but you asked for ",
       format(asked), ". Use ",
       if (below > 0 || what == "means") paste(show_number(below, digits), "or "),
       show_number(above, digits), ", or increase decimals.", call. = FALSE)
}

check_correlations <- function(r, n_variables, r_digits) {
  if (is.null(r)) {
    stop("r is missing. Give the correlation you want, such as r = 0.5.",
         call. = FALSE)
  }
  if (!is_number(r)) {
    stop("r must be a number or a table of numbers.", call. = FALSE)
  }

  # A single r for two variables becomes a 2 x 2 correlation table.
  if (length(r) == 1 && n_variables == 2) {
    r <- matrix(c(1, r, r, 1), nrow = 2)
  }
  is_table <- is.matrix(r) && all(dim(r) == n_variables) &&
    isSymmetric(unname(r)) && all(diag(r) == 1)
  if (!is_table) {
    stop("For ", n_variables, " variables, r must be a ", n_variables, " x ",
         n_variables, " correlation table with 1s on the diagonal and the ",
         "same values above and below it.", call. = FALSE)
  }

  correlations <- r[upper.tri(r)]
  if (any(abs(correlations) >= 1)) {
    stop("A correlation must be between -1 and 1 (not exactly -1 or 1). ",
         "You asked for ",
         format(correlations[abs(correlations) >= 1][1]), ".", call. = FALSE)
  }
  too_fine <- abs(correlations * 10^r_digits -
                    round(correlations * 10^r_digits)) > 1e-6
  if (any(too_fine)) {
    asked <- correlations[too_fine][1]
    stop("Correlations are matched to ", r_digits, " decimal places, but you ",
         "asked for ", format(asked), ". Use ",
         show_number(floor(asked * 10^r_digits) / 10^r_digits, r_digits),
         " or ",
         show_number(ceiling(asked * 10^r_digits) / 10^r_digits, r_digits),
         ".", call. = FALSE)
  }

  # A set of correlations can exist together only if the table is positive
  # definite (all eigenvalues above zero).
  smallest_eigenvalue <- min(eigen(r, symmetric = TRUE,
                                   only.values = TRUE)$values)
  if (smallest_eigenvalue < 1e-8) {
    stop("These correlations cannot all be true at once. For example, if A ",
         "and B are strongly related and A and C are strongly related, then ",
         "B and C cannot be strongly related in the opposite direction. ",
         "Move one or more correlations closer to 0.", call. = FALSE)
  }
  unname(r)
}

# ---- 2. Can rounded data reach each mean and SD? --------------------------

# Returns the total (in rounding units) that each column must add up to.
check_feasible <- function(n, m, sd, decimals) {
  units_per_one <- 10^decimals
  values        <- describe_values(n, decimals)
  target_total  <- numeric(length(m))

  for (j in seq_along(m)) {
    mean_units <- m[j] * units_per_one
    sd_units   <- sd[j] * units_per_one

    # 1. Mean: rounded values add to a whole number of units, so only
    #    certain means exist. Take the total closest to n times the mean.
    total <- round(n * mean_units)
    if (abs(total / n - mean_units) >= half_step - tie_margin) {
      nearest <- nearest_means(n, mean_units) / units_per_one
      stop("With ", values, ", the mean cannot be ",
           show_number(m[j], decimals + 1), ". The closest means you can ",
           "get are ", show_number(nearest[1], decimals + 1), " and ",
           show_number(nearest[2], decimals + 1), ". Use one of those, a ",
           "different n, or more decimals.", call. = FALSE)
    }

    # 2. SD: with the total fixed, the sum of squares moves in steps of 2,
    #    so only certain SDs exist (details in nearest_sds()).
    nearest <- nearest_sds(n, total, sd_units)
    if (!is.null(nearest)) {
      # An SD that would print as 0 is not a usable suggestion.
      nearest <- nearest / units_per_one
      nearest <- nearest[nearest >= 0.5 * 10^-(decimals + 1)]
      stop("With ", values, " and a mean of ",
           show_number(m[j], decimals + 1), ", the SD cannot be ",
           show_number(sd[j], decimals + 1), ". ",
           if (length(nearest) == 1) {
             paste0("The closest SD you can get is ",
                    show_number(nearest, decimals + 1), ". Use that")
           } else if (length(nearest) == 2) {
             paste0("The closest SDs you can get are ",
                    paste(show_number(nearest, decimals + 1),
                          collapse = " and "), ". Use one of those")
           } else {
             "Use a different SD"
           },
           ", a larger n, or more decimals.", call. = FALSE)
    }
    target_total[j] <- total
  }
  target_total
}

# The nearest reportable means below and above an impossible target. A total
# whose mean sits exactly on a rounding boundary is skipped (the tie rule).
nearest_means <- function(n, mean_units) {
  reported <- function(total) {
    tenths <- total / n * 10
    if (abs(abs(tenths - round(tenths)) - 0.5) < tie_margin) NA
    else round(tenths) / 10
  }
  below <- floor(n * mean_units)
  while (is.na(reported(below))) below <- below - 1
  above <- ceiling(n * mean_units)
  while (is.na(reported(above))) above <- above + 1
  c(reported(below), reported(above))
}

# NULL when the SD is reachable; otherwise the nearest SDs below and above.
#
# Write each value as (whole part of the mean) + y. The y values are whole
# numbers that add up to `remainder`, and
#     sum of squared deviations = sum(y^2) - remainder^2 / n.
# sum(y^2) is a whole number that is odd when `remainder` is odd and even
# when it is even, and it can never be smaller than `remainder`. So the
# reachable SDs come from sum(y^2) = remainder, remainder + 2, ...
# With fewer than 5 values some of those sums cannot be made either, so for
# n = 3 or 4 the reachable sums are listed outright (small_n_sums()).
nearest_sds <- function(n, total, sd_units) {
  remainder <- total %% n
  offset    <- remainder^2 / n
  sd_from   <- function(sum_y2) sqrt((sum_y2 - offset) / (n - 1))

  # The sums of squares that would report as the target SD.
  lowest  <- (n - 1) * max(sd_units - half_step, 0)^2 + offset
  highest <- (n - 1) * (sd_units + half_step)^2 + offset

  if (n < 5 && sd_units <= 20) {
    reachable <- small_n_sums(n, remainder,
                              largest = (n - 1) * (sd_units + 1)^2 + offset)
    if (any(reachable > lowest + tie_margin &
            reachable < highest - tie_margin)) return(NULL)
    below <- reachable[reachable <= lowest + tie_margin]
    above <- reachable[reachable >= highest - tie_margin]
    return(sd_from(c(if (length(below)) max(below),
                     if (length(above)) min(above))))
  }

  same_parity_up <- function(x) x + (x - remainder) %% 2
  first_inside   <- max(same_parity_up(floor(lowest + tie_margin) + 1),
                        remainder)
  if (first_inside < highest - tie_margin) return(NULL)

  below <- first_inside - 2
  c(if (below >= remainder) sd_from(below), sd_from(first_inside))
}

# Every value of sum(y^2) that n whole numbers adding to `remainder` can
# have, up to `largest`, found by trying all combinations. No y can be
# larger than sqrt(largest), which keeps the list short for n = 3 or 4.
small_n_sums <- function(n, remainder, largest) {
  reach      <- ceiling(sqrt(largest))
  free       <- as.matrix(expand.grid(rep(list(-reach:reach), n - 1)))
  last_value <- remainder - rowSums(free)
  sums       <- rowSums(free^2) + last_value^2
  sort(unique(sums[sums <= largest]))
}

# ---- 3. Small helpers ------------------------------------------------------

is_number <- function(x) is.numeric(x) && length(x) > 0 && all(is.finite(x))

is_whole <- function(x) is_number(x) && all(abs(x - round(x)) < 1e-8)

# A number as text with a fixed number of decimals ("-0.00" is shown as
# "0.00").
show_number <- function(x, digits) {
  sub("^-(0\\.?0*)$", "\\1", formatC(x, format = "f", digits = digits))
}

describe_values <- function(n, decimals) {
  if (decimals == 0) return(paste(n, "whole-number values"))
  paste0(n, " values rounded to ", decimals, " decimal place",
         if (decimals > 1) "s")
}
