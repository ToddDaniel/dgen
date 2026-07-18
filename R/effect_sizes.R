#' Convert Cohen's d to Target Means for Independent Groups
#'
#' Computes the second group mean that yields a specified Cohen's d, given
#' the group standard deviations and the first group mean. Useful for
#' working "effect-size-first": decide the d you want students to find,
#' then pass the returned means to [t_indep_gen()] (or, with
#' \code{sd2 = sd1}, use \code{mean2} as the target mean for
#' [t_onesample_gen()] against \code{test_value = mean1}).
#'
#' Cohen's d is defined here as \code{(mean2 - mean1) / pooled_sd} with
#' \code{pooled_sd = sqrt((sd1^2 + sd2^2) / 2)}, the equal-group-size
#' pooled standard deviation. When \code{sd1 == sd2} this is exact for any
#' group sizes.
#'
#' @param d Target Cohen's d (positive means \code{mean2 > mean1})
#' @param sd1 Standard deviation of Group 1
#' @param sd2 Standard deviation of Group 2 (default = \code{sd1})
#' @param mean1 Mean of Group 1 (default = 0)
#'
#' @return (Invisibly) a list with:
#'   \item{mean1}{Group 1 mean (as supplied)}
#'   \item{mean2}{Group 2 mean that produces the target d}
#'   \item{sd1, sd2}{Standard deviations (as supplied)}
#'   \item{pooled_sd}{Pooled standard deviation used in the conversion}
#'   \item{d}{Target Cohen's d (as supplied)}
#'
#' @seealso [t_indep_gen()], [d_to_means_paired()], [f_to_means()],
#'   [h_to_prop()]
#'
#' @examples
#' # A medium effect (d = 0.5) on an IQ-style scale
#' params <- d_to_means(d = 0.5, sd1 = 15, mean1 = 100)
#' params$mean2
#'
#' @export
d_to_means <- function(d, sd1, sd2 = sd1, mean1 = 0) {
  if (!is.numeric(d) || length(d) != 1)
    stop("'d' must be a single number")
  if (!is.numeric(sd1) || length(sd1) != 1 || sd1 <= 0)
    stop("'sd1' must be a single positive number")
  if (!is.numeric(sd2) || length(sd2) != 1 || sd2 <= 0)
    stop("'sd2' must be a single positive number")
  if (!is.numeric(mean1) || length(mean1) != 1)
    stop("'mean1' must be a single number")

  pooled_sd <- sqrt((sd1^2 + sd2^2) / 2)
  mean2     <- mean1 + d * pooled_sd

  message(sprintf("d = %.3f: mean1 = %g, mean2 = %g (pooled SD = %.3f)",
                  d, mean1, mean2, pooled_sd))

  invisible(list(
    mean1     = mean1,
    mean2     = mean2,
    sd1       = sd1,
    sd2       = sd2,
    pooled_sd = pooled_sd,
    d         = d
  ))
}

#' Convert Paired Cohen's d to Target Means for Repeated Measures
#'
#' Computes the second measurement mean that yields a specified paired
#' Cohen's d, given the measurement standard deviations, their correlation,
#' and the first measurement mean. Pass the returned means to
#' [t_paired_gen()] with the same \code{correlation}.
#'
#' The paired d is defined as \code{(mean2 - mean1) / sd_diff}, matching
#' the \code{cohens_d_paired} value reported by [t_paired_gen()], where
#' \code{sd_diff = sqrt(sd1^2 + sd2^2 - 2 * r * sd1 * sd2)} is the standard
#' deviation of the difference scores.
#'
#' @param d Target paired Cohen's d (positive means \code{mean2 > mean1})
#' @param sd1 Standard deviation of measurement 1
#' @param sd2 Standard deviation of measurement 2 (default = \code{sd1})
#' @param r Correlation between the two measurements (default = 0.70,
#'   matching the [t_paired_gen()] default)
#' @param mean1 Mean of measurement 1 (default = 0)
#'
#' @return (Invisibly) a list with:
#'   \item{mean1}{Measurement 1 mean (as supplied)}
#'   \item{mean2}{Measurement 2 mean that produces the target d}
#'   \item{sd1, sd2}{Standard deviations (as supplied)}
#'   \item{r}{Correlation (as supplied)}
#'   \item{sd_diff}{Standard deviation of the difference scores}
#'   \item{d}{Target paired Cohen's d (as supplied)}
#'
#' @seealso [t_paired_gen()], [d_to_means()]
#'
#' @examples
#' # Pre/post design: d = 0.8 with r = 0.70
#' params <- d_to_means_paired(d = 0.8, sd1 = 10, r = 0.70, mean1 = 50)
#' params$mean2
#'
#' @export
d_to_means_paired <- function(d, sd1, sd2 = sd1, r = 0.70, mean1 = 0) {
  if (!is.numeric(d) || length(d) != 1)
    stop("'d' must be a single number")
  if (!is.numeric(sd1) || length(sd1) != 1 || sd1 <= 0)
    stop("'sd1' must be a single positive number")
  if (!is.numeric(sd2) || length(sd2) != 1 || sd2 <= 0)
    stop("'sd2' must be a single positive number")
  if (!is.numeric(r) || length(r) != 1 || r <= -1 || r >= 1)
    stop("'r' must be a single number between -1 and 1 (exclusive)")
  if (!is.numeric(mean1) || length(mean1) != 1)
    stop("'mean1' must be a single number")

  sd_diff <- sqrt(sd1^2 + sd2^2 - 2 * r * sd1 * sd2)
  if (sd_diff == 0)
    stop("The difference-score SD is zero for these inputs; ",
         "d is undefined")
  mean2 <- mean1 + d * sd_diff

  message(sprintf(
    "paired d = %.3f: mean1 = %g, mean2 = %g (SD of differences = %.3f)",
    d, mean1, mean2, sd_diff))

  invisible(list(
    mean1   = mean1,
    mean2   = mean2,
    sd1     = sd1,
    sd2     = sd2,
    r       = r,
    sd_diff = sd_diff,
    d       = d
  ))
}

#' Convert Cohen's f (or Eta-Squared) to Target Group Means for ANOVA
#'
#' Computes a set of evenly spaced group means that yields a specified
#' Cohen's f (or eta-squared), given the number of groups and the
#' within-group standard deviation. Pass the returned means to
#' [anova_gen()] with the same \code{sds}.
#'
#' Cohen's f is defined as \code{sigma_means / sigma_within} (Cohen, 1988),
#' where \code{sigma_means} is the population standard deviation of the
#' group means around the grand mean. The returned means are evenly spaced
#' and centered on \code{grand_mean}. Conventional benchmarks: f = 0.10
#' (small), 0.25 (medium), 0.40 (large). If \code{eta_squared} is supplied
#' instead of \code{f}, it is converted via
#' \code{f = sqrt(eta_squared / (1 - eta_squared))}. The sample eta-squared
#' computed from generated data approaches this population target as group
#' size grows.
#'
#' @param f Target Cohen's f (supply exactly one of \code{f} or
#'   \code{eta_squared})
#' @param k Number of groups (minimum 2)
#' @param sd Within-group standard deviation (used for every group)
#' @param grand_mean Center of the group means (default = 0)
#' @param eta_squared Target eta-squared, converted to f (supply exactly
#'   one of \code{f} or \code{eta_squared})
#'
#' @return (Invisibly) a list with:
#'   \item{means}{Numeric vector of k evenly spaced group means}
#'   \item{f}{Target Cohen's f}
#'   \item{eta_squared}{Population eta-squared implied by f}
#'   \item{sd}{Within-group standard deviation (as supplied)}
#'   \item{grand_mean}{Center of the means (as supplied)}
#'
#' @seealso [anova_gen()], [d_to_means()]
#'
#' @examples
#' # Three groups, medium effect (f = 0.25), within-group SD of 10
#' params <- f_to_means(f = 0.25, k = 3, sd = 10, grand_mean = 50)
#' params$means
#'
#' # The same request stated as eta-squared
#' params2 <- f_to_means(eta_squared = 0.059, k = 3, sd = 10, grand_mean = 50)
#'
#' @export
f_to_means <- function(f = NULL, k, sd, grand_mean = 0, eta_squared = NULL) {
  if (is.null(f) == is.null(eta_squared))
    stop("Supply exactly one of 'f' or 'eta_squared'")
  if (!is.null(eta_squared)) {
    if (!is.numeric(eta_squared) || length(eta_squared) != 1 ||
        eta_squared <= 0 || eta_squared >= 1)
      stop("'eta_squared' must be a single number between 0 and 1 (exclusive)")
    f <- sqrt(eta_squared / (1 - eta_squared))
  }
  if (!is.numeric(f) || length(f) != 1 || f <= 0)
    stop("'f' must be a single positive number")
  if (!is.numeric(k) || length(k) != 1 || k < 2 || k != round(k))
    stop("'k' must be an integer >= 2")
  if (!is.numeric(sd) || length(sd) != 1 || sd <= 0)
    stop("'sd' must be a single positive number")
  if (!is.numeric(grand_mean) || length(grand_mean) != 1)
    stop("'grand_mean' must be a single number")

  dev     <- seq_len(k) - (k + 1) / 2
  scaling <- f * sd / sqrt(mean(dev^2))
  means   <- grand_mean + scaling * dev
  eta_sq  <- f^2 / (1 + f^2)

  message(sprintf("f = %.3f (eta^2 = %.3f), k = %d, within-group SD = %g",
                  f, eta_sq, k, sd))
  message("Group means: ", paste(round(means, 4), collapse = ", "))

  invisible(list(
    means       = means,
    f           = f,
    eta_squared = eta_sq,
    sd          = sd,
    grand_mean  = grand_mean
  ))
}

#' Convert Cohen's h to a Second Target Proportion
#'
#' Computes the second proportion that yields a specified Cohen's h, given
#' the first proportion. Pass the returned proportions to [twoprop_gen()]
#' (or use \code{p2} as the target for [oneprop_gen()] against
#' \code{test_value = p1}).
#'
#' Cohen's h is defined as
#' \code{2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))}, matching the
#' \code{cohens_h} value reported by [twoprop_gen()]. Positive h means
#' \code{p2 < p1}. Conventional benchmarks: h = 0.20 (small), 0.50
#' (medium), 0.80 (large).
#'
#' @param h Target Cohen's h
#' @param p1 First proportion (0 < p1 < 1)
#'
#' @return (Invisibly) a list with:
#'   \item{p1}{First proportion (as supplied)}
#'   \item{p2}{Second proportion that produces the target h}
#'   \item{h}{Target Cohen's h (as supplied)}
#'
#' @seealso [twoprop_gen()], [oneprop_gen()], [d_to_means()]
#'
#' @examples
#' # Control group at 0.60; what proportion gives a medium effect (h = 0.5)?
#' params <- h_to_prop(h = 0.5, p1 = 0.60)
#' params$p2
#'
#' @export
h_to_prop <- function(h, p1) {
  if (!is.numeric(h) || length(h) != 1)
    stop("'h' must be a single number")
  if (!is.numeric(p1) || length(p1) != 1 || p1 <= 0 || p1 >= 1)
    stop("'p1' must be a single number between 0 and 1 (exclusive)")

  phi2 <- asin(sqrt(p1)) - h / 2
  if (phi2 <= 0 || phi2 >= pi / 2)
    stop("This combination of 'h' and 'p1' implies a second proportion ",
         "outside (0, 1); use a smaller |h| or a less extreme 'p1'")
  p2 <- sin(phi2)^2

  message(sprintf("h = %.3f: p1 = %.3f, p2 = %.3f", h, p1, p2))

  invisible(list(p1 = p1, p2 = p2, h = h))
}
