

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
#' @seealso [anova_gen()]
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
#' @seealso [twoprop_gen()], [oneprop_gen()]
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
