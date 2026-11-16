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
