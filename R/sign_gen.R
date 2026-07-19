#' Generate Data for a Sign Test (One-Sample Median or Matched Pairs)
#'
#' Creates data whose sign-test statistic is EXACT: you specify k, the
#' number of observations above the hypothesized median (or, in
#' preference mode, the number preferring the desired option), and the
#' data are constructed to contain exactly that count — so a published
#' result like "n = 11, k = 2, p = .065" regenerates to the number.
#' Runs R's canonical exact \code{binom.test()} and also reports the
#' large-sample normal approximation with continuity correction
#' (z = (|k - n/2| - 0.5) / sqrt(n/4)), the method SPSS and hand
#' calculation use for larger samples.
#'
#' Two modes:
#' \describe{
#'   \item{\code{"median"}}{One-sample median test. Generates numeric
#'     data with exactly \code{k} of \code{n} observations above
#'     \code{target_median} and none equal to it (ties with the target
#'     are excluded by construction, so the usable n equals \code{n}).}
#'   \item{\code{"preference"}}{Matched-pairs preference test.
#'     Generates a categorical column with exactly \code{k} of \code{n}
#'     participants choosing the first (desired) label in
#'     \code{category_labels}.}
#' }
#'
#' Effect sizes, both conventions: raw (\code{p-hat - 0.5}) and
#' standardized (\code{r = z / sqrt(n)}).
#'
#' @param n Sample size (after ties are excluded — the usable n)
#' @param k Number of observations above the target median (median
#'   mode) or preferring the desired option (preference mode);
#'   \code{0 <= k <= n}
#' @param mode \code{"median"} (default) or \code{"preference"}
#' @param target_median Hypothesized median M0 (median mode only)
#' @param category_labels Character vector of length 2, desired option
#'   first (preference mode only; default
#'   \code{c("Desired", "Alternative")})
#' @param spread SD-like scale of the distances from the target
#'   (median mode; approximate; default = 1)
#' @param shape Distribution shape for the magnitudes, as in
#'   [dist_gen()] (median mode; default = \code{"right_skew"})
#' @param severity Shape severity (default = \code{"moderate"})
#' @param scale_min,scale_max Optional bounds (median mode); values are
#'   clamped without crossing the target median
#' @param var_name Name for the generated variable (default =
#'   \code{"Score"} in median mode, \code{"Preference"} in preference
#'   mode)
#' @param decimals Number of decimal places (median mode;
#'   default = 0)
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run the sign test automatically?
#'   (default = \code{TRUE})
#'
#' @return (Invisibly) a list with:
#'   \item{data}{Data frame with the generated variable}
#'   \item{counts}{n, k (above / preferring), j (below / alternative)}
#'   \item{exact_test}{The exact \code{binom.test()} result}
#'   \item{z}{Normal-approximation z with continuity correction}
#'   \item{p_normal}{Two-sided p from the normal approximation}
#'   \item{effect_sizes}{\code{p_hat_minus_half} and \code{r}}
#'   \item{recommendation}{Which result to interpret at this n
#'     (exact for n <= 20, normal approximation above)}
#'
#' @seealso [oneprop_gen()] for the proportion framing of the same
#'   binomial machinery, [signedrank_gen()] for the magnitude-aware
#'   paired test
#'
#' @examples
#' # Replicate "n = 11, k = 2": weekly revenue vs a $9000 target median
#' result <- sign_gen(n = 11, k = 2, target_median = 9000,
#'                    spread = 800, var_name = "Revenue", seed = 42)
#' result$exact_test
#'
#' # Preference mode: 12 of 15 guests prefer the premium pillow
#' pillows <- sign_gen(n = 15, k = 12, mode = "preference",
#'                     category_labels = c("Premium", "Standard"),
#'                     seed = 42)
#' pillows$counts
#'
#' @concept sign test
#' @concept nonparametric
#' @concept median test
#'
#' @export
sign_gen <- function(n,
                     k,
                     mode            = "median",
                     target_median   = NULL,
                     category_labels = c("Desired", "Alternative"),
                     spread          = 1,
                     shape           = "right_skew",
                     severity        = "moderate",
                     scale_min       = NULL,
                     scale_max       = NULL,
                     var_name        = NULL,
                     decimals        = 0,
                     seed            = NULL,
                     export_csv      = FALSE,
                     run_analysis    = TRUE) {

  if (!is.numeric(n) || n < 3 || n != round(n))
    stop("'n' must be an integer >= 3")
  if (!is.numeric(k) || k < 0 || k > n || k != round(k))
    stop("'k' must be an integer between 0 and n")
  if (!mode %in% c("median", "preference"))
    stop("'mode' must be 'median' or 'preference'")

  if (!is.null(seed)) set.seed(seed)

  if (mode == "preference") {
    if (length(category_labels) != 2 ||
        category_labels[1] == category_labels[2])
      stop("'category_labels' must be two distinct labels ",
           "(desired first)")
    if (is.null(var_name)) var_name <- "Preference"
    values <- sample(c(rep(category_labels[1], k),
                       rep(category_labels[2], n - k)))
    data_out <- data.frame(
      id = seq_len(n),
      v  = factor(values, levels = category_labels),
      stringsAsFactors = FALSE
    )
    colnames(data_out) <- c("ID", var_name)
  } else {
    if (is.null(target_median) || !is.numeric(target_median) ||
        length(target_median) != 1)
      stop("'target_median' is required in median mode")
    npar_validate_common(spread, shape, severity, scale_min, scale_max,
                         decimals)
    if (!is.null(scale_min) && target_median <= scale_min && k < n)
      stop("'scale_min' leaves no room for values below the target median")
    if (!is.null(scale_max) && target_median >= scale_max && k > 0)
      stop("'scale_max' leaves no room for values above the target median")
    if (is.null(var_name)) var_name <- "Score"

    step     <- 10^(-decimals)
    draw_fun <- dist_shape_params(shape, severity)$draw
    mags <- abs(as.vector(scale(draw_fun(n)))) * spread
    mags <- pmax(round(mags, decimals), step)   # never lands on M0
    signs <- sample(c(rep(1, k), rep(-1, n - k)))
    values <- target_median + signs * mags
    if (!is.null(scale_min))
      values <- pmax(values, pmin(scale_min, target_median - step))
    if (!is.null(scale_max))
      values <- pmin(values, pmax(scale_max, target_median + step))
    values <- round(values, decimals)

    data_out <- data.frame(id = seq_len(n), v = values,
                           stringsAsFactors = FALSE)
    colnames(data_out) <- c("ID", var_name)
  }

  counts <- list(n = n, k = k, j = n - k)

  exact_test <- NULL
  z <- NULL
  p_normal <- NULL
  effect_sizes <- NULL
  recommendation <- NULL

  if (run_analysis) {
    exact_test <- stats::binom.test(k, n, p = 0.5)
    z_abs <- max(0, (abs(k - 0.5 * n) - 0.5) / sqrt(0.25 * n))
    z <- z_abs * sign(k - 0.5 * n + (k == 0.5 * n))
    p_normal <- 2 * stats::pnorm(-abs(z))
    effect_sizes <- list(
      p_hat_minus_half = k / n - 0.5,
      r                = z / sqrt(n)
    )
    recommendation <- if (n <= 20) {
      "n <= 20: interpret the exact binomial test"
    } else {
      "n > 20: interpret the normal approximation (z, continuity-corrected)"
    }
  }

  dgen_export(data_out, export_csv, paste0(var_name, "_sign_dgen.csv"))

  message(sprintf("Sign test data (%s mode): n = %d, k = %d, j = %d",
                  mode, n, k, n - k))
  if (run_analysis) {
    message(sprintf("Exact binomial: p = %s | Normal approx: z = %.3f, p = %s",
                    format.pval(exact_test$p.value, digits = 3,
                                eps = 0.001),
                    z, format.pval(p_normal, digits = 3, eps = 0.001)))
    message(recommendation)
  }

  result <- list(data = data_out, counts = counts)
  if (run_analysis) {
    result$exact_test     <- exact_test
    result$z              <- z
    result$p_normal       <- p_normal
    result$effect_sizes   <- effect_sizes
    result$recommendation <- recommendation
  }
  invisible(result)
}
