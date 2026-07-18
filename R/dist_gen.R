#' Generate a Non-Normal Variable with Exact Mean and SD
#'
#' Creates a numeric vector with an exact specified mean and standard
#' deviation but a deliberately non-normal shape: skewed, heavy- or
#' light-tailed, or bimodal. This is the companion to [stat_gen()] for
#' teaching what departures from normality look like — data that visibly
#' fail histograms, Q-Q plots, and normality tests — while still hitting
#' the descriptive statistics a lesson or answer key requires. Because
#' skewness and kurtosis are invariant under linear transformation, the
#' same rescaling that guarantees the exact mean and SD leaves the shape
#' untouched.
#'
#' Two ways to specify shape:
#'
#' \strong{Named shapes} (via \code{shape} + \code{severity}), the
#' shortcut for lectures, tests, and quizzes:
#' \describe{
#'   \item{\code{"normal"}}{No shape distortion (equivalent to
#'     [stat_gen()]); \code{severity} is ignored}
#'   \item{\code{"right_skew"}, \code{"left_skew"}}{Gamma-based skew.
#'     Target skewness by severity: mild = 0.75, moderate = 1.5,
#'     severe = 2.5 (negated for left skew)}
#'   \item{\code{"heavy_tails"}}{Leptokurtic (t-distribution based).
#'     Target excess kurtosis: mild = 0.75, moderate = 2, severe = 6}
#'   \item{\code{"light_tails"}}{Platykurtic (beta-based). Target excess
#'     kurtosis: mild = -0.86, moderate = -1.2 (uniform),
#'     severe = -1.5 (U-shaped)}
#'   \item{\code{"bimodal"}}{Equal mixture of two normal components.
#'     Mode separation by severity: mild = subtle dip, moderate = two
#'     clear humps, severe = well-separated modes}
#' }
#'
#' \strong{Numeric targets} (via \code{skew} and/or \code{kurtosis}),
#' for replicating a specific dataset: values are passed to the
#' Fleishman (1978) power method, which transforms normal draws to hit
#' the requested population skewness and excess kurtosis. \code{kurtosis}
#' here is always \emph{excess} kurtosis (normal = 0). If only
#' \code{skew} is given, excess kurtosis defaults to
#' \code{1.7 * skew^2} (a natural pairing that keeps the request
#' feasible); if only \code{kurtosis} is given, skewness defaults to 0.
#' Not every combination exists: excess kurtosis must exceed roughly
#' \code{skew^2 - 2}, and the Fleishman region is slightly narrower;
#' infeasible requests produce an informative error. Bimodality cannot
#' be requested numerically — use \code{shape = "bimodal"}.
#'
#' The mean and SD are exact (same iterative refinement as
#' [stat_gen()]). The shape is targeted and \emph{reported}, not forced:
#' sample skewness and kurtosis fluctuate around their targets,
#' especially at small n, and the achieved values are returned so you
#' can see exactly what the data do. Values may be negative whenever the
#' mean is within a few SDs of zero; choose \code{m} large relative to
#' \code{sd} for inherently positive variables.
#'
#' @param n Sample size (positive integer)
#' @param m Target mean
#' @param sd Target standard deviation (positive)
#' @param shape One of \code{"normal"}, \code{"right_skew"},
#'   \code{"left_skew"}, \code{"heavy_tails"}, \code{"light_tails"},
#'   \code{"bimodal"} (default = \code{"normal"})
#' @param severity Preset strength for named shapes: \code{"mild"},
#'   \code{"moderate"} (default), or \code{"severe"}
#' @param skew Numeric target skewness (Fleishman route; do not combine
#'   with a named \code{shape})
#' @param kurtosis Numeric target \emph{excess} kurtosis (Fleishman
#'   route; normal = 0)
#' @param decimals Number of decimal places for rounding (default = 2)
#' @param var_name Name of the generated variable (default =
#'   \code{"Variable_X"})
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; compute normality diagnostics
#'   (Shapiro-Wilk, sample skewness/kurtosis with standard errors)?
#'   (default = \code{TRUE})
#'
#' @return A list with:
#'   \item{data}{Data frame with the generated variable}
#'   \item{targets}{Data frame of targets: mean, SD, shape, severity,
#'     and the implied population skewness / excess kurtosis (NA for
#'     bimodal skew/kurtosis targets other than its mixture values)}
#'   \item{achieved}{Data frame of achieved values: mean, SD, sample
#'     skewness, sample excess kurtosis, min, max}
#'   \item{normality}{If \code{run_analysis = TRUE}: Shapiro-Wilk
#'     result (n <= 5000), plus skewness/kurtosis standard errors
#'     (sqrt(6/n), sqrt(24/n)) for z-score interpretation}
#'   \item{iterations}{Number of refinement iterations required}
#'
#' @references Fleishman, A. I. (1978). A method for simulating
#'   non-normal distributions. Psychometrika, 43(4), 521-532.
#'
#' @seealso [stat_gen()] for normal data with exact mean and SD
#'
#' @examples
#' # A visibly right-skewed variable for a histogram lecture
#' result <- dist_gen(n = 200, m = 50, sd = 10,
#'                    shape = "right_skew", severity = "severe", seed = 42)
#' result$achieved
#'
#' # Bimodal data for "always plot your data"
#' result <- dist_gen(n = 200, m = 100, sd = 15,
#'                    shape = "bimodal", severity = "moderate", seed = 42)
#'
#' # Replicating a specific skewness/kurtosis combination
#' result <- dist_gen(n = 300, m = 4.2, sd = 1.1,
#'                    skew = 1.8, kurtosis = 5.5, seed = 42)
#' result$achieved
#'
#' @concept non-normal data
#' @concept skewness
#' @concept kurtosis
#' @concept normality assumption
#'
#' @export
dist_gen <- function(n,
                     m,
                     sd,
                     shape        = "normal",
                     severity     = "moderate",
                     skew         = NULL,
                     kurtosis     = NULL,
                     decimals     = 2,
                     var_name     = "Variable_X",
                     seed         = NULL,
                     export_csv   = FALSE,
                     run_analysis = TRUE) {

  shapes <- c("normal", "right_skew", "left_skew", "heavy_tails",
              "light_tails", "bimodal")
  if (!is.numeric(n) || length(n) != 1 || n <= 3 || n != round(n))
    stop("'n' must be an integer greater than 3")
  if (!is.numeric(m) || length(m) != 1)
    stop("'m' must be a single numeric value")
  if (!is.numeric(sd) || length(sd) != 1 || sd <= 0)
    stop("'sd' must be a positive numeric value")
  if (!shape %in% shapes)
    stop("'shape' must be one of: ", paste(shapes, collapse = ", "))
  if (!severity %in% c("mild", "moderate", "severe"))
    stop("'severity' must be 'mild', 'moderate', or 'severe'")
  if (!is.numeric(decimals) || decimals < 0 || decimals != round(decimals))
    stop("'decimals' must be a non-negative integer")
  if (nchar(var_name) == 0) stop("'var_name' cannot be empty")

  numeric_route <- !is.null(skew) || !is.null(kurtosis)
  if (numeric_route && !missing(shape))
    stop("Supply either a named 'shape' or numeric 'skew'/'kurtosis' ",
         "targets, not both")

  if (numeric_route) {
    if (is.null(skew))     skew     <- 0
    if (is.null(kurtosis)) kurtosis <- 1.7 * skew^2
    if (!is.numeric(skew) || length(skew) != 1)
      stop("'skew' must be a single numeric value")
    if (!is.numeric(kurtosis) || length(kurtosis) != 1)
      stop("'kurtosis' must be a single numeric value (excess kurtosis)")
    coef <- fleishman_coef(skew, kurtosis)
    draw_fun    <- function(n) fleishman_draw(n, coef)
    shape_label <- "numeric"
    target_skew <- skew
    target_kurt <- kurtosis
  } else {
    params      <- dist_shape_params(shape, severity)
    draw_fun    <- params$draw
    shape_label <- shape
    target_skew <- params$skew
    target_kurt <- params$kurtosis
  }

  if (!is.null(seed)) set.seed(seed)

  target_m  <- round(m,  decimals + 1)
  target_sd <- round(sd, decimals + 1)
  max_iterations <- 100000
  iteration <- 0

  repeat {
    iteration <- iteration + 1

    raw_data  <- draw_fun(n)
    z_scores  <- scale(raw_data)
    rescaled  <- z_scores[, 1] * target_sd + target_m
    final_data <- round(rescaled, decimals)

    if (round(mean(final_data), decimals + 1) == target_m &&
        round(stats::sd(final_data), decimals + 1) == target_sd) break

    if (iteration >= max_iterations) {
      warning("Maximum iterations reached. Results may not exactly match targets.")
      break
    }
  }

  data_out <- data.frame(final_data, stringsAsFactors = FALSE)
  colnames(data_out) <- var_name

  x <- data_out[[var_name]]
  ach_skew <- sample_skewness(x)
  ach_kurt <- sample_excess_kurtosis(x)

  targets <- data.frame(
    Variable        = var_name,
    Target_Mean     = m,
    Target_SD       = sd,
    Shape           = shape_label,
    Severity        = if (numeric_route || shape == "normal") NA_character_
                      else severity,
    Target_Skew     = target_skew,
    Target_Ex_Kurt  = target_kurt,
    stringsAsFactors = FALSE
  )

  achieved <- data.frame(
    Variable         = var_name,
    Achieved_Mean    = round(mean(x), decimals + 2),
    Achieved_SD      = round(stats::sd(x), decimals + 2),
    Achieved_Skew    = round(ach_skew, 3),
    Achieved_Ex_Kurt = round(ach_kurt, 3),
    Min              = round(min(x), decimals),
    Max              = round(max(x), decimals),
    stringsAsFactors = FALSE
  )

  normality <- NULL
  if (run_analysis) {
    shapiro <- if (n <= 5000) stats::shapiro.test(x) else NULL
    normality <- list(
      shapiro_test = shapiro,
      skew_se      = sqrt(6 / n),
      kurtosis_se  = sqrt(24 / n),
      note         = if (is.null(shapiro))
        "Shapiro-Wilk skipped (n > 5000)" else NULL
    )
  }

  dgen_export(data_out, export_csv, paste0(var_name, "_dgen.csv"))

  message("Non-normal variable generated in ", iteration, " iterations.")
  message(sprintf("Shape   : %s%s | target skew = %s | target excess kurtosis = %s",
                  shape_label,
                  if (!numeric_route && shape != "normal")
                    paste0(" (", severity, ")") else "",
                  round(target_skew, 3), round(target_kurt, 3)))
  message("Target  : Mean = ", m, " | SD = ", sd)
  message(sprintf("Achieved: Mean = %s | SD = %s | skew = %.3f | excess kurtosis = %.3f",
                  achieved$Achieved_Mean, achieved$Achieved_SD,
                  ach_skew, ach_kurt))
  if (run_analysis && !is.null(normality$shapiro_test)) {
    message(sprintf("Shapiro-Wilk: W = %.4f, p = %s",
                    as.numeric(normality$shapiro_test$statistic),
                    format.pval(normality$shapiro_test$p.value,
                                digits = 3, eps = 0.001)))
  }

  invisible(list(
    data       = data_out,
    targets    = targets,
    achieved   = achieved,
    normality  = normality,
    iterations = iteration
  ))
}

# Internal: draw function and implied population moments for each named
# shape/severity combination.
dist_shape_params <- function(shape, severity) {
  lvl <- function(mild, moderate, severe)
    switch(severity, mild = mild, moderate = moderate, severe = severe)

  switch(shape,
    normal = list(
      draw = function(n) stats::rnorm(n),
      skew = 0, kurtosis = 0
    ),
    right_skew = {
      s <- lvl(0.75, 1.5, 2.5)
      alpha <- (2 / s)^2                       # gamma skewness = 2/sqrt(alpha)
      list(draw = function(n) stats::rgamma(n, shape = alpha),
           skew = s, kurtosis = 6 / alpha)     # gamma excess kurtosis
    },
    left_skew = {
      s <- lvl(0.75, 1.5, 2.5)
      alpha <- (2 / s)^2
      list(draw = function(n) -stats::rgamma(n, shape = alpha),
           skew = -s, kurtosis = 6 / alpha)
    },
    heavy_tails = {
      df <- lvl(12, 7, 5)
      list(draw = function(n) stats::rt(n, df = df),
           skew = 0, kurtosis = 6 / (df - 4))  # t excess kurtosis
    },
    light_tails = {
      a <- lvl(2, 1, 0.5)
      list(draw = function(n) stats::rbeta(n, a, a),
           skew = 0, kurtosis = -6 / (2 * a + 3))  # beta(a,a) excess kurtosis
    },
    bimodal = {
      d <- lvl(1.2, 1.8, 2.8)                  # mode separation in component SDs
      mix_kurt <- (3 + 6 * d^2 + d^4) / (1 + d^2)^2 - 3
      list(draw = function(n) {
        comp <- sample(c(-1, 1), n, replace = TRUE)
        stats::rnorm(n, mean = comp * d, sd = 1)
      },
      skew = 0, kurtosis = mix_kurt)
    }
  )
}

# Internal: solve for Fleishman (1978) power-method coefficients so that
# Y = -c + b*Z + c*Z^2 + d*Z^3 (Z standard normal) has mean 0, variance
# 1, and the requested skewness and excess kurtosis.
fleishman_coef <- function(skew, exkurt) {
  if (exkurt < skew^2 - 2)
    stop("Impossible combination: excess kurtosis must be at least ",
         "skew^2 - 2 (here, at least ", round(skew^2 - 2, 2), ")")
  objective <- function(p) {
    b <- p[1]; cc <- p[2]; d <- p[3]
    v <- b^2 + 6 * b * d + 2 * cc^2 + 15 * d^2 - 1
    s <- 2 * cc * (b^2 + 24 * b * d + 105 * d^2 + 2) - skew
    k <- 24 * (b * d + cc^2 * (1 + b^2 + 28 * b * d) +
                 d^2 * (12 + 48 * b * d + 141 * cc^2 + 225 * d^2)) - exkurt
    v^2 + s^2 + k^2
  }
  starts <- list(c(1, skew / 6, 0.01), c(0.9, skew / 8, 0.05),
                 c(1.05, skew / 5, -0.02))
  best <- list(value = Inf, par = NULL)
  for (st in starts) {
    fit <- stats::optim(st, objective,
                        control = list(maxit = 5000, reltol = 1e-15))
    fit <- stats::optim(fit$par, objective, method = "BFGS",
                        control = list(maxit = 2000, reltol = 1e-15))
    if (fit$value < best$value) best <- fit
    if (best$value < 1e-10) break
  }
  fit <- best
  if (fit$value > 1e-8)
    stop("No Fleishman solution for skew = ", skew, ", excess kurtosis = ",
         exkurt, ". This combination lies outside the feasible region ",
         "(the region is slightly narrower than excess kurtosis >= ",
         "skew^2 - 2). For strongly platykurtic data use ",
         "shape = \"light_tails\"; for bimodal data use shape = \"bimodal\".")
  c(b = fit$par[1], c = fit$par[2], d = fit$par[3])
}

# Internal: draw n values from the Fleishman polynomial.
fleishman_draw <- function(n, coef) {
  z <- stats::rnorm(n)
  -coef["c"] + coef["b"] * z + coef["c"] * z^2 + coef["d"] * z^3
}

# Internal: moment-based (population-formula) sample skewness and excess
# kurtosis, as commonly taught: m3/m2^1.5 and m4/m2^2 - 3.
sample_skewness <- function(x) {
  m  <- mean(x)
  m2 <- mean((x - m)^2)
  m3 <- mean((x - m)^3)
  m3 / m2^1.5
}

sample_excess_kurtosis <- function(x) {
  m  <- mean(x)
  m2 <- mean((x - m)^2)
  m4 <- mean((x - m)^4)
  m4 / m2^2 - 3
}
