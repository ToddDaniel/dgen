# reg_gen(): data for simple or multiple linear regression.
# Method: the exact engine builds the outcome and the predictors together,
# with each predictor correlated with the outcome as asked and the
# predictors uncorrelated with one another.

#' Generate data for linear regression
#'
#' Creates an outcome and one or more predictors with exactly the means,
#' standard deviations and correlations with the outcome that you choose,
#' and runs the regression. With one predictor it is simple linear
#' regression; with several, multiple regression.
#'
#' @details
#' **How to describe the design.** Say how strongly each predictor is
#' related to the outcome: setting `r` to `c(0.45, 0.10)` means the first
#' predictor correlates .45 with the outcome and the second .10. The
#' predictors are
#' made uncorrelated with one another, so each one's standardized slope
#' (beta) is its own `r`, and R squared is the sum of the squared `r`
#' values. For that reason the squared `r` values must add up to less
#' than 1.
#'
#' **Order.** `m`, `sd` and `var_name` list the outcome first and then the
#' predictors, the way articles lay out correlation tables.
#'
#' **What is exact.** The mean and SD of every variable, rounded to
#' `decimals + 1` places, equal `m` and `sd`. SDs are sample SDs (`n - 1`
#' in the denominator; Excel's `STDEV.S`). Each predictor's correlation
#' with the outcome, rounded to 2 decimal places, equals `r`, and the
#' correlations among the predictors round to 0.00. The slopes, their
#' standard errors and tests, the standardized betas, F, R squared and
#' adjusted R squared are calculated from the data and shown in the
#' summary; they are not targets.
#'
#' **Limits.** The data are bell-shaped, with no minimum or maximum. Coarse
#' rounding makes exact correlations harder: with whole numbers, small SDs
#' and several predictors, use more decimals or a larger `n`. When a request
#' cannot be met, the function stops and says why; in rare cases that can
#' take several seconds.
#'
#' For predictors that are correlated with one another, or to recreate a
#' published regression from its correlation table, use [cor_gen()] with a
#' correlation table and run `lm()` on the result.
#'
#' @inheritParams dgen
#' @param m Target means: the outcome first, then each predictor.
#' @param sd Target standard deviations, in the same order.
#' @param r Target correlation of each predictor with the outcome, such as
#'   `0.6` for one predictor or `c(0.45, 0.10)` for two.
#' @param var_name Names for the variables, in quotes: the outcome first,
#'   then each predictor. The default is `"y"`, `"x1"`, `"x2"`, ...
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   targets beside what the data actually have, then the regression.
#'   `FALSE` prints nothing.
#'
#' @return A data frame with `n` rows and one column per variable (the
#'   outcome first), returned invisibly. The summary is stored inside it;
#'   see it again with [dgen_results()].
#'
#' @examples
#' # Simple regression: sales on advertising
#' ads <- reg_gen(n = 40, m = c(250, 30), sd = c(40, 8), r = 0.6,
#'                var_name = c("sales", "advertising"), decimals = 1,
#'                seed = 1)
#' coef(lm(sales ~ advertising, data = ads))
#'
#' # Multiple regression: two predictors
#' store <- reg_gen(n = 60, m = c(250, 30, 12), sd = c(40, 8, 3),
#'                  r = c(0.45, -0.30),
#'                  var_name = c("sales", "advertising", "price"),
#'                  decimals = 1, seed = 2)
#'
#' @export
reg_gen <- function(n, m, sd, r, decimals = 2, var_name = NULL, seed = NULL,
                    export_csv = FALSE, show_results = TRUE) {

  # 1. Check inputs
  if (missing(n) || missing(m) || missing(sd) || missing(r)) {
    stop("reg_gen needs n, m, sd and r, for example ",
         "reg_gen(n = 40, m = c(250, 30), sd = c(40, 8), r = 0.6).",
         call. = FALSE)
  }
  if (!is_number(r) || !is.null(dim(r))) {
    stop("r must be one correlation for each predictor, such as r = 0.6 or ",
         "r = c(0.45, 0.10).", call. = FALSE)
  }
  n_predictors <- length(r)
  if (length(m) != n_predictors + 1 || length(sd) != n_predictors + 1) {
    stop("reg_gen needs a mean and an SD for the outcome and for each ",
         "predictor. With ", n_predictors, " value(s) of r, give ",
         n_predictors + 1, " means and ", n_predictors + 1, " SDs, the ",
         "outcome first.", call. = FALSE)
  }
  # Uncorrelated predictors each explain r^2 of the outcome, and together
  # they cannot explain all of it.
  if (all(abs(r) < 1) && sum(r^2) >= 1) {
    stop("These correlations are too strong to hold together. When the ",
         "predictors are unrelated to one another, the squared values of r ",
         "must add up to less than 1. Yours add up to ",
         show_number(sum(r^2), 2), ". Lower one or more values of r.",
         call. = FALSE)
  }
  default <- c("y", paste0("x", seq_len(n_predictors)))
  names   <- variable_names(var_name, n_predictors + 1, default)
  use_seed(seed)

  # 2. Build exact data: the outcome and predictors together. In the
  #    correlation table, row and column 1 are the outcome; predictors
  #    correlate with it as asked and 0 with each other.
  target_r <- diag(n_predictors + 1)
  target_r[1, -1] <- r
  target_r[-1, 1] <- r
  values <- exact_mv(n, m, sd, target_r, decimals)$data
  data   <- stats::setNames(as.data.frame(values), names)

  # 3. Run the regression, assemble the summary and hand everything back
  results <- list(
    title = regression_title(var_name, names),
    table = compare_table(names, as.list(data), m, sd, decimals),
    lines = c(outcome_correlations(data, target_r), "",
              regression_lines(data)),
    note  = data_note(data, decimals)
  )
  deliver(data, results, show_results, export_csv, "reg_gen")
}

# Each predictor's correlation with the outcome, target beside data, and
# one line confirming that the predictors are unrelated to one another.
outcome_correlations <- function(data, target_r) {
  n_predictors <- ncol(data) - 1
  with_outcome <- correlation_table(data, target_r)[seq_len(n_predictors), 1:2]
  if (n_predictors == 1) return(utils::capture.output(print(with_outcome)))

  among <- stats::cor(data[-1])
  c(utils::capture.output(print(with_outcome)),
    paste0("Correlations among the predictors: target 0.00, largest in the ",
           "data ", show_number(max(abs(among[upper.tri(among)])), 2)))
}

regression_title <- function(var_name, names) {
  kind <- if (length(names) == 2) "Simple regression" else "Multiple regression"
  if (is.null(var_name)) {
    return(if (length(names) == 2) kind
           else paste(kind, "with", length(names) - 1, "predictors"))
  }
  paste0(kind, ": ", names[1], " on ", and_list(names[-1]))
}

# The coefficient table, the model F test and R squared, as lines of text.
# The outcome is the first column of `data`.
regression_lines <- function(data) {
  fit     <- stats::lm(data[[1]] ~ ., data = data[-1])
  model   <- summary(fit)
  slopes  <- model$coefficients
  spreads <- vapply(data, stats::sd, numeric(1))

  # A standardized beta is the slope in SD units: b * SD(x) / SD(y)
  beta <- slopes[-1, "Estimate"] * spreads[-1] / spreads[1]
  table <- data.frame(
    "b"    = show_estimate(slopes[, "Estimate"]),
    "SE"   = show_estimate(slopes[, "Std. Error"]),
    "Beta" = c("", show_number(beta, 2)),
    "t"    = show_number(slopes[, "t value"], 2),
    "p"    = show_p(slopes[, "Pr(>|t|)"]),
    row.names = c("(Intercept)", names(data)[-1])
  )
  f <- model$fstatistic
  p <- stats::pf(f[["value"]], f[["numdf"]], f[["dendf"]], lower.tail = FALSE)
  c(utils::capture.output(print(table)),
    "",
    paste0("F(", f[["numdf"]], ", ", f[["dendf"]], ") = ",
           show_number(f[["value"]], 2), ", p ", p_text(p)),
    paste0("R squared = ", show_number(model$r.squared, 2),
           "   Adjusted R squared = ", show_number(model$adj.r.squared, 2)))
}
