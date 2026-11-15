# cor_gen(): two or more variables with exact means, SDs and correlations.
# Method: the exact engine builds all the variables together, so every
# correlation in the rounded data matches its target to 2 decimal places.

#' Generate correlated data with exact means, SDs and correlations
#'
#' Creates two variables whose means, standard deviations and correlation
#' are exactly the values you ask for, in the rounded data your students
#' will see. With a correlation table, it creates three or more variables.
#'
#' @details
#' **What "exact" means.** The mean and SD of each variable, rounded to
#' `decimals + 1` places, equal `m` and `sd`, and each correlation, rounded
#' to 2 decimal places, equals `r`.
#'
#' **How it works.** Random bell-shaped values are transformed so that, in
#' the sample itself, every variable has a mean of exactly 0 and an SD of
#' exactly 1 and every pair is exactly uncorrelated. The correlations you
#' asked for are then built in, the variables are rescaled to your means and
#' SDs, and the data are rounded. A few values are moved by one unit to make
#' each mean exact, and the draw is repeated until every SD and correlation
#' of the rounded data matches as well.
#'
#' **Limits.** The data are bell-shaped, with no minimum or maximum. Coarse
#' rounding makes exact correlations harder: with whole numbers, small SDs
#' and several variables, use more decimals or a larger `n`. When a request
#' cannot be met, `cor_gen()` stops and says why; in rare cases that can
#' take several seconds.
#'
#' **Three or more variables.** Give `r` as a correlation table: a square
#' matrix with 1s on the diagonal and the same values above and below it.
#' This is also how to recreate a published study from its means, SDs and
#' correlation table.
#'
#' **Regression.** The data can be used for simple linear regression: the
#' slope of the second variable on the first is `r` times the ratio of
#' their SDs.
#'
#' @inheritParams dgen
#' @param m Target means, one for each variable, such as `c(50, 55)`.
#' @param sd Target standard deviations, one for each variable.
#' @param r Target correlation between the two variables, such as `0.5`.
#'   For three or more variables, a correlation table (see Details).
#' @param var_name Names for the variables, in quotes, such as
#'   `c("hours", "exam")`. The default is `"x"` and `"y"` (or `"x1"`,
#'   `"x2"`, ... for three or more).
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   targets beside what the data actually have, and the test of each
#'   correlation. `FALSE` prints nothing.
#'
#' @return A data frame with `n` rows and one column per variable, returned
#'   invisibly. The summary is stored inside it; see it again with
#'   [dgen_results()].
#'
#' @examples
#' study <- cor_gen(n = 40, m = c(6.5, 78), sd = c(2.1, 9.5), r = 0.55,
#'                  decimals = 1, var_name = c("hours", "exam"), seed = 1)
#' cor(study$hours, study$exam)
#'
#' # Simple regression on the same data
#' coef(lm(exam ~ hours, data = study))
#'
#' # Three variables from a correlation table
#' table <- matrix(c(1.00, 0.40, 0.30,
#'                   0.40, 1.00, 0.20,
#'                   0.30, 0.20, 1.00), nrow = 3)
#' survey <- cor_gen(n = 100, m = c(2.8, 2.6, 3.0), sd = c(0.9, 0.85, 1.0),
#'                   r = table, var_name = c("pay", "hours", "morale"),
#'                   seed = 2)
#'
#' @export
cor_gen <- function(n, m, sd, r, decimals = 2, var_name = NULL, seed = NULL,
                    export_csv = FALSE, show_results = TRUE) {

  # 1. Check inputs
  if (missing(n) || missing(m) || missing(sd) || missing(r)) {
    stop("cor_gen needs n, m, sd and r, for example ",
         "cor_gen(n = 30, m = c(50, 55), sd = c(10, 10), r = 0.5).",
         call. = FALSE)
  }
  n_variables <- length(m)
  if (n_variables < 2) {
    stop("cor_gen makes two or more variables, so it needs a mean and an SD ",
         "for each one, such as m = c(50, 55) and sd = c(10, 10).",
         call. = FALSE)
  }
  default <- if (n_variables == 2) c("x", "y")
             else paste0("x", seq_len(n_variables))
  names   <- variable_names(var_name, n_variables, default)
  use_seed(seed)

  # 2. Build exact data, all variables together
  values <- exact_mv(n, m, sd, r, decimals)$data
  data   <- stats::setNames(as.data.frame(values), names)

  # 3. Assemble the summary and hand everything back
  target_r <- if (length(r) == 1) matrix(c(1, r, r, 1), nrow = 2) else r
  results <- list(
    title = if (n_variables == 2)
              paste("Correlation between", names[1], "and", names[2])
            else paste("Correlations among", n_variables, "variables"),
    table = compare_table(names, as.list(data), m, sd, decimals),
    lines = utils::capture.output(print(correlation_table(data, target_r))),
    note  = data_note(data, decimals)
  )
  deliver(data, results, show_results, export_csv, "cor_gen")
}

# One row per pair of variables: the target correlation, the correlation in
# the data, and the usual t test of that correlation.
correlation_table <- function(data, target_r) {
  pairs <- which(upper.tri(target_r), arr.ind = TRUE)
  rows  <- lapply(seq_len(nrow(pairs)), function(pair) {
    first  <- pairs[pair, "row"]
    second <- pairs[pair, "col"]
    test   <- stats::cor.test(data[[first]], data[[second]])
    data.frame(
      "r (target)" = show_number(target_r[first, second], 2),
      "r (data)"   = show_number(unname(test$estimate), 2),
      "t"          = show_number(unname(test$statistic), 2),
      "df"         = unname(test$parameter),
      "p"          = show_p(test$p.value),
      row.names   = paste(names(data)[first], "and", names(data)[second]),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}
