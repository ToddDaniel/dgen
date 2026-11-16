# t_one_gen(): data for a one-sample t test.
# Method: one exact variable from the engine, then the t test of its mean
# against the test value.

#' Generate data for a one-sample t test
#'
#' Creates one variable with exactly the mean and standard deviation you
#' choose, and tests its mean against a test value (the mean under the null
#' hypothesis).
#'
#' @details
#' **What is exact.** The mean and SD of the returned data, rounded to
#' `decimals + 1` places, equal `m` and `sd`. SDs are sample SDs (`n - 1`
#' in the denominator; Excel's `STDEV.S`). If you give `d` instead of
#' `m`, Cohen's d of the returned data, rounded to 2 decimal places, equals
#' `d`. The t statistic, p-value and confidence interval are calculated from
#' the data and shown in the summary; they are not targets.
#'
#' **Using `d`.** Cohen's d here is (mean - `test_value`) / SD. Giving `d`
#' sets the mean to `test_value + d * sd`, moved to the nearest mean that
#' `n` rounded values can have.
#'
#' **Limits.** The data are bell-shaped, with no minimum or maximum. When a
#' request cannot be met, the function stops and says why; in rare cases
#' that can take several seconds.
#'
#' The same data serve a one-sample z test when the population SD is
#' treated as known.
#'
#' @inheritParams dgen
#' @param m Target mean. Leave it out if you give `d`.
#' @param sd Target standard deviation.
#' @param test_value The mean under the null hypothesis: the value the
#'   sample mean is tested against.
#' @param d Target Cohen's d, to 2 decimal places, as another way of setting
#'   the mean. Give `m` or `d`, not both.
#' @param var_name Name for the variable, in quotes. The default is `"x"`.
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   targets beside what the data actually have, then the t test. `FALSE`
#'   prints nothing.
#'
#' @return A data frame with `n` rows and one column, returned invisibly.
#'   The summary is stored inside it; see it again with [dgen_results()].
#'
#' @examples
#' # Is the mean delivery time different from 30 minutes?
#' times <- t_one_gen(n = 25, m = 32.4, sd = 5.5, test_value = 30,
#'                    decimals = 1, var_name = "minutes", seed = 1)
#' t.test(times$minutes, mu = 30)
#'
#' # The same design from an effect size
#' times <- t_one_gen(n = 25, sd = 5.5, test_value = 30, d = 0.5,
#'                    decimals = 1, var_name = "minutes", seed = 1)
#'
#' @export
t_one_gen <- function(n, m, sd, test_value, d = NULL, decimals = 2,
                      var_name = NULL, seed = NULL, export_csv = FALSE,
                      show_results = TRUE) {

  # 1. Check inputs; with d, work out the mean it stands for
  if (missing(n) || missing(sd) || missing(test_value)) {
    stop("t_one_gen needs n, sd, test_value and either m or d, for example ",
         "t_one_gen(n = 25, m = 32, sd = 5, test_value = 30).", call. = FALSE)
  }
  if (missing(m) == is.null(d)) {
    stop("Give the mean (m) or Cohen's d (d), but not both: d is another ",
         "way of setting the mean.", call. = FALSE)
  }
  check_d(d)
  need_numbers(sd, 1, "sd", "5")
  need_numbers(test_value, 1, "test_value", "30")
  name <- variable_names(var_name, 1, "x")
  use_seed(seed)
  if (!is.null(d)) m <- mean_from_d(test_value, d, sd, n, decimals)
  need_numbers(m, 1, "m", "32")

  # 2. Build exact data; a typed-in d is one more promise for the engine
  cohens_d <- function(values) {
    (mean(values) - test_value) / stats::sd(values)
  }
  values <- exact_mv(
    n, m, sd, decimals = decimals, derived_means = !is.null(d),
    also_check = if (!is.null(d)) function(data) reports_as(cohens_d(data), d)
  )$data[, 1]
  data <- stats::setNames(data.frame(values), name)

  # 3. Run the test, assemble the summary and hand everything back
  test <- stats::t.test(values, mu = test_value)
  results <- list(
    title = paste0("One-sample t test",
                   if (!is.null(var_name)) paste0(": ", name),
                   " against a test value of ", format(test_value)),
    table = compare_table(name, list(values), m, sd, decimals),
    lines = c(paste0("Mean - test value = ",
                     show_number(mean(values) - test_value, decimals + 1)),
              t_line(test),
              ci_line(test, "mean", decimals),
              d_line(cohens_d(values), d)),
    note  = data_note(data, decimals)
  )
  deliver(data, results, show_results, export_csv, "t_one_gen")
}
