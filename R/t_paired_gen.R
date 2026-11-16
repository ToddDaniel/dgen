# t_paired_gen(): data for a paired (dependent-samples) t test.
# Method: two exactly correlated variables from the engine, one row per
# person, then the t test of the mean difference.

#' Generate data for a paired t test
#'
#' Creates two measurements on the same people, with exactly the means,
#' standard deviations and correlation you choose, and tests the mean
#' difference.
#'
#' @details
#' **What is exact.** The mean and SD of each measurement, rounded to
#' `decimals + 1` places, equal `m` and `sd`, and their correlation, rounded
#' to 2 decimal places, equals `r`. SDs are sample SDs (`n - 1` in the
#' denominator; Excel's `STDEV.S`). Because both means are exact, so is the
#' mean difference. If you give `d`, Cohen's d of the returned data, rounded
#' to 2 decimal places, equals `d`. The SD of the differences, the t
#' statistic, the p-value and the confidence interval are calculated from
#' the data and shown in the summary; they are not targets.
#'
#' **Direction.** Differences are always the second measurement minus the
#' first.
#'
#' **Why `r` matters.** The SD of the differences depends on the
#' correlation: the more strongly the two measurements are related, the
#' smaller the SD of the differences and the larger t becomes.
#'
#' **Using `d`.** Cohen's d here is the mean difference divided by the SD of
#' the differences (t divided by the square root of `n`). Give the first
#' measurement's mean as `m`, and `d`; the second mean is set to
#' `m + d * SD of differences`, moved to the nearest mean that `n` rounded
#' values can have.
#'
#' **Limits.** The data are bell-shaped, with no minimum or maximum. When a
#' request cannot be met, the function stops and says why; in rare cases
#' that can take several seconds.
#'
#' @inheritParams dgen
#' @param n Number of people (pairs of measurements).
#' @param m Target means of the two measurements, such as `c(50, 55)`. If
#'   you give `d`, give only the first mean.
#' @param sd Target standard deviations of the two measurements.
#' @param r Target correlation between the two measurements, such as `0.7`.
#' @param d Target Cohen's d, to 2 decimal places, as another way of setting
#'   the second mean.
#' @param var_name Names for the two measurements, in quotes. The default is
#'   `c("time1", "time2")`.
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   targets beside what the data actually have, then the t test. `FALSE`
#'   prints nothing.
#'
#' @return A data frame with `n` rows (one per person) and two columns (one
#'   per measurement), returned invisibly. The summary is stored inside it;
#'   see it again with [dgen_results()].
#'
#' @examples
#' training <- t_paired_gen(n = 30, m = c(50, 55), sd = c(10, 10), r = 0.7,
#'                          var_name = c("before", "after"), decimals = 1,
#'                          seed = 1)
#' t.test(training$after, training$before, paired = TRUE)
#'
#' # The same design from an effect size
#' training <- t_paired_gen(n = 30, m = 50, sd = c(10, 10), r = 0.7, d = 0.5,
#'                          var_name = c("before", "after"), decimals = 1,
#'                          seed = 1)
#'
#' @export
t_paired_gen <- function(n, m, sd, r, d = NULL, var_name = NULL,
                         decimals = 2, seed = NULL, export_csv = FALSE,
                         show_results = TRUE) {

  # 1. Check inputs; with d, work out the second mean it stands for
  if (missing(n) || missing(m) || missing(sd) || missing(r)) {
    stop("t_paired_gen needs n, m, sd and r, for example ",
         "t_paired_gen(n = 30, m = c(50, 55), sd = c(10, 10), r = 0.7).",
         call. = FALSE)
  }
  need_numbers(n, 1, "n", "30")
  need_numbers(sd, 2, "sd", "c(10, 10)")
  need_numbers(r, 1, "r", "0.7")
  check_d(d)
  names <- variable_names(var_name, 2, c("time1", "time2"))
  use_seed(seed)
  if (is.null(d)) {
    need_numbers(m, 2, "m", "c(50, 55)")
  } else {
    need_numbers(m, 1, "m (the first mean, when d is given)", "50")
    check_precision(m, decimals + 1, decimals, "means")
    sd_of_differences <- sqrt(sd[1]^2 + sd[2]^2 - 2 * r * sd[1] * sd[2])
    m <- c(m, mean_from_d(m, d, sd_of_differences, n, decimals))
  }

  # 2. Build exact data; a typed-in d is one more promise for the engine
  cohens_d <- function(values) {
    differences <- values[, 2] - values[, 1]
    mean(differences) / stats::sd(differences)
  }
  values <- exact_mv(
    n, m, sd, r, decimals, derived_means = !is.null(d),
    also_check = if (!is.null(d)) function(data) reports_as(cohens_d(data), d)
  )$data
  data <- stats::setNames(as.data.frame(values), names)

  # 3. Run the test, assemble the summary and hand everything back
  differences <- values[, 2] - values[, 1]
  test <- stats::t.test(values[, 2], values[, 1], paired = TRUE)
  results <- list(
    title = paste0("Paired t test",
                   if (!is.null(var_name)) paste0(": ", and_list(names))),
    table = compare_table(names, as.list(data), m, sd, decimals),
    lines = c(paste0("Correlation between ", names[1], " and ", names[2],
                     ": target ", show_number(r, 2), ", data ",
                     show_number(stats::cor(values)[1, 2], 2)),
              paste0("Differences (", names[2], " - ", names[1], "): mean = ",
                     show_number(mean(differences), decimals + 1), ", SD = ",
                     show_number(stats::sd(differences), decimals + 1)),
              t_line(test),
              ci_line(test, "mean difference", decimals),
              d_line(cohens_d(values), d,
                     "mean difference / SD of differences")),
    note  = data_note(data, decimals)
  )
  deliver(data, results, show_results, export_csv, "t_paired_gen")
}
