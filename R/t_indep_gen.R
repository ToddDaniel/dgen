# t_indep_gen(): data for an independent-samples t test.
# Method: one exact variable per group from the engine, then the t test of
# the difference between the two group means.

#' Generate data for an independent-samples t test
#'
#' Creates two independent groups, each with exactly the mean and standard
#' deviation you choose, and tests the difference between their means.
#'
#' @details
#' **What is exact.** The mean and SD of each group, rounded to
#' `decimals + 1` places, equal `m` and `sd`. If you give `d`, Cohen's d of
#' the returned data, rounded to 2 decimal places, equals `d`. The t
#' statistics, p-values and confidence interval are calculated from the data
#' and shown in the summary; they are not targets.
#'
#' **Direction.** Differences are always the second group minus the first.
#'
#' **Using `d`.** Cohen's d here is (second mean - first mean) / pooled SD.
#' Give the first group's mean as `m`, and `d`; the second group's mean is
#' set to `m + d * pooled SD`, moved to the nearest mean that its `n2`
#' rounded values can have.
#'
#' **Limits.** The data are bell-shaped, with no minimum or maximum. When a
#' request cannot be met, the function stops and says why; in rare cases
#' that can take several seconds.
#'
#' The same data serve a two-sample z test, an F test of two variances, a
#' point-biserial correlation, and a simple logistic regression of group on
#' the measured variable.
#'
#' @inheritParams dgen
#' @param n1,n2 Sample sizes of the first and second group. `n2` is the same
#'   as `n1` unless you say otherwise.
#' @param m Target means of the two groups, such as `c(72, 78)`. If you give
#'   `d`, give only the first group's mean.
#' @param sd Target standard deviations of the two groups, such as
#'   `c(9, 10)`.
#' @param d Target Cohen's d, to 2 decimal places, as another way of setting
#'   the second group's mean.
#' @param labels Names of the two groups, in quotes.
#' @param var_name Name for the measured variable, in quotes. The default is
#'   `"x"`. Not used in wide layout, where the columns are named by `labels`.
#' @param layout `"long"` (the default) gives one row per observation, with
#'   a `group` column and a column for the measured variable. `"wide"` gives
#'   one column per group, the layout the Excel Data Analysis ToolPak needs;
#'   if the groups differ in size, the shorter column ends with empty (`NA`)
#'   cells.
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   targets beside what the data actually have, then the t test. `FALSE`
#'   prints nothing.
#'
#' @return A data frame, returned invisibly: in long layout, `n1 + n2` rows
#'   and two columns (`group` and the measured variable); in wide layout, one
#'   column per group. The summary is stored inside it;
#'   see it again with [dgen_results()].
#'
#' @examples
#' scores <- t_indep_gen(n1 = 25, n2 = 25, m = c(72, 78), sd = c(9, 10),
#'                       labels = c("Online", "Classroom"),
#'                       var_name = "score", decimals = 0, seed = 1)
#' t.test(score ~ group, data = scores, var.equal = TRUE)
#'
#' # One column per group, for the Excel Data Analysis ToolPak
#' wide <- t_indep_gen(n1 = 25, n2 = 25, m = c(72, 78), sd = c(9, 10),
#'                     labels = c("Online", "Classroom"), layout = "wide",
#'                     decimals = 0, seed = 1, show_results = FALSE)
#' head(wide)
#'
#' # The same design from an effect size: Classroom is 0.6 pooled SDs higher
#' scores <- t_indep_gen(n1 = 25, n2 = 25, m = 72, sd = c(9, 10), d = 0.6,
#'                       labels = c("Online", "Classroom"),
#'                       var_name = "score", decimals = 0, seed = 1)
#'
#' @export
t_indep_gen <- function(n1, n2 = n1, m, sd, d = NULL,
                        labels = c("Group 1", "Group 2"), var_name = "x",
                        layout = "long", decimals = 2, seed = NULL,
                        export_csv = FALSE, show_results = TRUE) {

  # 1. Check inputs; with d, work out the second mean it stands for
  if (missing(n1) || missing(m) || missing(sd)) {
    stop("t_indep_gen needs n1, m and sd, for example ",
         "t_indep_gen(n1 = 25, m = c(72, 78), sd = c(9, 10)).", call. = FALSE)
  }
  need_numbers(n1, 1, "n1", "25")
  need_numbers(n2, 1, "n2", "25")
  need_numbers(sd, 2, "sd", "c(9, 10)")
  check_d(d)
  labels <- variable_names(labels, 2, NULL, "labels", "groups")
  name   <- variable_names(var_name, 1, "x")
  use_seed(seed)
  if (is.null(d)) {
    need_numbers(m, 2, "m", "c(72, 78)")
  } else {
    need_numbers(m, 1, "m (the first group's mean, when d is given)", "72")
    check_precision(m, decimals + 1, decimals, "means")
    pooled_sd <- sqrt(((n1 - 1) * sd[1]^2 + (n2 - 1) * sd[2]^2) /
                        (n1 + n2 - 2))
    m <- c(m, mean_from_d(m, d, pooled_sd, n2, decimals))
  }

  # 2. Build exact data; a typed-in d is one more promise for the engine
  cohens_d <- function(groups) {
    spread <- sqrt(((n1 - 1) * stats::var(groups[[1]]) +
                      (n2 - 1) * stats::var(groups[[2]])) / (n1 + n2 - 2))
    (mean(groups[[2]]) - mean(groups[[1]])) / spread
  }
  groups <- exact_groups(
    c(n1, n2), m, sd, decimals, derived_means = !is.null(d),
    also_check = if (!is.null(d)) function(groups) reports_as(cohens_d(groups), d)
  )
  data <- arrange_groups(groups, labels, name, layout)

  # 3. Run the tests, assemble the summary and hand everything back
  pooled <- stats::t.test(groups[[2]], groups[[1]], var.equal = TRUE)
  welch  <- stats::t.test(groups[[2]], groups[[1]])
  results <- list(
    title = paste0("Independent-samples t test: ", name, " by group"),
    table = compare_table(labels, groups, m, sd, decimals),
    lines = c(paste0("Difference in means (", labels[2], " - ", labels[1],
                     ") = ",
                     show_number(mean(groups[[2]]) - mean(groups[[1]]),
                                 decimals + 1)),
              t_line(pooled, "equal variances"),
              t_line(welch, "Welch, unequal variances"),
              ci_line(pooled, "difference", decimals),
              d_line(cohens_d(groups), d)),
    note  = data_note(data, decimals)
  )
  deliver(data, results, show_results, export_csv, "t_indep_gen")
}
