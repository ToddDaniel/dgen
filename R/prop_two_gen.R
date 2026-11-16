# prop_two_gen(): data for a two-proportion z test.
# Method: each group's proportion becomes a whole count, the engine writes
# out one row per observation in random order, and the pooled z test is
# calculated from the counts.

#' Generate data for a two-proportion z test
#'
#' Creates two independent groups in which exactly the proportions you
#' choose answer "Yes", and tests the difference between the two
#' proportions.
#'
#' @details
#' **What is exact.** The number of "Yes" answers in each group is that
#' group's size times its `prop`. When that is not a whole number, the
#' nearest whole count is used and the summary says so. The z statistic,
#' p-value, confidence interval and effect size are calculated from the data
#' and shown in the summary; they are not targets.
#'
#' **Direction.** Differences are always the first group minus the second:
#' the difference in proportions, z, the confidence interval and Cohen's h
#' all run that way, and the summary names the direction. If a student's
#' software shows the opposite sign, it listed the groups in the other
#' order: a CSV file does not record the order, and some programs sort group
#' names alphabetically. The p-value is the same either way.
#'
#' **The test.** The summary shows the textbook pooled z test: the standard
#' error uses the proportion in the two groups combined, with no continuity
#' correction, so it matches a hand calculation. The confidence interval
#' for the difference uses each group's own proportion, as textbooks do. If
#' an expected count is below 5 the summary adds a note.
#'
#' **Effect size.** Cohen's h compares the two proportions on a scale where
#' 0.2 is small, 0.5 medium and 0.8 large.
#'
#' The same data serve a chi-square test of independence on the 2 x 2
#' table: its chi-square equals z squared. In R, `prop.test()` and
#' `chisq.test()` apply a continuity correction to a 2 x 2 table unless you
#' add `correct = FALSE`, so their default results differ slightly from the
#' summary. Excel and JASP make no correction by default.
#'
#' @inheritParams dgen
#' @param n1,n2 Sample sizes of the first and second group. `n2` is the same
#'   as `n1` unless you say otherwise.
#' @param prop Target proportions answering "Yes" in the two groups, each
#'   between 0 and 1, such as `c(0.6, 0.45)`.
#' @param labels Names of the two groups, in quotes, first group first. The
#'   default is `"Group 1"` and `"Group 2"`. The `group` column keeps the
#'   groups in this order.
#' @param var_name Name for the Yes/No variable, in quotes. The default is
#'   `"response"`.
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   counts and proportions, then the z test. `FALSE` prints nothing.
#'
#' @return A data frame with `n1 + n2` rows, in random order, and two
#'   columns: `group` and the Yes/No variable. It is returned invisibly. The
#'   summary is stored inside it; see it again with [dgen_results()].
#'
#' @examples
#' # Did a larger share of the new-layout visitors buy something?
#' visits <- prop_two_gen(n1 = 120, n2 = 150, prop = c(0.25, 0.42),
#'                        labels = c("Old layout", "New layout"),
#'                        var_name = "bought", seed = 1)
#' table(visits$group, visits$bought)
#'
#' # The same test in R: chi-square is z squared
#' chisq.test(table(visits$group, visits$bought), correct = FALSE)
#'
#' @export
prop_two_gen <- function(n1, n2 = n1, prop, labels = NULL, var_name = NULL,
                         seed = NULL, export_csv = FALSE,
                         show_results = TRUE) {

  # 1. Check inputs
  if (missing(n1) || missing(prop)) {
    stop("prop_two_gen needs n1 and prop, for example ",
         "prop_two_gen(n1 = 120, prop = c(0.25, 0.36)).", call. = FALSE)
  }
  need_sample_size(n1, "n1")
  need_sample_size(n2, "n2")
  need_proportions(prop, 2, "prop", "c(0.25, 0.36)")
  group_names <- variable_names(labels, 2, c("Group 1", "Group 2"),
                                "labels", "groups")
  name <- variable_names(var_name, 1, "response")
  use_seed(seed)

  # 2. Build exact data: each proportion as a whole count, one row each
  n     <- c(n1, n2)
  count <- whole_count(n, prop)
  if (sum(count) == 0 || sum(count) == sum(n)) {
    stop("With these sample sizes and proportions, ",
         if (sum(count) == 0) "nobody" else "everybody",
         " in either group would answer \"Yes\", so there is no difference ",
         "to test. Use larger groups, or proportions further from ",
         if (sum(count) == 0) "0." else "1.", call. = FALSE)
  }
  data  <- exact_counts(matrix(c(count, n - count), nrow = 2,
                               dimnames = list(group_names, c("Yes", "No"))))
  names(data) <- c("group", name)

  # 3. Run the test (first group minus second), assemble the summary and
  #    hand everything back
  proportion <- count / n
  pooled     <- sum(count) / sum(n)
  direction  <- paste(group_names[1], "-", group_names[2])
  difference <- proportion[1] - proportion[2]
  z      <- difference / sqrt(pooled * (1 - pooled) * sum(1 / n))
  margin <- stats::qnorm(0.975) * sqrt(sum(proportion * (1 - proportion) / n))
  results <- list(
    title = paste0("Two-proportion z test",
                   if (!is.null(labels)) paste0(": ", and_list(group_names))),
    table = data.frame(
      "n"                   = n,
      "Yes"                 = count,
      "Proportion (target)" = show_number(prop, 3),
      "Proportion (data)"   = show_number(proportion, 3),
      row.names = group_names, check.names = FALSE
    ),
    lines = c(rounding_lines(n, prop, count, "Yes"),
              paste0("Difference in proportions (", direction, ") = ",
                     show_number(difference, 3)),
              z_line(z, "pooled"),
              proportion_ci_line("difference", difference, margin),
              paste0("Cohen's h (", direction, ") = ",
                     show_number(cohens_h(proportion[1], proportion[2]), 2)),
              small_expected_note(outer(n, c(pooled, 1 - pooled)), "z test")),
    note  = data_note(data)
  )
  deliver(data, results, show_results, export_csv, "prop_two_gen")
}
