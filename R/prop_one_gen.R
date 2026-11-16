# prop_one_gen(): data for a one-proportion z test.
# Method: the proportion becomes a whole count, the engine writes out one
# row per observation in random order, and the z test is calculated from
# the counts.

#' Generate data for a one-proportion z test
#'
#' Creates a two-category variable in which exactly the proportion you
#' choose falls in the first category, and tests that proportion against a
#' test value (the proportion under the null hypothesis).
#'
#' @details
#' **What is exact.** The number of observations in the first category is
#' `n * prop`. When that is not a whole number, the nearest whole count is
#' used and the summary says so. The z statistic, p-value, confidence
#' interval and effect size are calculated from the data and shown in the
#' summary; they are not targets.
#'
#' **The test.** The summary shows the textbook z test,
#' z = (p - `test_value`) / sqrt(`test_value` * (1 - `test_value`) / n),
#' with no continuity correction, so it matches a hand calculation. The
#' confidence interval is the usual one, p plus or minus 1.96 standard
#' errors. If an expected count is below 5 the summary adds a note.
#'
#' **Effect size.** Cohen's h compares the sample proportion with the test
#' value on a scale where 0.2 is small, 0.5 medium and 0.8 large.
#'
#' The same data serve a chi-square goodness-of-fit test with two
#' categories: its chi-square equals z squared.
#'
#' @inheritParams dgen
#' @param n Sample size: the number of rows.
#' @param prop Target proportion in the first category, between 0 and 1,
#'   such as `0.7`.
#' @param test_value The proportion under the null hypothesis: the value
#'   the sample proportion is tested against.
#' @param labels Names of the two categories, in quotes; `prop` refers to
#'   the first. The default is `c("Yes", "No")`.
#' @param var_name Name for the variable, in quotes. The default is
#'   `"response"`.
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   counts and proportions, then the z test. `FALSE` prints nothing.
#'
#' @return A data frame with `n` rows and one column of category labels, in
#'   random order, returned invisibly. The summary is stored inside it; see
#'   it again with [dgen_results()].
#'
#' @examples
#' # 70% of 80 customers would recommend the store; is that more than half?
#' survey <- prop_one_gen(n = 80, prop = 0.7, test_value = 0.5,
#'                        labels = c("Recommend", "Not"),
#'                        var_name = "answer", seed = 1)
#' table(survey$answer)
#'
#' @export
prop_one_gen <- function(n, prop, test_value, labels = NULL, var_name = NULL,
                         seed = NULL, export_csv = FALSE,
                         show_results = TRUE) {

  # 1. Check inputs
  if (missing(n) || missing(prop) || missing(test_value)) {
    stop("prop_one_gen needs n, prop and test_value, for example ",
         "prop_one_gen(n = 80, prop = 0.7, test_value = 0.5).", call. = FALSE)
  }
  need_sample_size(n, "n")
  need_proportions(prop, 1, "prop", "0.7")
  need_proportions(test_value, 1, "test_value", "0.5")
  categories <- variable_names(labels, 2, c("Yes", "No"), "labels",
                               "categories")
  name <- variable_names(var_name, 1, "response")
  use_seed(seed)

  # 2. Build exact data: the proportion as a whole count, one row each
  count <- whole_count(n, prop)
  data  <- exact_counts(stats::setNames(c(count, n - count), categories))
  names(data) <- name

  # 3. Run the test, assemble the summary and hand everything back
  proportion <- count / n
  z      <- (proportion - test_value) / sqrt(test_value * (1 - test_value) / n)
  margin <- stats::qnorm(0.975) * sqrt(proportion * (1 - proportion) / n)
  results <- list(
    title = paste0("One-proportion z test",
                   if (!is.null(labels)) paste0(": proportion of ",
                                                categories[1]),
                   " against a test value of ", format(test_value)),
    table = data.frame(
      "Count"               = c(count, n - count),
      "Proportion (target)" = show_number(c(prop, 1 - prop), 3),
      "Proportion (data)"   = show_number(c(proportion, 1 - proportion), 3),
      row.names = categories, check.names = FALSE
    ),
    lines = c(rounding_lines(n, prop, count, categories[1]),
              z_line(z),
              paste0("95% CI for the proportion: ",
                     show_number(proportion - margin, 3), " to ",
                     show_number(proportion + margin, 3)),
              paste0("Cohen's h = ",
                     show_number(cohens_h(proportion, test_value), 2)),
              small_expected_note(n * c(test_value, 1 - test_value),
                                  "z test")),
    note  = data_note(data)
  )
  deliver(data, results, show_results, export_csv, "prop_one_gen")
}
