# chisq_gen(): data for a chi-square test.
# Method: the engine writes out one row per observation in random order, so
# the counts in the data are exactly the counts typed in. A list of counts
# gives a goodness-of-fit test; a table of counts gives a test of
# independence.

#' Generate data for a chi-square test
#'
#' Turns the counts you choose into raw data, one row per observation, and
#' runs the chi-square test. A named list of counts gives a goodness-of-fit
#' test; a table of counts gives a test of independence.
#'
#' @details
#' **What is exact.** The counts. Tabulating the returned data gives back
#' exactly the counts you typed in; only the order of the rows is random.
#' The chi-square statistic, p-value and effect size are calculated from
#' those counts and shown in the summary.
#'
#' **Goodness of fit.** Give `counts` as a named set of numbers, such as
#' `c(Happy = 56, Tragic = 24)`. `expected` holds the proportions expected
#' under the null hypothesis, in the same order; leave it out to test
#' against equal proportions. The effect size is Cohen's w, the square root
#' of chi-square divided by the sample size (0.1 small, 0.3 medium, 0.5
#' large).
#'
#' **Independence.** Give `counts` as a table: a matrix with one row for
#' each category of the first variable and one column for each category of
#' the second. The data then have two columns. The effect size is Cramer's
#' V, which runs from 0 (no relationship) to 1.
#'
#' **The test.** The summary shows Pearson's chi-square with no continuity
#' correction, so it matches a hand calculation, Excel's `CHISQ.TEST` and
#' JASP. In R, `chisq.test()` applies a continuity correction to a 2 x 2
#' table unless you add `correct = FALSE`, so its default result for such a
#' table is a little smaller than the summary's. If an expected count is
#' below 5 the summary adds a note.
#'
#' @inheritParams dgen
#' @param counts Observed counts. For goodness of fit, a named set of whole
#'   numbers, one per category. For independence, a matrix of whole numbers
#'   with row and column names.
#' @param expected For goodness of fit only: the expected proportions, one
#'   per category, adding up to 1. The default is equal proportions.
#' @param var_name Name for the variable, in quotes; for a test of
#'   independence, two names (the row variable, then the column variable).
#'   The default is `"category"`, or `"row"` and `"column"`.
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   observed and expected counts, then the chi-square test. `FALSE` prints
#'   nothing.
#'
#' @return A data frame with one row per observation, in random order, and
#'   one column (goodness of fit) or two (independence) of category labels.
#'   It is returned invisibly. The summary is stored inside it; see it again
#'   with [dgen_results()].
#'
#' @examples
#' # Goodness of fit: do viewers prefer one ending?
#' endings <- chisq_gen(counts = c(Happy = 56, Tragic = 24),
#'                      expected = c(0.5, 0.5), var_name = "ending",
#'                      seed = 1)
#' table(endings$ending)
#'
#' # Independence: is the result related to how the course was taken?
#' results <- matrix(c(30, 10,
#'                     20, 40), nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("Online", "Classroom"),
#'                                   c("Pass", "Fail")))
#' course <- chisq_gen(counts = results, var_name = c("mode", "result"),
#'                     seed = 2)
#' table(course$mode, course$result)
#' chisq.test(table(course$mode, course$result), correct = FALSE)
#'
#' @export
chisq_gen <- function(counts, expected = NULL, var_name = NULL, seed = NULL,
                      export_csv = FALSE, show_results = TRUE) {

  # 1. Check inputs
  if (missing(counts)) {
    stop("chisq_gen needs counts, for example ",
         "chisq_gen(counts = c(Happy = 56, Tragic = 24)).", call. = FALSE)
  }
  is_table <- is.matrix(counts)
  names <- variable_names(var_name, if (is_table) 2 else 1,
                          if (is_table) c("row", "column") else "category")
  use_seed(seed)

  # 2. Build exact data: one row per observation
  data <- exact_counts(counts)
  names(data) <- names

  # 3. Run the test, assemble the summary and hand everything back
  results <- if (is_table) independence_results(data, expected, var_name)
             else goodness_of_fit_results(data, expected, var_name)
  results$note <- data_note(data)
  deliver(data, results, show_results, export_csv, "chisq_gen")
}

# Goodness of fit: observed counts against expected proportions.
goodness_of_fit_results <- function(data, expected, var_name) {
  observed     <- table(data[[1]])
  n_categories <- length(observed)
  if (n_categories < 2) {
    stop("A goodness-of-fit test needs at least two categories, such as ",
         "counts = c(Happy = 56, Tragic = 24).", call. = FALSE)
  }
  if (is.null(expected)) expected <- rep(1 / n_categories, n_categories)
  if (!is_number(expected) || length(expected) != n_categories ||
      any(expected <= 0) || abs(sum(expected) - 1) > 1e-6) {
    stop("expected must give one proportion for each of the ", n_categories,
         " categories, each above 0 and adding up to 1, such as ",
         "expected = c(0.5, 0.5).", call. = FALSE)
  }

  # correct = FALSE and no warning: the note below replaces R's warning
  test <- suppressWarnings(stats::chisq.test(observed, p = expected,
                                             correct = FALSE))
  list(
    title = paste0("Chi-square goodness-of-fit test",
                   if (!is.null(var_name)) paste0(": ", var_name)),
    table = data.frame(
      "Observed"            = as.vector(observed),
      "Expected proportion" = show_number(expected, 3),
      "Expected count"      = show_number(as.vector(test$expected), 1),
      row.names = names(observed), check.names = FALSE
    ),
    lines = c(chisq_line(test),
              paste0("Cohen's w = ",
                     show_number(sqrt(test$statistic / sum(observed)), 2)),
              small_expected_note(test$expected, "chi-square test"))
  )
}

# Independence: a two-way table of observed counts.
independence_results <- function(data, expected, var_name) {
  if (!is.null(expected)) {
    stop("expected is only for a goodness-of-fit test. For a table of ",
         "counts, the expected counts come from the row and column totals.",
         call. = FALSE)
  }
  observed <- table(data[[1]], data[[2]])
  if (any(dim(observed) < 2) || any(rowSums(observed) == 0) ||
      any(colSums(observed) == 0)) {
    stop("A test of independence needs a table with at least two rows and ",
         "two columns, and at least one observation in every row and ",
         "every column.", call. = FALSE)
  }

  test <- suppressWarnings(stats::chisq.test(observed, correct = FALSE))
  smaller_side <- min(dim(observed)) - 1
  expected_counts <- as.data.frame.matrix(
    matrix(show_number(test$expected, 1), nrow = nrow(observed),
           dimnames = dimnames(unclass(observed))))
  list(
    title = paste0("Chi-square test of independence",
                   if (!is.null(var_name)) paste0(": ", and_list(var_name))),
    table = as.data.frame.matrix(stats::addmargins(
      observed, FUN = list(Total = sum), quiet = TRUE)),
    lines = c("Expected counts if the two variables were unrelated:",
              utils::capture.output(print(expected_counts)),
              "",
              chisq_line(test),
              paste0("Cramer's V = ",
                     show_number(sqrt(test$statistic /
                                        (sum(observed) * smaller_side)), 2)),
              small_expected_note(test$expected, "chi-square test"))
  )
}

# "Chi-square(1) = 12.80, p < .001", from the result of chisq.test()
chisq_line <- function(test) {
  paste0("Chi-square(", unname(test$parameter), ") = ",
         show_number(unname(test$statistic), 2), ", p ", p_text(test$p.value))
}
