# prop_one_gen(), prop_two_gen() and chisq_gen(): the promise (exact counts)
# and the textbook tests, checked on the returned data.

# ---- prop_one_gen ------------------------------------------------------------

test_that("prop_one_gen: the count in the data is exactly n x prop", {
  for (seed in 1:5) {
    data <- prop_one_gen(n = 80, prop = 0.7, test_value = 0.5, seed = seed,
                         show_results = FALSE)
    expect_identical(class(data), "data.frame")
    expect_named(data, "response")
    expect_equal(nrow(data), 80)
    expect_equal(as.vector(table(data$response)), c(56, 24))
    expect_equal(levels(data$response), c("Yes", "No"))
  }
  named <- prop_one_gen(n = 200, prop = 0.045, test_value = 0.03,
                        labels = c("Defective", "Good"), var_name = "item",
                        seed = 1, show_results = FALSE)
  expect_named(named, "item")
  expect_equal(as.vector(table(named$item)), c(9, 191))
})

test_that("prop_one_gen: the summary is the textbook z test", {
  shown <- capture.output(
    data <- prop_one_gen(n = 80, prop = 0.7, test_value = 0.5,
                         labels = c("Recommend", "Not"), var_name = "answer",
                         seed = 1))
  z <- (0.7 - 0.5) / sqrt(0.5 * 0.5 / 80)
  expect_true(any(grepl(
    "One-proportion z test: proportion of Recommend against a test value of 0.5",
    shown, fixed = TRUE)))
  expect_true(any(grepl("Recommend +56 +0.700 +0.700", shown)))
  expect_true(any(grepl(paste0("z = ", show_number(z, 2), ", p < .001"),
                        shown, fixed = TRUE)))
  expect_true(any(grepl("95% CI for the proportion: 0.600 to 0.800", shown,
                        fixed = TRUE)))
  # the exact binomial p-value, for JASP users
  expect_true(any(grepl("Exact binomial test (JASP's default): p < .001",
                        shown, fixed = TRUE)))
  middling <- dgen_results(prop_one_gen(40, 0.6, 0.5, seed = 1,
                                        show_results = FALSE))
  expect_true(paste0("Exact binomial test (JASP's default): p ",
                     p_text(stats::binom.test(24, 40, 0.5)$p.value)) %in%
                middling$lines)
  expect_true(any(grepl("Cohen's h = 0.41", shown, fixed = TRUE)))
  expect_false(any(grepl("Note:", shown)))

  # the same z as R's test without continuity correction (chi-square = z^2)
  r_test <- stats::prop.test(56, 80, p = 0.5, correct = FALSE)
  expect_equal(unname(r_test$statistic), z^2)
  expect_equal(cohens_h(0.7, 0.5), 2 * asin(sqrt(0.7)) - 2 * asin(sqrt(0.5)))
})

test_that("prop_one_gen: a proportion that is not a whole count is rounded and reported", {
  shown <- capture.output(
    data <- prop_one_gen(n = 25, prop = 0.33, test_value = 0.1, seed = 1))
  expect_equal(as.vector(table(data$response)), c(8, 17))
  expect_true(any(grepl(
    "0.33 of 25 is 8.25, not a whole number, so the nearest count is used: 8 Yes (proportion 0.320).",
    shown, fixed = TRUE)))
  expect_true(any(grepl("Yes +8 +0.330 +0.320", shown)))
  # expected counts are 2.5 and 22.5: a note, not a warning
  expect_true(any(grepl(
    "Note: an expected count is below 5 (the smallest is 2.5), so the z test is only approximate",
    shown, fixed = TRUE)))
  expect_no_warning(prop_one_gen(25, 0.33, 0.1, seed = 1,
                                 show_results = FALSE))
  # a half rounds up
  half <- prop_one_gen(25, 0.5, 0.4, seed = 1, show_results = FALSE)
  expect_equal(as.vector(table(half$response)), c(13, 12))
})

# ---- prop_two_gen ------------------------------------------------------------

test_that("prop_two_gen: each group's count is exactly n x prop", {
  for (seed in 1:5) {
    data <- prop_two_gen(n1 = 120, n2 = 150, prop = c(0.25, 0.36),
                         labels = c("Old layout", "New layout"),
                         var_name = "bought", seed = seed,
                         show_results = FALSE)
    expect_identical(class(data), "data.frame")
    expect_named(data, c("group", "bought"))
    expect_equal(nrow(data), 270)
    counts <- table(data$group, data$bought)
    expect_equal(unname(counts["Old layout", ]), c(30, 90))
    expect_equal(unname(counts["New layout", ]), c(54, 96))
    expect_equal(colnames(counts), c("Yes", "No"))
  }
  equal <- prop_two_gen(n1 = 50, prop = c(0.4, 0.6), seed = 1,
                        show_results = FALSE)
  expect_equal(as.vector(table(equal$group)), c(50, 50))
})

test_that("prop_two_gen: the summary is the textbook pooled z test", {
  shown <- capture.output(
    data <- prop_two_gen(n1 = 120, n2 = 150, prop = c(0.25, 0.36),
                         labels = c("Old layout", "New layout"),
                         var_name = "bought", seed = 1))
  pooled <- (30 + 54) / 270
  z <- (0.36 - 0.25) / sqrt(pooled * (1 - pooled) * (1 / 120 + 1 / 150))
  expect_true(any(grepl("Two-proportion z test: Old layout and New layout",
                        shown)))
  expect_true(any(grepl("New layout +150 +54 +0.360 +0.360", shown)))
  expect_true(any(grepl(
    "Difference in proportions (New layout - Old layout) = 0.110", shown,
    fixed = TRUE)))
  expect_true(any(grepl(paste0("z = ", show_number(z, 2), ", p = .052"),
                        shown, fixed = TRUE)))
  expect_true(any(grepl("Cohen's h = 0.24", shown, fixed = TRUE)))

  # z squared is the chi-square of the 2 x 2 table, without correction
  chi <- stats::chisq.test(table(data$group, data$bought), correct = FALSE)
  expect_equal(unname(chi$statistic), z^2)
  r_test <- stats::prop.test(c(54, 30), c(150, 120), correct = FALSE)
  expect_true(any(grepl(
    paste0("95% CI for the difference: ", show_number(r_test$conf.int[1], 3),
           " to ", show_number(r_test$conf.int[2], 3)), shown, fixed = TRUE)))
})

test_that("prop_two_gen: rounding and small expected counts are reported", {
  shown <- capture.output(
    prop_two_gen(n1 = 15, n2 = 18, prop = c(0.1, 0.3), seed = 1))
  expect_true(any(grepl("0.1 of 15 is 1.5, not a whole number", shown,
                        fixed = TRUE)))
  expect_true(any(grepl("0.3 of 18 is 5.4, not a whole number", shown,
                        fixed = TRUE)))
  expect_true(any(grepl("Note: an expected count is below 5", shown)))
})

# ---- chisq_gen: goodness of fit ----------------------------------------------

test_that("chisq_gen: the data tabulate back to exactly the counts", {
  counts <- c(Red = 18, Blue = 31, Green = 0, Other = 11)
  for (seed in 1:5) {
    data <- chisq_gen(counts, seed = seed, show_results = FALSE)
    expect_identical(class(data), "data.frame")
    expect_named(data, "category")
    expect_equal(c(table(data$category)), counts)
  }
  first  <- chisq_gen(counts, seed = 1, show_results = FALSE)
  second <- chisq_gen(counts, seed = 2, show_results = FALSE)
  expect_false(identical(first$category, second$category))   # order differs
})

test_that("chisq_gen: goodness of fit is Pearson's chi-square with Cohen's w", {
  shown <- capture.output(
    data <- chisq_gen(counts = c(Happy = 56, Tragic = 24),
                      expected = c(0.5, 0.5), var_name = "ending", seed = 1))
  chi <- sum((c(56, 24) - 40)^2 / 40)
  expect_true(any(grepl("Chi-square goodness-of-fit test: ending", shown)))
  expect_true(any(grepl("Happy +56 +0.500 +40.0", shown)))
  expect_true(any(grepl(paste0("Chi-square(1) = ", show_number(chi, 2),
                               ", p < .001"), shown, fixed = TRUE)))
  expect_true(any(grepl(paste0("Cohen's w = ",
                               show_number(sqrt(chi / 80), 2)), shown,
                        fixed = TRUE)))

  # unequal expected proportions; equal is the default
  unequal <- dgen_results(chisq_gen(c(A = 30, B = 50, C = 20),
                                    expected = c(0.2, 0.5, 0.3), seed = 1,
                                    show_results = FALSE))
  expect_equal(unequal$table[["Expected count"]], c("20.0", "50.0", "30.0"))
  expect_equal(unequal$lines[1], "Chi-square(2) = 8.33, p = .016")
  default <- dgen_results(chisq_gen(c(A = 30, B = 50, C = 20), seed = 1,
                                    show_results = FALSE))
  expect_equal(default$table[["Expected proportion"]],
               rep("0.333", 3))
})

# ---- chisq_gen: independence ---------------------------------------------------

test_that("chisq_gen: a table of counts gives two columns and the same table", {
  counts <- matrix(c(30, 10, 5,
                     20, 40, 15), nrow = 2, byrow = TRUE,
                   dimnames = list(c("Online", "Classroom"),
                                   c("Pass", "Fail", "Withdrew")))
  data <- chisq_gen(counts, var_name = c("mode", "result"), seed = 2,
                    show_results = FALSE)
  expect_equal(dim(data), c(120, 2))
  expect_named(data, c("mode", "result"))
  rebuilt <- table(data$mode, data$result)
  expect_equal(matrix(rebuilt, nrow = 2), unname(counts))
  expect_equal(rownames(rebuilt), c("Online", "Classroom"))
  expect_equal(colnames(rebuilt), c("Pass", "Fail", "Withdrew"))
})

test_that("chisq_gen: independence is Pearson's chi-square with Cramer's V", {
  counts <- matrix(c(30, 10,
                     20, 40), nrow = 2, byrow = TRUE,
                   dimnames = list(c("Online", "Classroom"),
                                   c("Pass", "Fail")))
  shown <- capture.output(
    data <- chisq_gen(counts, var_name = c("mode", "result"), seed = 2))
  test <- stats::chisq.test(counts, correct = FALSE)
  expect_true(any(grepl("Chi-square test of independence: mode and result",
                        shown)))
  expect_true(any(grepl("Online +30 +10 +40", shown)))
  expect_true(any(grepl("Total +50 +50 +100", shown)))
  expect_true(any(grepl("Classroom +30.0 +30.0", shown)))
  expect_true(any(grepl(paste0("Chi-square(1) = ",
                               show_number(test$statistic, 2), ", p < .001"),
                        shown, fixed = TRUE)))
  expect_true(any(grepl(paste0("Cramer's V = ",
                               show_number(sqrt(test$statistic / 100), 2)),
                        shown, fixed = TRUE)))

  # a 3 x 2 table: V divides by the smaller of (rows - 1, columns - 1)
  bigger <- matrix(c(12, 5, 7, 3, 8, 15), nrow = 3)
  results <- dgen_results(chisq_gen(bigger, seed = 1, show_results = FALSE))
  chi <- stats::chisq.test(bigger, correct = FALSE)$statistic
  expect_true(any(grepl(paste0("Cramer's V = ",
                               show_number(sqrt(chi / 50), 2)),
                        results$lines, fixed = TRUE)))
})

test_that("chisq_gen: small expected counts give a note, never a warning", {
  expect_no_warning(shown <- capture.output(
    chisq_gen(c(A = 3, B = 9, C = 4), expected = c(0.2, 0.5, 0.3), seed = 1)))
  expect_true(any(grepl(
    "Note: an expected count is below 5 (the smallest is 3.2), so the chi-square test is only approximate",
    shown, fixed = TRUE)))
  expect_no_warning(small <- capture.output(
    chisq_gen(matrix(c(3, 2, 4, 6), 2), seed = 1)))
  expect_true(any(grepl("Note: an expected count is below 5", small)))
})

# ---- Shared behaviour ----------------------------------------------------------

test_that("the categorical functions are reproducible, silent and invisible", {
  calls <- list(
    function() prop_one_gen(80, 0.7, 0.5, seed = 3, show_results = FALSE),
    function() prop_two_gen(60, prop = c(0.4, 0.6), seed = 3,
                            show_results = FALSE),
    function() chisq_gen(c(A = 30, B = 50), seed = 3, show_results = FALSE)
  )
  for (call in calls) {
    expect_silent(first <- withVisible(call()))
    expect_false(first$visible)
    expect_identical(first$value, call())
    expect_s3_class(dgen_results(first$value), "dgen_results")
  }
})

test_that("titles never show default names", {
  title <- function(data) dgen_results(data)$title
  expect_equal(title(prop_one_gen(80, 0.7, 0.5, seed = 1,
                                  show_results = FALSE)),
               "One-proportion z test against a test value of 0.5")
  expect_equal(title(prop_two_gen(60, prop = c(0.4, 0.6), seed = 1,
                                  show_results = FALSE)),
               "Two-proportion z test")
  expect_equal(title(chisq_gen(c(A = 30, B = 50), seed = 1,
                               show_results = FALSE)),
               "Chi-square goodness-of-fit test")
  expect_equal(title(chisq_gen(matrix(c(30, 10, 20, 40), 2), seed = 1,
                               show_results = FALSE)),
               "Chi-square test of independence")
})

test_that("problems are explained in plain language", {
  expect_error(prop_one_gen(80, 0.7), "prop_one_gen needs n, prop and test_value")
  expect_error(prop_one_gen(80, 70, 0.5),
               "prop must be a proportion between 0 and 1 (not a percentage), such as prop = 0.7",
               fixed = TRUE)
  expect_error(prop_one_gen(80, 0.7, 1), "test_value must be a proportion")
  expect_error(prop_one_gen(80.5, 0.7, 0.5), "single whole number of 2 or more")
  expect_error(prop_one_gen(80, 0.7, 0.5, labels = "Yes"),
               "labels must give a different name for each of the 2 categories",
               fixed = TRUE)
  expect_error(prop_two_gen(prop = c(0.4, 0.6)), "prop_two_gen needs n1 and prop")
  expect_error(prop_two_gen(60, prop = 0.4),
               "prop must be 2 proportions between 0 and 1", fixed = TRUE)
  expect_error(chisq_gen(), "chisq_gen needs counts")
  expect_error(chisq_gen(c(A = 5.5, B = 3)), "counts must be whole numbers")
  expect_error(chisq_gen(c(A = 30)), "at least two categories")
  expect_error(chisq_gen(c(A = 30, B = 50), expected = c(0.5, 0.6)),
               "expected must give one proportion for each of the 2 categories, each above 0 and adding up to 1",
               fixed = TRUE)
  expect_error(chisq_gen(c(A = 30, B = 50), expected = c(0.2, 0.3, 0.5)),
               "expected must give one proportion")
  expect_error(chisq_gen(matrix(c(30, 10, 20, 40), 2), expected = c(0.5, 0.5)),
               "expected is only for a goodness-of-fit test")
  expect_error(chisq_gen(matrix(c(30, 0, 20, 0), 2)),
               "at least one observation in every row and every column")
  expect_error(chisq_gen(c(A = 30, B = 50), var_name = c("a", "b")),
               "var_name must give one name in quotes")
})
