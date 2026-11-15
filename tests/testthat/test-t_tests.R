# The t-test family: each promise, checked on the returned data.

reported <- function(values, target, decimals) {
  all(abs(values - target) < 10^-(decimals + 1) / 2)
}
pooled_d <- function(first, second) {
  n1 <- length(first)
  n2 <- length(second)
  spread <- sqrt(((n1 - 1) * stats::var(first) + (n2 - 1) * stats::var(second)) /
                   (n1 + n2 - 2))
  (mean(second) - mean(first)) / spread
}

# ---- t_one_gen ---------------------------------------------------------------

test_that("t_one_gen: the mean and SD are exact in the returned data", {
  for (decimals in 0:2) {
    for (n in c(10, 25, 120)) {
      for (seed in 1:4) {
        data <- t_one_gen(n, m = 32.4, sd = 5.5, test_value = 30,
                          decimals = decimals, seed = seed,
                          show_results = FALSE)
        expect_equal(dim(data), c(n, 1))
        expect_true(reported(mean(data$x), 32.4, decimals))
        expect_true(reported(stats::sd(data$x), 5.5, decimals))
        expect_equal(data$x, round(data$x, decimals))
      }
    }
  }
})

test_that("t_one_gen: a typed-in d is exact to 2 decimals, with an exact SD", {
  for (d in c(0.5, -0.8, 0.2, 1.25)) {
    for (decimals in 0:2) {
      for (seed in 1:5) {
        data <- t_one_gen(25, sd = 12, test_value = 100, d = d,
                          decimals = decimals, seed = seed,
                          show_results = FALSE)
        achieved <- (mean(data$x) - 100) / stats::sd(data$x)
        expect_true(abs(achieved - d) < 0.005, info = paste(d, decimals, seed))
        expect_true(reported(stats::sd(data$x), 12, decimals))
      }
    }
  }
})

test_that("t_one_gen: the summary shows the test R would give", {
  shown <- capture.output(
    data <- t_one_gen(25, m = 32.4, sd = 5.5, test_value = 30, decimals = 1,
                      var_name = "minutes", seed = 1))
  test <- stats::t.test(data$minutes, mu = 30)
  expect_true(any(grepl("One-sample t test: minutes against a test value of 30",
                        shown, fixed = TRUE)))
  expect_true(any(grepl("Mean - test value = 2.40", shown, fixed = TRUE)))
  expect_true(any(grepl(t_line(test), shown, fixed = TRUE)))
  expect_true(any(grepl("95% CI for the mean: ", shown, fixed = TRUE)))
  expect_true(any(grepl("Cohen's d = 0.44", shown, fixed = TRUE)))
  with_d <- capture.output(
    t_one_gen(25, sd = 5.5, test_value = 30, d = 0.5, decimals = 1, seed = 1))
  expect_true(any(grepl("Cohen's d: target 0.50, data 0.50", with_d,
                        fixed = TRUE)))
})

test_that("t_one_gen: problems are explained in plain language", {
  expect_error(t_one_gen(25, m = 32, sd = 5), "t_one_gen needs n, sd, test_value")
  expect_error(t_one_gen(25, sd = 5, test_value = 30),
               "Give the mean (m) or Cohen's d (d), but not both", fixed = TRUE)
  expect_error(t_one_gen(25, m = 32, sd = 5, test_value = 30, d = 0.5),
               "but not both")
  expect_error(t_one_gen(25, sd = 5, test_value = 30, d = 0.455),
               "Cohen's d is matched to 2 decimal places, but you asked for 0.455. Use 0.45 or 0.46",
               fixed = TRUE)
  expect_error(t_one_gen(25, m = c(32, 33), sd = 5, test_value = 30),
               "m must be a single number")
  expect_error(t_one_gen(25, m = 32, sd = 5, test_value = c(30, 31)),
               "test_value must be a single number")
  expect_error(t_one_gen(5, m = 75.5, sd = 8.2, test_value = 70, decimals = 0),
               "With 5 whole-number values, the mean cannot be 75.5",
               fixed = TRUE)
})

# ---- t_indep_gen -------------------------------------------------------------

test_that("t_indep_gen: each group's mean and SD are exact", {
  for (decimals in 0:2) {
    for (seed in 1:5) {
      data <- t_indep_gen(n1 = 18, n2 = 30, m = c(72, 78.5), sd = c(9, 10.5),
                          labels = c("Online", "Classroom"),
                          var_name = "score", decimals = decimals,
                          seed = seed, show_results = FALSE)
      expect_identical(class(data), "data.frame")
      expect_named(data, c("group", "score"))
      expect_equal(as.vector(table(data$group)), c(18, 30))
      expect_equal(levels(data$group), c("Online", "Classroom"))
      groups <- split(data$score, data$group)
      expect_true(reported(vapply(groups, mean, 1), c(72, 78.5), decimals))
      expect_true(reported(vapply(groups, stats::sd, 1), c(9, 10.5), decimals))
    }
  }
})

test_that("t_indep_gen: n2 defaults to n1, and the mean difference is exact", {
  data <- t_indep_gen(n1 = 25, m = c(72, 78), sd = c(9, 10), decimals = 0,
                      seed = 1, show_results = FALSE)
  expect_equal(as.vector(table(data$group)), c(25, 25))
  expect_equal(levels(data$group), c("Group 1", "Group 2"))
  groups <- split(data$x, data$group)
  expect_equal(mean(groups[[2]]) - mean(groups[[1]]), 6)
})

test_that("t_indep_gen: a typed-in d is exact to 2 decimals", {
  # Whole numbers with SDs near 10: the classroom case that must work
  for (d in c(0.5, 0.6, -0.3, 0.8)) {
    for (decimals in 0:1) {
      for (seed in 1:5) {
        data <- t_indep_gen(n1 = 25, n2 = 20, m = 72, sd = c(9, 10), d = d,
                            decimals = decimals, seed = seed,
                            show_results = FALSE)
        groups <- split(data$x, data$group)
        expect_true(abs(pooled_d(groups[[1]], groups[[2]]) - d) < 0.005,
                    info = paste(d, decimals, seed))
        expect_true(reported(mean(groups[[1]]), 72, decimals))
        expect_true(reported(vapply(groups, stats::sd, 1), c(9, 10), decimals))
      }
    }
  }
})

test_that("t_indep_gen: the summary shows both t tests R would give", {
  shown <- capture.output(
    data <- t_indep_gen(n1 = 25, n2 = 25, m = c(72, 78), sd = c(9, 10),
                        labels = c("Online", "Classroom"), var_name = "score",
                        decimals = 0, seed = 1))
  groups <- split(data$score, data$group)
  pooled <- stats::t.test(groups$Classroom, groups$Online, var.equal = TRUE)
  welch  <- stats::t.test(groups$Classroom, groups$Online)
  expect_true(any(grepl("Difference in means (Classroom - Online) = 6.0",
                        shown, fixed = TRUE)))
  expect_true(any(grepl(t_line(pooled, "equal variances"), shown,
                        fixed = TRUE)))
  expect_true(any(grepl(t_line(welch, "Welch, unequal variances"), shown,
                        fixed = TRUE)))
  expect_true(any(grepl("Online +25 +72.0 +72.0 +9.0 +9.0", shown)))
  expect_equal(t_line(pooled), "t(48) = 2.22, p = .031")
})

test_that("t_indep_gen: the same data serve the quiet applications", {
  data <- t_indep_gen(n1 = 30, n2 = 30, m = c(50, 56), sd = c(10, 10),
                      decimals = 1, seed = 2, show_results = FALSE)
  t_value <- stats::t.test(x ~ group, data = data, var.equal = TRUE)$statistic
  # point-biserial correlation: r^2 = t^2 / (t^2 + df)
  r <- stats::cor(data$x, as.numeric(data$group))
  expect_equal(r^2, unname(t_value^2 / (t_value^2 + 58)))
  # the F test of two variances runs on the exact SDs
  ratio <- stats::var.test(x ~ group, data = data)$statistic
  expect_equal(unname(ratio), 1, tolerance = 0.02)
})

test_that("t_indep_gen: problems are explained in plain language", {
  expect_error(t_indep_gen(n1 = 25, m = c(72, 78)), "t_indep_gen needs n1, m and sd")
  expect_error(t_indep_gen(25, m = c(72, 78), sd = 9),
               "sd must be 2 numbers, such as sd = c(9, 10)", fixed = TRUE)
  expect_error(t_indep_gen(25, m = 72, sd = c(9, 10)),
               "m must be 2 numbers, such as m = c(72, 78)", fixed = TRUE)
  expect_error(t_indep_gen(25, m = c(72, 78), sd = c(9, 10), d = 0.5),
               "the first group's mean, when d is given")
  expect_error(t_indep_gen(25, m = c(72, 78), sd = c(9, 10), labels = "A"),
               "labels must give a different name for each of the 2 groups",
               fixed = TRUE)
  expect_error(t_indep_gen(n1 = 20, n2 = 5, m = c(72, 75.5), sd = c(9, 8),
                           decimals = 0),
               "Group 2: With 5 whole-number values, the mean cannot be 75.5",
               fixed = TRUE)
  expect_error(t_indep_gen(25, m = 72.25, sd = c(9, 10), d = 0.5, decimals = 0),
               "means are matched to 1 decimal place, but you asked for 72.25",
               fixed = TRUE)
})

# ---- t_paired_gen ------------------------------------------------------------

test_that("t_paired_gen: means, SDs and the correlation are exact", {
  for (decimals in 0:2) {
    for (r in c(0.3, 0.7, -0.4)) {
      for (seed in 1:4) {
        data <- t_paired_gen(n = 30, m = c(50, 55), sd = c(10, 12), r = r,
                             decimals = decimals, seed = seed,
                             show_results = FALSE)
        expect_identical(class(data), "data.frame")
        expect_named(data, c("time1", "time2"))
        expect_equal(nrow(data), 30)
        expect_true(reported(colMeans(data), c(50, 55), decimals))
        expect_true(reported(vapply(data, stats::sd, 1), c(10, 12), decimals))
        expect_true(abs(stats::cor(data$time1, data$time2) - r) < 0.005)
        # the promise rule: both means exact, so the mean difference is too
        expect_equal(mean(data$time2 - data$time1), 5)
      }
    }
  }
})

test_that("t_paired_gen: a typed-in d is exact to 2 decimals", {
  for (d in c(0.5, -0.35, 1)) {
    for (decimals in 0:2) {
      for (seed in 1:4) {
        data <- t_paired_gen(n = 30, m = 50, sd = c(10, 10), r = 0.7, d = d,
                             decimals = decimals, seed = seed,
                             show_results = FALSE)
        differences <- data$time2 - data$time1
        achieved <- mean(differences) / stats::sd(differences)
        expect_true(abs(achieved - d) < 0.005, info = paste(d, decimals, seed))
        expect_true(reported(mean(data$time1), 50, decimals))
        expect_true(abs(stats::cor(data$time1, data$time2) - 0.7) < 0.005)
      }
    }
  }
})

test_that("t_paired_gen: the summary shows the paired test R would give", {
  shown <- capture.output(
    data <- t_paired_gen(n = 30, m = c(50, 55), sd = c(10, 10), r = 0.7,
                         var_name = c("before", "after"), decimals = 1,
                         seed = 1))
  test <- stats::t.test(data$after, data$before, paired = TRUE)
  expect_true(any(grepl("Paired t test: before and after", shown)))
  expect_true(any(grepl(
    "Correlation between before and after: target 0.70, data 0.70", shown,
    fixed = TRUE)))
  expect_true(any(grepl("Differences (after - before): mean = 5.00, SD = ",
                        shown, fixed = TRUE)))
  expect_true(any(grepl(t_line(test), shown, fixed = TRUE)))
  # d for a paired design is t / sqrt(n)
  d <- mean(data$after - data$before) / stats::sd(data$after - data$before)
  expect_equal(d, unname(test$statistic) / sqrt(30))
})

test_that("t_paired_gen: problems are explained in plain language", {
  expect_error(t_paired_gen(30, m = c(50, 55), sd = c(10, 10)),
               "t_paired_gen needs n, m, sd and r")
  expect_error(t_paired_gen(30, m = c(50, 55), sd = 10, r = 0.7),
               "sd must be 2 numbers")
  expect_error(t_paired_gen(30, m = c(50, 55), sd = c(10, 10), r = 1),
               "between -1 and 1")
  expect_error(t_paired_gen(30, m = c(50, 55), sd = c(10, 10), r = 0.7,
                            var_name = "score"),
               "var_name must give a different name for each of the 2 variables",
               fixed = TRUE)
  expect_error(t_paired_gen(30, m = 50, sd = c(10, 10), r = 0.7),
               "m must be 2 numbers")
})

# ---- Shared behaviour --------------------------------------------------------

test_that("every t function is reproducible, silent on request, and invisible", {
  calls <- list(
    function() t_one_gen(20, m = 32, sd = 5, test_value = 30, seed = 9,
                         show_results = FALSE),
    function() t_indep_gen(20, m = c(72, 78), sd = c(9, 10), seed = 9,
                           show_results = FALSE),
    function() t_paired_gen(20, m = c(50, 55), sd = c(10, 10), r = 0.6,
                            seed = 9, show_results = FALSE)
  )
  for (call in calls) {
    expect_silent(first <- withVisible(call()))
    expect_false(first$visible)
    expect_identical(first$value, call())
    expect_s3_class(dgen_results(first$value), "dgen_results")
  }
})
