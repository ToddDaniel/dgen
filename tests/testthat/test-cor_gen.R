# cor_gen(): the promise, checked on the returned data.

expect_reported <- function(values, target, decimals) {
  expect_true(all(abs(values - target) < 10^-(decimals + 1) / 2))
}

test_that("two variables have exactly the means, SDs and correlation", {
  for (decimals in 0:2) {
    for (n in c(10, 30, 100)) {
      for (r in c(0, 0.5, -0.8)) {
        for (seed in 1:3) {
          data <- cor_gen(n, m = c(50, 55.5), sd = c(10, 12.5), r = r,
                          decimals = decimals, seed = seed,
                          show_results = FALSE)
          expect_reported(colMeans(data), c(50, 55.5), decimals)
          expect_reported(vapply(data, stats::sd, 1), c(10, 12.5), decimals)
          expect_true(abs(stats::cor(data$x, data$y) - r) < 0.005)
          expect_equal(data, round(data, decimals))
        }
      }
    }
  }
})

test_that("Morse's example is exact in the rounded data", {
  # n = 14; the 0.1.0 cor_gen returned r = .80 for this .68 target
  data <- cor_gen(14, m = c(62.93, 68.36), sd = c(6.34, 8.42), r = 0.68,
                  decimals = 2, var_name = c("Pre", "Post"), seed = 4,
                  show_results = FALSE)
  expect_reported(colMeans(data), c(62.93, 68.36), 2)
  expect_reported(vapply(data, stats::sd, 1), c(6.34, 8.42), 2)
  expect_equal(round(stats::cor(data$Pre, data$Post), 2), 0.68)
})

test_that("a correlation table gives three or more exact variables", {
  table <- matrix(c(1, 0.4, 0.3,
                    0.4, 1, 0.2,
                    0.3, 0.2, 1), nrow = 3)
  m  <- c(2.8, 2.6, 3)
  sd <- c(0.9, 0.85, 1)
  for (seed in 1:5) {
    data <- cor_gen(100, m, sd, r = table, seed = seed, show_results = FALSE)
    expect_equal(dim(data), c(100, 3))
    expect_named(data, c("x1", "x2", "x3"))
    expect_reported(colMeans(data), m, 2)
    expect_reported(vapply(data, stats::sd, 1), sd, 2)
    pairs <- upper.tri(table)
    expect_true(all(abs(stats::cor(data)[pairs] - table[pairs]) < 0.005))
  }
})

test_that("a published correlation table can be replicated with lm()", {
  table <- matrix(c(1, 0.4, 0.5,
                    0.4, 1, 0.6,
                    0.5, 0.6, 1), nrow = 3)
  data <- cor_gen(200, m = c(10, 20, 30), sd = c(2, 4, 5), r = table,
                  var_name = c("a", "b", "y"), seed = 1,
                  show_results = FALSE)
  fit <- stats::lm(scale(y) ~ scale(a) + scale(b), data = data)
  # standardized betas implied by the correlation table
  implied <- solve(table[1:2, 1:2], table[1:2, 3])
  expect_equal(unname(stats::coef(fit)[2:3]), implied, tolerance = 0.03)
})

test_that("cor_gen returns a plain data frame, invisibly, and is reproducible", {
  out <- withVisible(cor_gen(30, c(50, 55), c(10, 10), 0.7, seed = 1,
                             show_results = FALSE))
  expect_false(out$visible)
  expect_identical(class(out$value), "data.frame")
  expect_named(out$value, c("x", "y"))
  again <- cor_gen(30, c(50, 55), c(10, 10), 0.7, seed = 1,
                   show_results = FALSE)
  expect_identical(out$value, again)
})

test_that("show_results prints each correlation with its test", {
  expect_silent(cor_gen(40, c(6.5, 78), c(2.1, 9.5), 0.55, decimals = 1,
                        seed = 1, show_results = FALSE))
  shown <- capture.output(
    data <- cor_gen(40, c(6.5, 78), c(2.1, 9.5), 0.55, decimals = 1,
                    var_name = c("hours", "exam"), seed = 1))
  expect_true(any(grepl("Correlation between hours and exam", shown)))
  expect_true(any(grepl("hours +40 +6.50 +6.50 +2.10 +2.10", shown)))
  expect_true(any(grepl("hours and exam +0.55 +0.55", shown)))

  test <- stats::cor.test(data$hours, data$exam)
  pairs <- correlation_table(data, matrix(c(1, 0.55, 0.55, 1), 2))
  expect_equal(pairs$t, show_number(unname(test$statistic), 2))
  expect_equal(pairs$df, 38)
  expect_equal(pairs$p, "< .001")
  expect_equal(show_p(c(0.0312, 0.5, 0.0004)), c(".031", ".500", "< .001"))
})

test_that("problems are explained in plain language", {
  expect_error(cor_gen(30, c(50, 55), c(10, 10)),
               "cor_gen needs n, m, sd and r", fixed = TRUE)
  expect_error(cor_gen(30, 50, 10, 0.5), "two or more variables")
  expect_error(cor_gen(30, c(50, 55), c(10, 10), 1), "between -1 and 1")
  expect_error(cor_gen(30, c(50, 55), c(10, 10), 0.455), "Use 0.45 or 0.46",
               fixed = TRUE)
  expect_error(cor_gen(30, c(50, 55, 60), c(10, 10, 10), 0.5),
               "3 x 3 correlation table", fixed = TRUE)
  expect_error(cor_gen(30, c(50, 55), c(10, 10), 0.5, var_name = "a"),
               "a different name for each of the 2 variables", fixed = TRUE)
  expect_error(cor_gen(3, c(50, 55), c(10, 10), 0.5),
               "n must be at least 4 for 2 variables", fixed = TRUE)
})
