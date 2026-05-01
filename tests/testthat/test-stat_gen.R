test_that("stat_gen returns correct structure", {
  result <- stat_gen(n = 50, m = 10, sd = 2, seed = 1)
  expect_named(result, c("data", "targets", "achieved", "iterations"))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 50)
})

test_that("stat_gen achieves exact mean and SD (rounded)", {
  result <- stat_gen(n = 100, m = 5.5, sd = 1.2, decimals = 1, seed = 42)
  x <- result$data[[1]]
  expect_equal(round(mean(x), 1), 5.5)
  expect_equal(round(sd(x), 1), 1.2)
})

test_that("stat_gen seed reproducibility", {
  r1 <- stat_gen(n = 50, m = 0, sd = 1, seed = 99)
  r2 <- stat_gen(n = 50, m = 0, sd = 1, seed = 99)
  expect_identical(r1$data, r2$data)
})

test_that("stat_gen input validation", {
  expect_error(stat_gen(n = 0, m = 5, sd = 1))
  expect_error(stat_gen(n = 10, m = 5, sd = -1))
})

test_that("stat_gen neg_data flag produces non-negative values", {
  result <- stat_gen(n = 200, m = 5, sd = 2, neg_data = FALSE, seed = 7)
  expect_true(all(result$data[[1]] >= 0))
})

test_that("stat_gen export_csv writes a file", {
  tmp <- tempdir()
  old <- setwd(tmp)
  on.exit(setwd(old))
  stat_gen(n = 20, m = 3, sd = 1, export_csv = TRUE, seed = 1)
  expect_true(file.exists(file.path(tmp, "Variable_X_dgen.csv")))
})
