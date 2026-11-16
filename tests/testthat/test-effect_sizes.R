# Tests for the effect-size-first helper functions







test_that("h_to_prop round-trips through the Cohen's h formula", {
  suppressMessages(res <- h_to_prop(h = 0.5, p1 = 0.60))
  h_check <- 2 * (asin(sqrt(res$p1)) - asin(sqrt(res$p2)))
  expect_equal(h_check, 0.5)
  expect_gt(res$p2, 0)
  expect_lt(res$p2, res$p1)
})

test_that("h_to_prop supports negative h (p2 > p1)", {
  suppressMessages(res <- h_to_prop(h = -0.3, p1 = 0.40))
  expect_gt(res$p2, 0.40)
  h_check <- 2 * (asin(sqrt(res$p1)) - asin(sqrt(res$p2)))
  expect_equal(h_check, -0.3)
})

test_that("h_to_prop rejects infeasible combinations", {
  expect_error(h_to_prop(h = 2.5, p1 = 0.10), "outside")
  expect_error(h_to_prop(h = 0.5, p1 = 1.1), "'p1'")
})
