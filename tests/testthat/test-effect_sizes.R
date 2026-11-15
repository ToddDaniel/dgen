# Tests for the effect-size-first helper functions







test_that("f_to_means hits the population f exactly", {
  suppressMessages(res <- f_to_means(f = 0.25, k = 3, sd = 10,
                                     grand_mean = 50))
  expect_length(res$means, 3)
  expect_equal(mean(res$means), 50)
  sigma_means <- sqrt(mean((res$means - 50)^2))
  expect_equal(sigma_means / 10, 0.25)
  expect_equal(res$eta_squared, 0.25^2 / (1 + 0.25^2))
})

test_that("f_to_means accepts eta_squared and converts correctly", {
  eta <- 0.06
  suppressMessages(res <- f_to_means(eta_squared = eta, k = 4, sd = 5))
  expect_equal(res$f, sqrt(eta / (1 - eta)))
  sigma_means <- sqrt(mean((res$means - res$grand_mean)^2))
  expect_equal(sigma_means / 5, res$f)
})

test_that("f_to_means requires exactly one of f and eta_squared", {
  expect_error(f_to_means(k = 3, sd = 10), "exactly one")
  expect_error(f_to_means(f = 0.25, eta_squared = 0.06, k = 3, sd = 10),
               "exactly one")
})

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
