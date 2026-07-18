# Tests for the effect-size-first helper functions

test_that("d_to_means recovers the target d exactly", {
  suppressMessages(res <- d_to_means(d = 0.5, sd1 = 15, mean1 = 100))
  expect_equal(res$mean2, 100 + 0.5 * 15)
  expect_equal(res$pooled_sd, 15)
  expect_equal((res$mean2 - res$mean1) / res$pooled_sd, 0.5)
})

test_that("d_to_means handles unequal SDs via the pooled SD", {
  suppressMessages(res <- d_to_means(d = -0.8, sd1 = 10, sd2 = 20, mean1 = 50))
  pooled <- sqrt((10^2 + 20^2) / 2)
  expect_equal(res$pooled_sd, pooled)
  expect_equal(res$mean2, 50 - 0.8 * pooled)
})

test_that("d_to_means feeds t_indep_gen to an accurate achieved d", {
  suppressMessages(p <- d_to_means(d = 0.5, sd1 = 10, mean1 = 50))
  suppressMessages(gen <- t_indep_gen(
    n1 = 40, mean1 = p$mean1, sd1 = p$sd1, label1 = "A",
    n2 = 40, mean2 = p$mean2, sd2 = p$sd2, label2 = "B",
    seed = 42, run_analysis = FALSE
  ))
  expect_lt(abs(gen$cohens_d - 0.5), 0.05)
})

test_that("d_to_means validates inputs", {
  expect_error(d_to_means(d = "big", sd1 = 10), "'d'")
  expect_error(d_to_means(d = 0.5, sd1 = -1), "'sd1'")
  expect_error(d_to_means(d = 0.5, sd1 = 10, sd2 = 0), "'sd2'")
})

test_that("d_to_means_paired matches t_paired_gen's d definition", {
  suppressMessages(res <- d_to_means_paired(d = 0.8, sd1 = 10, r = 0.70,
                                            mean1 = 50))
  sd_diff <- sqrt(10^2 + 10^2 - 2 * 0.70 * 10 * 10)
  expect_equal(res$sd_diff, sd_diff)
  expect_equal(res$mean2, 50 + 0.8 * sd_diff)
  expect_equal((res$mean2 - res$mean1) / res$sd_diff, 0.8)
})

test_that("d_to_means_paired validates r and degenerate inputs", {
  expect_error(d_to_means_paired(d = 0.5, sd1 = 10, r = 1.2), "'r'")
  # r = 1 exactly is rejected by validation before sd_diff can be zero
  expect_error(d_to_means_paired(d = 0.5, sd1 = 10, r = 1), "'r'")
})

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
