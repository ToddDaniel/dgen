# Tests for mcnemar_gen()

test_that("mcnemar_gen hits exact marginal proportions", {
  suppressMessages(res <- mcnemar_gen(n = 100, p1 = 0.55, p2 = 0.75,
                                      seed = 42))
  expect_equal(res$measure1_stats$proportion, 0.55)
  expect_equal(res$measure2_stats$proportion, 0.75)
  expect_equal(res$difference, 0.20)
  # Recompute the proportions from the raw data
  p1_data <- mean(res$data$Time1 == "Success")
  p2_data <- mean(res$data$Time2 == "Success")
  expect_equal(p1_data, 0.55)
  expect_equal(p2_data, 0.75)
})

test_that("mcnemar_gen cell counts are internally consistent", {
  suppressMessages(res <- mcnemar_gen(n = 80, p1 = 0.40, p2 = 0.60,
                                      seed = 1))
  cc <- res$cell_counts
  expect_equal(cc$both_success + cc$success_to_failure +
                 cc$failure_to_success + cc$both_failure, 80)
  expect_equal(cc$both_success + cc$success_to_failure, round(80 * 0.40))
  expect_equal(cc$both_success + cc$failure_to_success, round(80 * 0.60))
  expect_true(all(unlist(cc) >= 0))
})

test_that("mcnemar_gen honors a valid n_discordant", {
  # diff = 75 - 55 = 20, so n_discordant must be even and >= 20
  suppressMessages(res <- mcnemar_gen(n = 100, p1 = 0.55, p2 = 0.75,
                                      n_discordant = 30, seed = 42))
  expect_equal(res$n_discordant, 30)
  cc <- res$cell_counts
  expect_equal(cc$success_to_failure + cc$failure_to_success, 30)
  expect_equal(cc$failure_to_success - cc$success_to_failure, 20)
})

test_that("mcnemar_gen rejects invalid n_discordant values", {
  # Wrong parity (diff = 20, so 25 is invalid)
  expect_error(
    suppressMessages(mcnemar_gen(n = 100, p1 = 0.55, p2 = 0.75,
                                 n_discordant = 25)),
    "parity")
  # Below the minimum |diff|
  expect_error(
    suppressMessages(mcnemar_gen(n = 100, p1 = 0.55, p2 = 0.75,
                                 n_discordant = 10)),
    "between")
})

test_that("mcnemar_gen test statistic matches the textbook formula", {
  suppressMessages(res <- mcnemar_gen(n = 100, p1 = 0.55, p2 = 0.75,
                                      n_discordant = 30, seed = 42))
  b <- res$cell_counts$success_to_failure
  c_ <- res$cell_counts$failure_to_success
  manual_chi2 <- (abs(b - c_) - 1)^2 / (b + c_)
  expect_equal(as.numeric(res$mcnemar_test_results$statistic), manual_chi2)
  # Exact test is a binomial test on the discordant pairs
  expect_equal(as.numeric(res$mcnemar_exact_results$parameter), b + c_)
})

test_that("mcnemar_gen effect sizes are computed from discordant cells", {
  suppressMessages(res <- mcnemar_gen(n = 100, p1 = 0.55, p2 = 0.75,
                                      n_discordant = 30, seed = 42))
  b <- res$cell_counts$success_to_failure   # 5
  c_ <- res$cell_counts$failure_to_success  # 25
  expect_equal(res$odds_ratio, c_ / b)
  expect_equal(res$cohens_g, c_ / (b + c_) - 0.5)
})

test_that("mcnemar_gen is reproducible with a seed", {
  suppressMessages(r1 <- mcnemar_gen(n = 60, p1 = 0.50, p2 = 0.65,
                                     seed = 99))
  suppressMessages(r2 <- mcnemar_gen(n = 60, p1 = 0.50, p2 = 0.65,
                                     seed = 99))
  expect_identical(r1$data, r2$data)
})

test_that("mcnemar_gen respects custom names and labels", {
  suppressMessages(res <- mcnemar_gen(
    n = 50, p1 = 0.40, p2 = 0.60,
    measure1_name = "Before", measure2_name = "After",
    category_labels = c("Fail", "Pass"), id_name = "Student",
    seed = 7))
  expect_named(res$data, c("Student", "Before", "After"))
  expect_equal(levels(res$data$Before), c("Fail", "Pass"))
  expect_equal(dim(res$contingency_table), c(2, 2))
})

test_that("mcnemar_gen run_analysis = FALSE skips the tests", {
  suppressMessages(res <- mcnemar_gen(n = 50, p1 = 0.40, p2 = 0.60,
                                      run_analysis = FALSE, seed = 7))
  expect_null(res$mcnemar_test_results)
  expect_null(res$mcnemar_exact_results)
})

test_that("mcnemar_gen warns and skips tests with zero discordant pairs", {
  expect_warning(
    suppressMessages(res <- mcnemar_gen(n = 50, p1 = 0.40, p2 = 0.40,
                                        n_discordant = 0, seed = 7)),
    "discordant")
  expect_null(res$mcnemar_test_results)
})

test_that("mcnemar_gen validates basic inputs", {
  expect_error(suppressMessages(mcnemar_gen(n = 0, p1 = 0.5, p2 = 0.6)),
               "'n'")
  expect_error(suppressMessages(mcnemar_gen(n = 50, p1 = 0, p2 = 0.6)),
               "'p1'")
  expect_error(suppressMessages(mcnemar_gen(n = 50, p1 = 0.5, p2 = 1)),
               "'p2'")
  expect_error(suppressMessages(
    mcnemar_gen(n = 50, p1 = 0.5, p2 = 0.6, measure1_name = "X",
                measure2_name = "X")),
    "differ")
})
