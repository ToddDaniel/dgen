## ── t_onesample_gen ──────────────────────────────────────────────────────────

test_that("t_onesample_gen returns correct structure", {
  result <- t_onesample_gen(n = 30, m = 10, sd = 2, test_value = 8, seed = 1)
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 30)
})

test_that("t_onesample_gen achieves exact mean and SD", {
  result <- t_onesample_gen(n = 100, m = 7.5, sd = 1.5,
                             test_value = 7, decimals = 1, seed = 3)
  x <- result$data[[1]]
  expect_equal(round(mean(x), 1), 7.5)
  expect_equal(round(sd(x), 1), 1.5)
})

test_that("t_onesample_gen seed reproducibility", {
  r1 <- t_onesample_gen(n = 50, m = 5, sd = 1, test_value = 4, seed = 10)
  r2 <- t_onesample_gen(n = 50, m = 5, sd = 1, test_value = 4, seed = 10)
  expect_identical(r1$data, r2$data)
})

test_that("t_onesample_gen input validation", {
  expect_error(t_onesample_gen(n = 1, m = 5, sd = 1, test_value = 0))
  expect_error(t_onesample_gen(n = 30, m = 5, sd = -1, test_value = 0))
})

## ── t_indep_gen ───────────────────────────────────────────────────────────────

test_that("t_indep_gen returns correct structure", {
  result <- t_indep_gen(
    n1 = 30, mean1 = 10, sd1 = 2,
    n2 = 30, mean2 = 12, sd2 = 2,
    seed = 1
  )
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 60)
})

test_that("t_indep_gen achieves target means", {
  result <- t_indep_gen(
    n1 = 100, mean1 = 5.0, sd1 = 1.0,
    n2 = 100, mean2 = 7.0, sd2 = 1.0,
    decimals = 1, seed = 2
  )
  g1 <- result$data$Score[result$data$Group == "Group1"]
  g2 <- result$data$Score[result$data$Group == "Group2"]
  expect_equal(round(mean(g1), 1), 5.0)
  expect_equal(round(mean(g2), 1), 7.0)
})

test_that("t_indep_gen unequal n works", {
  result <- t_indep_gen(
    n1 = 40, mean1 = 5, sd1 = 1,
    n2 = 60, mean2 = 6, sd2 = 1,
    seed = 5
  )
  expect_equal(nrow(result$data), 100)
})

test_that("t_indep_gen seed reproducibility", {
  args <- list(n1 = 30, mean1 = 5, sd1 = 1,
               n2 = 30, mean2 = 6, sd2 = 1, seed = 9)
  r1 <- do.call(t_indep_gen, args)
  r2 <- do.call(t_indep_gen, args)
  expect_identical(r1$data, r2$data)
})

## ── t_paired_gen ─────────────────────────────────────────────────────────────

test_that("t_paired_gen returns correct structure", {
  result <- t_paired_gen(
    n = 40,
    measure1_mean = 10, measure1_sd = 2,
    measure2_mean = 12, measure2_sd = 2,
    correlation = 0.6, seed = 1
  )
  expect_true(is.list(result))
  expect_true(!is.null(result$data))
})

test_that("t_paired_gen n rows correct (wide format)", {
  result <- t_paired_gen(
    n = 50,
    measure1_mean = 5, measure1_sd = 1,
    measure2_mean = 6, measure2_sd = 1,
    correlation = 0.5, output_format = "wide", seed = 4
  )
  expect_equal(nrow(result$data), 50)
})

test_that("t_paired_gen correlation validation", {
  expect_error(
    t_paired_gen(n = 30,
                 measure1_mean = 5, measure1_sd = 1,
                 measure2_mean = 6, measure2_sd = 1,
                 correlation = 1.5)
  )
})

test_that("t_paired_gen seed reproducibility", {
  args <- list(n = 40, measure1_mean = 5, measure1_sd = 1,
               measure2_mean = 7, measure2_sd = 1,
               correlation = 0.5, seed = 77)
  r1 <- do.call(t_paired_gen, args)
  r2 <- do.call(t_paired_gen, args)
  expect_identical(r1$data, r2$data)
})
