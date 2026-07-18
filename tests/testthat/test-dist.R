# Tests for dist_gen()

test_that("dist_gen hits exact mean and SD for every named shape", {
  for (sh in c("normal", "right_skew", "left_skew", "heavy_tails",
               "light_tails", "bimodal")) {
    suppressMessages(r <- dist_gen(n = 100, m = 50, sd = 10, shape = sh,
                                   seed = 42))
    expect_equal(round(mean(r$data$Variable_X), 3), 50,
                 info = paste("shape:", sh))
    expect_equal(round(stats::sd(r$data$Variable_X), 3), 10,
                 info = paste("shape:", sh))
  }
})

test_that("skew directions come out as requested", {
  suppressMessages(right <- dist_gen(n = 300, m = 50, sd = 10,
                                     shape = "right_skew",
                                     severity = "moderate", seed = 42))
  suppressMessages(left <- dist_gen(n = 300, m = 50, sd = 10,
                                    shape = "left_skew",
                                    severity = "moderate", seed = 42))
  expect_gt(right$achieved$Achieved_Skew, 0.5)
  expect_lt(left$achieved$Achieved_Skew, -0.5)
})

test_that("severity increases the effect for skew", {
  suppressMessages(mild <- dist_gen(n = 500, m = 0, sd = 1,
                                    shape = "right_skew",
                                    severity = "mild", seed = 1))
  suppressMessages(severe <- dist_gen(n = 500, m = 0, sd = 1,
                                      shape = "right_skew",
                                      severity = "severe", seed = 1))
  expect_gt(severe$achieved$Achieved_Skew, mild$achieved$Achieved_Skew)
  expect_equal(mild$targets$Target_Skew, 0.75)
  expect_equal(severe$targets$Target_Skew, 2.5)
})

test_that("kurtosis shapes go in the right directions", {
  suppressMessages(heavy <- dist_gen(n = 500, m = 0, sd = 1,
                                     shape = "heavy_tails",
                                     severity = "severe", seed = 42))
  suppressMessages(light <- dist_gen(n = 500, m = 0, sd = 1,
                                     shape = "light_tails",
                                     severity = "severe", seed = 42))
  expect_gt(heavy$achieved$Achieved_Ex_Kurt, 0.5)
  expect_lt(light$achieved$Achieved_Ex_Kurt, -1)
})

test_that("bimodal data are platykurtic with a hollow middle", {
  suppressMessages(b <- dist_gen(n = 500, m = 100, sd = 15,
                                 shape = "bimodal", severity = "severe",
                                 seed = 42))
  expect_lt(b$achieved$Achieved_Ex_Kurt, -1)
  # far fewer points near the mean than a normal would have (~38%)
  x <- b$data$Variable_X
  prop_middle <- mean(abs(x - mean(x)) < 0.5 * stats::sd(x))
  expect_lt(prop_middle, 0.25)
})

test_that("numeric Fleishman targets are approximately achieved", {
  suppressMessages(r <- dist_gen(n = 1000, m = 4.2, sd = 1.1,
                                 skew = 1.8, kurtosis = 5.5, seed = 42))
  expect_equal(round(mean(r$data$Variable_X), 3), 4.2)
  expect_equal(round(stats::sd(r$data$Variable_X), 3), 1.1)
  expect_lt(abs(r$achieved$Achieved_Skew - 1.8), 0.5)
  expect_equal(r$targets$Shape, "numeric")
  expect_equal(r$targets$Target_Skew, 1.8)
})

test_that("skew-only numeric requests default kurtosis to 1.7 * skew^2", {
  suppressMessages(r <- dist_gen(n = 200, m = 0, sd = 1, skew = 1.5,
                                 seed = 1))
  expect_equal(r$targets$Target_Ex_Kurt, 1.7 * 1.5^2)
})

test_that("kurtosis-only numeric requests default skew to 0", {
  suppressMessages(r <- dist_gen(n = 200, m = 0, sd = 1, kurtosis = 3,
                                 seed = 1))
  expect_equal(r$targets$Target_Skew, 0)
})

test_that("mixing a named shape with numeric targets errors", {
  expect_error(
    suppressMessages(dist_gen(n = 100, m = 0, sd = 1,
                              shape = "right_skew", skew = 2)),
    "not both")
})

test_that("infeasible numeric combinations error informatively", {
  expect_error(
    suppressMessages(dist_gen(n = 100, m = 0, sd = 1,
                              skew = 3, kurtosis = 0)),
    "kurtosis")
  expect_error(
    suppressMessages(dist_gen(n = 100, m = 0, sd = 1,
                              skew = 0, kurtosis = -1.9)),
    "Fleishman|light_tails")
})

test_that("normality diagnostics flag severe non-normality", {
  suppressMessages(r <- dist_gen(n = 200, m = 50, sd = 10,
                                 shape = "right_skew", severity = "severe",
                                 seed = 42))
  expect_s3_class(r$normality$shapiro_test, "htest")
  expect_lt(r$normality$shapiro_test$p.value, 0.05)
  expect_equal(r$normality$skew_se, sqrt(6 / 200))
  suppressMessages(r2 <- dist_gen(n = 200, m = 50, sd = 10,
                                  run_analysis = FALSE, seed = 42))
  expect_null(r2$normality)
})

test_that("dist_gen is reproducible and validates inputs", {
  suppressMessages(a <- dist_gen(n = 100, m = 50, sd = 10,
                                 shape = "bimodal", seed = 9))
  suppressMessages(b <- dist_gen(n = 100, m = 50, sd = 10,
                                 shape = "bimodal", seed = 9))
  expect_identical(a$data, b$data)
  expect_error(suppressMessages(dist_gen(n = 2, m = 0, sd = 1)), "'n'")
  expect_error(suppressMessages(dist_gen(n = 100, m = 0, sd = 0)), "'sd'")
  expect_error(suppressMessages(dist_gen(n = 100, m = 0, sd = 1,
                                         shape = "zigzag")), "shape")
  expect_error(suppressMessages(dist_gen(n = 100, m = 0, sd = 1,
                                         severity = "extreme")), "severity")
})

test_that("dist_gen exports CSV and works inside batch_gen", {
  dest <- file.path(tempdir(), "dist_out.csv")
  if (file.exists(dest)) file.remove(dest)
  suppressMessages(dist_gen(n = 50, m = 10, sd = 2, shape = "right_skew",
                            seed = 1, export_csv = dest))
  expect_true(file.exists(dest))
  file.remove(dest)

  suppressMessages(batch <- batch_gen(dist_gen, n_datasets = 2,
                                      n = 50, m = 10, sd = 2,
                                      shape = "right_skew", seeds = 5))
  expect_equal(nrow(batch$answer_key), 2)
  expect_true("achieved_Achieved_Skew" %in% names(batch$answer_key))
})
