# Tests for anscombe_gen()

test_that("every panel hits identical exact statistics", {
  suppressMessages(q <- anscombe_gen(n = 50, seed = 42))
  st <- q$stats_table
  expect_equal(nrow(st), 4)
  expect_true(all(st$Mean_X == 9))
  expect_true(all(st$SD_X == 3.3))
  expect_true(all(st$Mean_Y == 7.5))
  expect_true(all(st$SD_Y == 2))
  expect_true(all(st$r == 0.816))
})

test_that("every panel has the same regression line", {
  suppressMessages(q <- anscombe_gen(n = 60, seed = 1))
  slopes     <- q$stats_table$Slope
  intercepts <- q$stats_table$Intercept
  expect_lt(max(slopes) - min(slopes), 0.01)
  expect_lt(max(intercepts) - min(intercepts), 0.03)
  # lm fits agree with the stats table
  co <- stats::coef(q$lm_fits$linear)
  expect_equal(unname(co[2]), q$stats_table$Slope[1], tolerance = 0.01)
})

test_that("long data is structured for faceting", {
  suppressMessages(q <- anscombe_gen(n = 40, seed = 3))
  expect_equal(nrow(q$data), 40 * 4)
  expect_s3_class(q$data$Dataset, "factor")
  expect_equal(levels(q$data$Dataset),
               c("linear", "curved", "outlier", "clusters"))
  expect_named(q$data, c("Dataset", "X", "Y"))
  expect_named(q$datasets, c("linear", "curved", "outlier", "clusters"))
})

test_that("the curved panel is genuinely curvilinear", {
  suppressMessages(q <- anscombe_gen(n = 80, seed = 42))
  d <- q$datasets$curved
  fit <- stats::lm(Y ~ X + I(X^2), data = d)
  p_quad <- summary(fit)$coefficients["I(X^2)", 4]
  expect_lt(p_quad, 0.01)
})

test_that("the outlier panel contains a high-leverage point", {
  suppressMessages(q <- anscombe_gen(n = 50, seed = 42))
  zx <- as.vector(scale(q$datasets$outlier$X))
  expect_gt(max(abs(zx)), 2.5)
})

test_that("the clusters panel has a hollow middle in x", {
  suppressMessages(q <- anscombe_gen(n = 100, seed = 42))
  x <- q$datasets$clusters$X
  prop_middle <- mean(abs(x - mean(x)) < 0.4 * stats::sd(x))
  expect_lt(prop_middle, 0.2)
})

test_that("the fan panel is heteroscedastic", {
  suppressMessages(q <- anscombe_gen(n = 100, shapes = c("linear", "fan"),
                                     seed = 42))
  d   <- q$datasets$fan
  res <- abs(stats::residuals(stats::lm(Y ~ X, data = d)))
  expect_gt(stats::cor(d$X, res, method = "spearman"), 0.2)
})

test_that("custom targets, names, and shape subsets work", {
  suppressMessages(q <- anscombe_gen(n = 30, x_mean = 100, x_sd = 15,
                                     y_mean = 3, y_sd = 0.8, r = 0.6,
                                     shapes = c("linear", "fan"),
                                     x_name = "IQ", y_name = "GPA",
                                     seed = 7))
  expect_equal(nrow(q$stats_table), 2)
  expect_true(all(q$stats_table$Mean_X == 100))
  expect_true(all(q$stats_table$r == 0.6))
  expect_named(q$datasets$linear, c("IQ", "GPA"))
})

test_that("negative correlations work", {
  suppressMessages(q <- anscombe_gen(n = 40, r = -0.5,
                                     shapes = c("linear", "curved"),
                                     seed = 5))
  expect_true(all(q$stats_table$r == -0.5))
  expect_true(all(q$stats_table$Slope < 0))
})

test_that("anscombe_gen validates inputs", {
  expect_error(suppressMessages(anscombe_gen(n = 5)), "'n'")
  expect_error(suppressMessages(anscombe_gen(r = 1)), "'r'")
  expect_error(suppressMessages(anscombe_gen(shapes = "spiral")), "shapes")
  expect_error(suppressMessages(anscombe_gen(shapes = c("linear", "linear"))),
               "duplicates")
  expect_error(suppressMessages(anscombe_gen(x_name = "V", y_name = "V")),
               "differ")
})

test_that("anscombe_gen is reproducible and run_analysis is optional", {
  suppressMessages(a <- anscombe_gen(n = 30, seed = 11))
  suppressMessages(b <- anscombe_gen(n = 30, seed = 11))
  expect_identical(a$data, b$data)
  suppressMessages(c_ <- anscombe_gen(n = 30, seed = 11,
                                      run_analysis = FALSE))
  expect_null(c_$lm_fits)
})

test_that("export_csv writes the long data", {
  dest <- file.path(tempdir(), "anscombe_out.csv")
  if (file.exists(dest)) file.remove(dest)
  suppressMessages(anscombe_gen(n = 20, shapes = c("linear", "curved"),
                                seed = 2, export_csv = dest))
  expect_true(file.exists(dest))
  reread <- utils::read.csv(dest)
  expect_equal(nrow(reread), 40)
  expect_equal(names(reread), c("Dataset", "X", "Y"))
  file.remove(dest)
})
