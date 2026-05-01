## ── make_predictors ───────────────────────────────────────────────────────────

test_that("make_predictors returns correct structure", {
  result <- make_predictors(
    n = 100,
    X1 = c(m = 5, sd = 1),
    X2 = c(m = 3, sd = 2),
    seed = 1
  )
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 100)
  expect_equal(ncol(result$data), 2)
})

test_that("make_predictors achieves approximate target means", {
  result <- make_predictors(
    n = 200,
    A = c(m = 10, sd = 2),
    B = c(m = 20, sd = 4),
    seed = 5
  )
  expect_equal(round(mean(result$data$A)), 10)
  expect_equal(round(mean(result$data$B)), 20)
})

test_that("make_predictors binary predictor has correct prevalence", {
  result <- make_predictors(
    n = 500,
    Grp = c(prev = 0.4),
    seed = 7
  )
  p_obs <- mean(result$data$Grp)
  expect_true(abs(p_obs - 0.4) < 0.05)
})

test_that("make_predictors seed reproducibility", {
  args <- list(n = 80, X = c(m = 0, sd = 1), Y = c(m = 5, sd = 2), seed = 20)
  r1 <- do.call(make_predictors, args)
  r2 <- do.call(make_predictors, args)
  expect_identical(r1$data, r2$data)
})

## ── bin_gen ───────────────────────────────────────────────────────────────────

test_that("bin_gen returns binary values only", {
  cont <- rnorm(100)
  result <- bin_gen(continuous_var = cont, prevalence = 0.5,
                    target_cor = 0.3, seed = 1)
  expect_true(all(result$data$Binary_Var %in% c(0, 1)))
})

test_that("bin_gen returns correct structure", {
  cont <- rnorm(100, mean = 5, sd = 1)
  result <- bin_gen(continuous_var = cont, prevalence = 0.4,
                    target_cor = 0.3, seed = 2)
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 100)
})

test_that("bin_gen prevalence is approximately correct", {
  set.seed(10)
  cont <- rnorm(500)
  result <- bin_gen(continuous_var = cont, prevalence = 0.6,
                    target_cor = 0.2, seed = 3)
  p_obs <- mean(result$data$Binary_Var)
  expect_true(abs(p_obs - 0.6) < 0.05)
})

test_that("bin_gen seed reproducibility", {
  set.seed(1); cont <- rnorm(100)
  r1 <- bin_gen(continuous_var = cont, prevalence = 0.5, target_cor = 0.3, seed = 55)
  r2 <- bin_gen(continuous_var = cont, prevalence = 0.5, target_cor = 0.3, seed = 55)
  expect_identical(r1$data, r2$data)
})

## ── reg_gen ───────────────────────────────────────────────────────────────────

test_that("reg_gen returns correct structure with continuous outcome", {
  preds <- make_predictors(n = 100, X1 = c(m = 5, sd = 1),
                            X2 = c(m = 3, sd = 2), seed = 1)
  result <- reg_gen(
    predictors = preds$data,
    betas = c(intercept = 0, X1 = 0.4, X2 = 0.3),
    outcome_type = "continuous",
    r_squared = 0.40,
    seed = 2
  )
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 100)
  expect_true("Y" %in% names(result$data))
})

test_that("reg_gen returns binary outcome with values 0/1", {
  preds <- make_predictors(n = 150, X1 = c(m = 0, sd = 1), seed = 3)
  result <- reg_gen(
    predictors = preds$data,
    betas = c(intercept = 0, X1 = 0.5),
    outcome_type = "binary",
    prevalence = 0.5,
    seed = 4
  )
  expect_true(all(result$data$Y %in% c(0, 1)))
})

test_that("reg_gen seed reproducibility", {
  preds <- make_predictors(n = 80, X1 = c(m = 5, sd = 1), seed = 1)
  args <- list(predictors = preds$data, betas = c(intercept = 0, X1 = 0.5),
               outcome_type = "continuous", r_squared = 0.3, seed = 10)
  r1 <- do.call(reg_gen, args)
  r2 <- do.call(reg_gen, args)
  expect_identical(r1$data, r2$data)
})
