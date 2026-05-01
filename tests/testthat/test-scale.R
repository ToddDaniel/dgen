## ── scale_gen ─────────────────────────────────────────────────────────────────

test_that("scale_gen returns correct structure", {
  result <- scale_gen(n = 100, n_items = 6, n_bad_items = 2, seed = 1)
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 100)
})

test_that("scale_gen correct number of item columns", {
  result <- scale_gen(n = 80, n_items = 8, n_bad_items = 2, seed = 2)
  # data should have 8 item columns (plus possibly composite)
  expect_true(ncol(result$data) >= 8)
})

test_that("scale_gen items within scale bounds", {
  result <- scale_gen(n = 100, n_items = 6, n_bad_items = 1,
                       scale_min = 1, scale_max = 7, seed = 3)
  item_cols <- result$data[, grep("Item", names(result$data))]
  expect_true(all(item_cols >= 1))
  expect_true(all(item_cols <= 7))
})

test_that("scale_gen alpha is between 0 and 1", {
  result <- scale_gen(n = 200, n_items = 8, n_bad_items = 2,
                       target_alpha = 0.80, seed = 4)
  expect_true(result$alpha_all >= 0 && result$alpha_all <= 1)
})

test_that("scale_gen seed reproducibility", {
  args <- list(n = 100, n_items = 6, n_bad_items = 2, seed = 66)
  r1 <- do.call(scale_gen, args)
  r2 <- do.call(scale_gen, args)
  expect_identical(r1$data, r2$data)
})

test_that("scale_gen n_bad_items < n_items", {
  expect_error(
    scale_gen(n = 100, n_items = 4, n_bad_items = 4, seed = 1)
  )
})

## ── item_gen ──────────────────────────────────────────────────────────────────

test_that("item_gen returns correct structure", {
  preds <- make_predictors(n = 80, FP = c(m = 5, sd = 1.5), seed = 1)
  result <- item_gen(composite = preds$data$FP, n_items = 5,
                      scale_min = 1, scale_max = 10, seed = 2)
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 80)
  expect_equal(ncol(result$data), 5)
})

test_that("item_gen items within scale bounds", {
  preds <- make_predictors(n = 100, FP = c(m = 5, sd = 1), seed = 3)
  result <- item_gen(composite = preds$data$FP, n_items = 5,
                      scale_min = 1, scale_max = 10, seed = 4)
  expect_true(all(result$data >= 1))
  expect_true(all(result$data <= 10))
})

test_that("item_gen row means close to composite", {
  preds <- make_predictors(n = 100, FP = c(m = 5, sd = 1), seed = 5)
  result <- item_gen(composite = preds$data$FP, n_items = 10,
                      scale_min = 1, scale_max = 10, seed = 6)
  expect_true(result$accuracy$mean_abs_error < 0.3)
})

test_that("item_gen composite range validation", {
  expect_error(
    item_gen(composite = c(0.5, 5, 7), n_items = 5,
             scale_min = 1, scale_max = 10)
  )
  expect_error(
    item_gen(composite = c(5, 7, 11), n_items = 5,
             scale_min = 1, scale_max = 10)
  )
})

test_that("item_gen seed reproducibility", {
  preds <- make_predictors(n = 60, FP = c(m = 5, sd = 1), seed = 1)
  args <- list(composite = preds$data$FP, n_items = 5,
               scale_min = 1, scale_max = 10, seed = 88)
  r1 <- do.call(item_gen, args)
  r2 <- do.call(item_gen, args)
  expect_identical(r1$data, r2$data)
})
