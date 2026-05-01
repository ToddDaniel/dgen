## ── anova_gen ─────────────────────────────────────────────────────────────────

test_that("anova_gen returns correct structure", {
  result <- anova_gen(k = 3, n = 20,
                       means = c(10, 12, 14), sds = c(2, 2, 2), seed = 1)
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
})

test_that("anova_gen correct number of rows", {
  result <- anova_gen(k = 3, n = 25,
                       means = c(5, 7, 9), sds = c(1, 1, 1), seed = 2)
  expect_equal(nrow(result$data), 75)
})

test_that("anova_gen unequal group sizes", {
  result <- anova_gen(k = 3, n = c(20, 30, 40),
                       means = c(5, 6, 7), sds = c(1, 1, 1), seed = 3)
  expect_equal(nrow(result$data), 90)
})

test_that("anova_gen requires k >= 3", {
  expect_error(anova_gen(k = 2, n = 20, means = c(5, 7), sds = c(1, 1)))
})

test_that("anova_gen seed reproducibility", {
  args <- list(k = 3, n = 20, means = c(5, 7, 9), sds = c(1, 1, 1), seed = 55)
  r1 <- do.call(anova_gen, args)
  r2 <- do.call(anova_gen, args)
  expect_identical(r1$data, r2$data)
})

test_that("anova_gen achieves target group means (rounded)", {
  result <- anova_gen(k = 3, n = 100,
                       means = c(5.0, 7.0, 9.0), sds = c(1.0, 1.0, 1.0),
                       decimals = 1, seed = 8)
  expect_equal(round(mean(result$data$Score[result$data$Group == "Group1"]), 1), 5.0)
  expect_equal(round(mean(result$data$Score[result$data$Group == "Group2"]), 1), 7.0)
  expect_equal(round(mean(result$data$Score[result$data$Group == "Group3"]), 1), 9.0)
})

## ── anova_twoway_gen ──────────────────────────────────────────────────────────

test_that("anova_twoway_gen returns correct structure", {
  result <- anova_twoway_gen(
    factor_a_levels = 2, factor_a_labels = c("A1", "A2"),
    factor_b_levels = 2, factor_b_labels = c("B1", "B2"),
    n_per_cell = 15,
    cell_means = matrix(c(5, 7, 6, 9), nrow = 2),
    cell_sds   = matrix(rep(1, 4), nrow = 2),
    seed = 1
  )
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 60)
})

test_that("anova_twoway_gen cell count correct for 3x2", {
  result <- anova_twoway_gen(
    factor_a_levels = 3, factor_a_labels = c("A1", "A2", "A3"),
    factor_b_levels = 2, factor_b_labels = c("B1", "B2"),
    n_per_cell = 10,
    cell_means = matrix(c(5, 6, 7, 6, 7, 8), nrow = 3),
    cell_sds   = matrix(rep(1, 6), nrow = 3),
    seed = 2
  )
  expect_equal(nrow(result$data), 60)
})

test_that("anova_twoway_gen seed reproducibility", {
  args <- list(
    factor_a_levels = 2, factor_a_labels = c("A1", "A2"),
    factor_b_levels = 2, factor_b_labels = c("B1", "B2"),
    n_per_cell = 10,
    cell_means = matrix(c(5, 7, 6, 9), nrow = 2),
    cell_sds   = matrix(rep(1, 4), nrow = 2),
    seed = 11
  )
  r1 <- do.call(anova_twoway_gen, args)
  r2 <- do.call(anova_twoway_gen, args)
  expect_identical(r1$data, r2$data)
})

## ── anova_blocked_gen ─────────────────────────────────────────────────────────

test_that("anova_blocked_gen returns correct structure", {
  result <- anova_blocked_gen(
    n_per_block      = 2,
    n_blocks         = 10,
    block_labels     = paste0("Block", 1:10),
    n_treatments     = 3,
    treatment_labels = c("T1", "T2", "T3"),
    cell_means       = matrix(c(5, 7, 9), nrow = 10, ncol = 3, byrow = FALSE),
    within_cell_sd   = 1,
    seed = 1
  )
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 60)
})

test_that("anova_blocked_gen seed reproducibility", {
  args <- list(
    n_per_block      = 2,
    n_blocks         = 8,
    block_labels     = paste0("B", 1:8),
    n_treatments     = 3,
    treatment_labels = c("T1", "T2", "T3"),
    cell_means       = matrix(c(5, 7, 9), nrow = 8, ncol = 3, byrow = FALSE),
    within_cell_sd   = 1,
    seed = 33
  )
  r1 <- do.call(anova_blocked_gen, args)
  r2 <- do.call(anova_blocked_gen, args)
  expect_identical(r1$data, r2$data)
})
