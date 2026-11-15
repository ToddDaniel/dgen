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
