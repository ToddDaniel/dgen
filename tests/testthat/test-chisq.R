## ── chisq_gof_gen ─────────────────────────────────────────────────────────────

test_that("chisq_gof_gen returns correct structure", {
  result <- chisq_gof_gen(n = 100, categories = c("A", "B", "C"),
                           expected = "equal", seed = 1)
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 100)
})

test_that("chisq_gof_gen returns n observations total", {
  result <- chisq_gof_gen(n = 200, categories = c("X", "Y"),
                           expected = "equal", seed = 2)
  expect_equal(nrow(result$data), 200)
})

test_that("chisq_gof_gen proportions mode", {
  result <- chisq_gof_gen(
    n = 150,
    categories = c("A", "B", "C"),
    expected = "proportions",
    expected_values = c(0.5, 0.3, 0.2),
    seed = 3
  )
  expect_equal(nrow(result$data), 150)
})

test_that("chisq_gof_gen seed reproducibility", {
  r1 <- chisq_gof_gen(n = 80, categories = c("A", "B", "C"),
                       expected = "equal", seed = 44)
  r2 <- chisq_gof_gen(n = 80, categories = c("A", "B", "C"),
                       expected = "equal", seed = 44)
  expect_identical(r1$data, r2$data)
})

## ── chisq_indep_gen ───────────────────────────────────────────────────────────

test_that("chisq_indep_gen returns correct structure", {
  result <- chisq_indep_gen(
    n = 100,
    row_levels = c("R1", "R2"),
    col_levels = c("C1", "C2"),
    association = "none",
    seed = 1
  )
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 100)
  expect_equal(ncol(result$data), 2)
})

test_that("chisq_indep_gen effect_size association", {
  result <- chisq_indep_gen(
    n = 200,
    row_levels = c("R1", "R2"),
    col_levels = c("C1", "C2"),
    association = "effect_size",
    cramers_v = 0.3,
    seed = 5
  )
  expect_equal(nrow(result$data), 200)
})

test_that("chisq_indep_gen seed reproducibility", {
  args <- list(n = 100, row_levels = c("R1", "R2"), col_levels = c("C1", "C2"),
               association = "none", seed = 66)
  r1 <- do.call(chisq_indep_gen, args)
  r2 <- do.call(chisq_indep_gen, args)
  expect_identical(r1$data, r2$data)
})

## ── chisq_table_gen ───────────────────────────────────────────────────────────

test_that("chisq_table_gen returns correct structure", {
  result <- chisq_table_gen(
    nrows = 2, ncols = 2,
    row_var = "Gender", col_var = "Outcome",
    row_levels = c("M", "F"),
    col_levels = c("Yes", "No"),
    frequencies = matrix(c(30, 20, 15, 35), nrow = 2),
    seed = 1
  )
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 100)
})

test_that("chisq_table_gen total observations equals sum of frequencies", {
  freq <- matrix(c(10, 20, 30, 40), nrow = 2)
  result <- chisq_table_gen(
    nrows = 2, ncols = 2,
    row_var = "A", col_var = "B",
    row_levels = c("R1", "R2"),
    col_levels = c("C1", "C2"),
    frequencies = freq,
    seed = 2
  )
  expect_equal(nrow(result$data), sum(freq))
})

test_that("chisq_table_gen seed reproducibility", {
  args <- list(
    nrows = 2, ncols = 2,
    row_var = "A", col_var = "B",
    row_levels = c("R1", "R2"),
    col_levels = c("C1", "C2"),
    frequencies = matrix(c(20, 30, 25, 25), nrow = 2),
    seed = 77
  )
  r1 <- do.call(chisq_table_gen, args)
  r2 <- do.call(chisq_table_gen, args)
  expect_identical(r1$data, r2$data)
})
