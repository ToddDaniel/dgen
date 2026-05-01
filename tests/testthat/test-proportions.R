## ── oneprop_gen ───────────────────────────────────────────────────────────────

test_that("oneprop_gen returns correct structure", {
  result <- oneprop_gen(n = 100, p = 0.4, test_value = 0.5, seed = 1)
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 100)
})

test_that("oneprop_gen achieved proportion matches target (exact successes)", {
  result <- oneprop_gen(n = 200, p = 0.6, test_value = 0.5, seed = 2)
  expect_equal(result$successes, round(200 * 0.6))
})

test_that("oneprop_gen input validation", {
  expect_error(oneprop_gen(n = 0,  p = 0.5, test_value = 0.5))
  expect_error(oneprop_gen(n = 50, p = 1.5, test_value = 0.5))
  expect_error(oneprop_gen(n = 50, p = 0.5, test_value = -0.1))
})

test_that("oneprop_gen seed reproducibility", {
  r1 <- oneprop_gen(n = 80, p = 0.3, test_value = 0.5, seed = 11)
  r2 <- oneprop_gen(n = 80, p = 0.3, test_value = 0.5, seed = 11)
  expect_identical(r1$data, r2$data)
})

## ── twoprop_gen ───────────────────────────────────────────────────────────────

test_that("twoprop_gen returns correct structure", {
  result <- twoprop_gen(
    n1 = 60, p1 = 0.5,
    n2 = 60, p2 = 0.7,
    seed = 1
  )
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data))
  expect_equal(nrow(result$data), 120)
})

test_that("twoprop_gen exact success counts", {
  result <- twoprop_gen(n1 = 100, p1 = 0.3, n2 = 100, p2 = 0.7, seed = 3)
  expect_equal(result$group1_stats$successes, round(100 * 0.3))
  expect_equal(result$group2_stats$successes, round(100 * 0.7))
})

test_that("twoprop_gen seed reproducibility", {
  args <- list(n1 = 50, p1 = 0.4, n2 = 50, p2 = 0.6, seed = 99)
  r1 <- do.call(twoprop_gen, args)
  r2 <- do.call(twoprop_gen, args)
  expect_identical(r1$data, r2$data)
})

test_that("twoprop_gen input validation", {
  expect_error(twoprop_gen(n1 = 0,  p1 = 0.5, n2 = 50, p2 = 0.5))
  expect_error(twoprop_gen(n1 = 50, p1 = 1.2, n2 = 50, p2 = 0.5))
})
