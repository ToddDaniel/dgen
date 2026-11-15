# Tests of the engine's promise, checked on the returned (rounded) data.

# The promise, with the tie rule: each statistic is strictly closer to its
# target than half a step of the reported precision.
expect_exact <- function(data, m, sd, r = NULL, decimals, r_digits = 2) {
  step <- 10^-(decimals + 1)
  expect_true(all(abs(colMeans(data) - m) < step / 2))
  expect_true(all(abs(apply(data, 2, stats::sd) - sd) < step / 2))
  expect_equal(data, round(data, decimals))
  if (!is.null(r)) {
    if (length(r) == 1) r <- matrix(c(1, r, r, 1), 2)
    pairs <- upper.tri(r)
    expect_true(all(abs(stats::cor(data)[pairs] - r[pairs]) <
                      0.5 * 10^-r_digits))
  }
}

# ---- Must-succeed zone -----------------------------------------------------

test_that("one variable is exact: n >= 10, SD >= 2 rounding units", {
  for (n in c(10, 25, 100, 500)) {
    for (decimals in 0:3) {
      for (seed in 1:5) {
        set.seed(seed)
        out <- exact_mv(n, m = 75.4, sd = 8.2, decimals = decimals)
        expect_exact(out$data, 75.4, 8.2, decimals = decimals)
        expect_equal(dim(out$data), c(n, 1))
      }
    }
  }
  # SD of exactly 2 rounding units, and a mean near zero
  for (seed in 1:20) {
    set.seed(seed)
    expect_exact(exact_mv(10, 50.3, 2, decimals = 0)$data, 50.3, 2,
                 decimals = 0)
    expect_exact(exact_mv(30, -0.4, 0.2, decimals = 1)$data, -0.4, 0.2,
                 decimals = 1)
  }
})

test_that("two variables are exact: n >= 10, SD >= 3 rounding units", {
  for (n in c(10, 30, 100)) {
    for (decimals in 0:2) {
      for (r in c(0, 0.3, -0.7, 0.95)) {
        for (seed in 1:3) {
          set.seed(seed)
          out <- exact_mv(n, m = c(50, 55.5), sd = c(10, 12.5), r = r,
                          decimals = decimals)
          expect_exact(out$data, c(50, 55.5), c(10, 12.5), r, decimals)
        }
      }
    }
  }
  for (seed in 1:20) {
    set.seed(seed)
    out <- exact_mv(10, m = c(20, 22), sd = c(3, 3), r = 0.5, decimals = 0)
    expect_exact(out$data, c(20, 22), c(3, 3), 0.5, decimals = 0)
  }
})

test_that("three variables are exact: n >= 30, SD >= 5 rounding units", {
  r <- matrix(c(1, 0.4, 0.3,
                0.4, 1, 0.2,
                0.3, 0.2, 1), 3)
  m  <- c(2.8, 2.6, 3)
  sd <- c(0.9, 0.85, 1)
  for (n in c(30, 100, 500)) {
    for (decimals in 1:3) {
      for (seed in 1:3) {
        set.seed(seed)
        out <- exact_mv(n, m, sd, r, decimals = decimals)
        expect_exact(out$data, m, sd, r, decimals)
      }
    }
  }
  r_half <- matrix(0.5, 3, 3)
  diag(r_half) <- 1
  for (seed in 1:10) {
    set.seed(seed)
    out <- exact_mv(30, m = c(50, 60, 70), sd = c(5, 5, 5), r = r_half,
                    decimals = 0)
    expect_exact(out$data, c(50, 60, 70), c(5, 5, 5), r_half, decimals = 0)
  }
})

# ---- Mean repair and the tie rule -----------------------------------------

test_that("mean repair makes the mean exactly the target", {
  # Without repair, 26% of these datasets had a mean of 75.25 (a tie).
  for (seed in 1:50) {
    set.seed(seed)
    out <- exact_mv(20, m = 75.2, sd = 8.2, decimals = 0)
    expect_equal(sum(out$data), 20 * 75.2)
  }
  set.seed(1)
  paired <- exact_mv(30, m = c(50, 55), sd = c(10, 10), r = 0.7,
                     decimals = 1)$data
  expect_equal(mean(paired[, 2] - paired[, 1]), 5)
})

test_that("repair_total moves the values closest to rounding the other way", {
  exact   <- c(68.48, 81.45, 70.10, 74.30, 79.20)
  rounded <- round(exact)
  expect_equal(repair_total(rounded, exact, sum(rounded) + 2),
               c(69, 82, 70, 74, 79))
  expect_equal(repair_total(rounded, exact, sum(rounded) - 1),
               c(68, 81, 69, 74, 79))
  expect_equal(repair_total(rounded, exact, sum(rounded)), rounded)
})

test_that("a statistic sitting exactly on a rounding boundary fails", {
  # SDs of 0.95 and 1.05 units are ties for a target of 1.0
  expect_equal(stats::sd(c(0, 0.95 * sqrt(2))), 0.95)
  expect_false(hits_targets(matrix(c(0, 0.95 * sqrt(2))), 1, NULL, 2))
  expect_false(hits_targets(matrix(c(0, 1.05 * sqrt(2))), 1, NULL, 2))
  expect_true(hits_targets(matrix(c(0, 0.951 * sqrt(2))), 1, NULL, 2))
  expect_true(hits_targets(matrix(c(0, 1.049 * sqrt(2))), 1, NULL, 2))
  # Four whole numbers: the only means near 75.1 are 75.0 and 75.25 (a tie)
  expect_error(exact_mv(4, 75.1, 8, decimals = 0), "75.0 and 75.5")
})

# ---- Must-fail-fast zone ---------------------------------------------------

test_that("impossible means and SDs stop at once, naming the nearest", {
  expect_error(exact_mv(5, 75.5, 8.2, decimals = 0),
               "With 5 whole-number values, the mean cannot be 75.5. The closest means you can get are 75.4 and 75.6",
               fixed = TRUE)
  expect_error(exact_mv(10, 50.3, 1, decimals = 0),
               "and a mean of 50.3, the SD cannot be 1.0. The closest SDs you can get are 0.9 and 1.1",
               fixed = TRUE)
  expect_error(exact_mv(7, 2.45, 1.2, decimals = 1),
               "With 7 values rounded to 1 decimal place, the mean cannot be 2.45",
               fixed = TRUE)
  started <- Sys.time()
  try(exact_mv(5, 75.5, 8.2, decimals = 0), silent = TRUE)
  expect_lt(as.numeric(Sys.time() - started, units = "secs"), 0.5)
})

test_that("the nearest SDs named in the message really are reachable", {
  # n = 10 whole numbers adding to 503: these have SDs of 0.9 and 1.1
  low  <- c(49, 49, 50, 50, 50, 50, 51, 51, 51, 52)
  high <- c(49, 49, 49, 50, 50, 51, 51, 51, 51, 52)
  expect_equal(c(sum(low), sum(high)), c(503, 503))
  expect_equal(nearest_sds(10, 503, 1), c(stats::sd(low), stats::sd(high)))
  expect_null(nearest_sds(10, 503, 2))
})

test_that("badly formed requests get plain-language messages", {
  expect_error(exact_mv(5, rep(50, 5), rep(10, 5), diag(5)),
               "You asked for 5 variables but only 5 rows. n must be at least 7",
               fixed = TRUE)
  expect_error(exact_mv(2, 50, 10), "n must be at least 3")
  expect_error(exact_mv(30.5, 50, 10), "single whole number")
  expect_error(exact_mv(30, 50, -1), "greater than 0")
  expect_error(exact_mv(30, c(50, 55), 10), "2 mean(s) and 1 SD(s)",
               fixed = TRUE)
  expect_error(exact_mv(30, 50, 10, decimals = 1.5), "decimals must be")
  expect_error(exact_mv(30, 75.25, 10, decimals = 0),
               "means are matched to 1 decimal place, but you asked for 75.25. Use 75.2 or 75.3",
               fixed = TRUE)
  expect_error(exact_mv(30, 50, 10, r = 0.5), "at least two variables")
  expect_error(exact_mv(30, c(50, 55), c(10, 10)), "r is missing")
  expect_error(exact_mv(30, c(50, 55), c(10, 10), r = 1),
               "between -1 and 1")
  expect_error(exact_mv(30, c(50, 55), c(10, 10), r = 0.455),
               "Use 0.45 or 0.46", fixed = TRUE)
  expect_error(exact_mv(30, c(50, 55, 60), c(10, 10, 10), r = 0.5),
               "3 x 3 correlation table", fixed = TRUE)
  impossible <- matrix(c(1, 0.9, 0.9,
                         0.9, 1, -0.9,
                         0.9, -0.9, 1), 3)
  expect_error(exact_mv(30, c(50, 55, 60), c(10, 10, 10), impossible),
               "cannot all be true at once")
})

test_that("extreme corners give up with advice instead of spinning", {
  skip_on_cran()
  r <- matrix(0.5, 5, 5)
  diag(r) <- 1
  set.seed(1)
  started <- Sys.time()
  expect_error(exact_mv(30, rep(3, 5), rep(1, 5), r, decimals = 0),
               "could not find data matching every target after 10,000 tries")
  expect_lt(as.numeric(Sys.time() - started, units = "secs"), 5)
})

test_that("typical calls are fast", {
  skip_on_cran()
  set.seed(1)
  elapsed <- system.time({
    exact_mv(30, 75.4, 8.2, decimals = 1)
    exact_mv(30, c(50, 55), c(10, 10), r = 0.7, decimals = 1)
    exact_mv(100, c(2.8, 2.6, 3), c(0.9, 0.85, 1),
             matrix(c(1, 0.4, 0.3, 0.4, 1, 0.2, 0.3, 0.2, 1), 3))
  })["elapsed"]
  expect_lt(elapsed, 1)
})
