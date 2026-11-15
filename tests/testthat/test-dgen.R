# dgen(): the promise, checked on the returned data.

expect_reported <- function(values, target, decimals) {
  expect_true(abs(values - target) < 10^-(decimals + 1) / 2)
}

test_that("the returned data have exactly the mean and SD asked for", {
  for (decimals in 0:3) {
    for (n in c(10, 30, 200)) {
      for (seed in 1:5) {
        data <- dgen(n, m = 75.4, sd = 8.2, decimals = decimals, seed = seed,
                     show_results = FALSE)
        expect_reported(mean(data$x), 75.4, decimals)
        expect_reported(stats::sd(data$x), 8.2, decimals)
        expect_equal(data$x, round(data$x, decimals))
        expect_equal(nrow(data), n)
      }
    }
  }
})

test_that("dgen returns a plain data frame, invisibly", {
  out <- withVisible(dgen(20, 50, 10, seed = 1, show_results = FALSE))
  expect_false(out$visible)
  expect_identical(class(out$value), "data.frame")
  expect_named(out$value, "x")
  named <- dgen(20, 50, 10, var_name = "score", seed = 1,
                show_results = FALSE)
  expect_named(named, "score")
})

test_that("several means give several independent exact variables", {
  data <- dgen(40, m = c(100, 50, 3.2), sd = c(15, 10, 0.8), decimals = 1,
               var_name = c("iq", "t_score", "rating"), seed = 3,
               show_results = FALSE)
  expect_equal(dim(data), c(40, 3))
  expect_named(data, c("iq", "t_score", "rating"))
  targets_m  <- c(100, 50, 3.2)
  targets_sd <- c(15, 10, 0.8)
  for (j in 1:3) {
    expect_reported(mean(data[[j]]), targets_m[j], 1)
    expect_reported(stats::sd(data[[j]]), targets_sd[j], 1)
  }
  expect_named(dgen(20, c(1, 2), c(1, 1), seed = 1, show_results = FALSE),
               c("x1", "x2"))
})

test_that("a seed makes the data reproducible; no seed gives new data", {
  first  <- dgen(30, 75.4, 8.2, decimals = 1, seed = 42, show_results = FALSE)
  second <- dgen(30, 75.4, 8.2, decimals = 1, seed = 42, show_results = FALSE)
  expect_identical(first, second)
  other <- dgen(30, 75.4, 8.2, decimals = 1, seed = 43, show_results = FALSE)
  expect_false(identical(first$x, other$x))
})

test_that("show_results prints targets beside the data, and can be silent", {
  expect_silent(dgen(30, 75.4, 8.2, decimals = 1, seed = 1,
                     show_results = FALSE))
  shown <- capture.output(
    data <- dgen(30, 75.4, 8.2, decimals = 1, var_name = "score", seed = 1))
  expect_true(any(grepl("One variable with an exact mean and SD", shown)))
  expect_true(any(grepl("score +30 +75.40 +75.40 +8.20 +8.20", shown)))
  expect_true(any(grepl("30 rows, 1 column (score). Rounded to 1 decimal place.",
                        shown, fixed = TRUE)))
  again <- capture.output(print(dgen_results(data)))
  expect_true(any(grepl("score +30 +75.40 +75.40 +8.20 +8.20", again)))

  results <- dgen_results(data)
  expect_equal(results$table$Smallest, show_number(min(data$score), 1))
  expect_equal(results$table$Largest, show_number(max(data$score), 1))
})

test_that("export_csv saves the data under the function's name", {
  folder <- tempfile("dgen-export")
  dir.create(folder)
  old <- setwd(folder)
  on.exit({
    setwd(old)
    unlink(folder, recursive = TRUE)
  })
  expect_message(
    data <- dgen(20, 50, 10, decimals = 0, seed = 1, export_csv = TRUE,
                 show_results = FALSE),
    "Saved: ")
  expect_equal(list.files(folder), "dgen.csv")
  expect_equal(utils::read.csv("dgen.csv")$x, data$x)
})

test_that("problems are explained in plain language", {
  expect_error(dgen(30), "dgen needs n, m and sd", fixed = TRUE)
  expect_error(dgen(c(20, 30), 50, 10), "single whole number")
  expect_error(dgen(5, 75.5, 8.2, decimals = 0),
               "With 5 whole-number values, the mean cannot be 75.5",
               fixed = TRUE)
  expect_error(dgen(5, c(75, 75.5), c(8, 8), decimals = 0),
               "Variable 2: With 5 whole-number values", fixed = TRUE)
  expect_error(dgen(30, c(50, 60), 10), "2 mean(s) and 1 SD(s)",
               fixed = TRUE)
  expect_error(dgen(30, 50, 10, var_name = c("a", "b")),
               "var_name must give one name in quotes", fixed = TRUE)
  expect_error(dgen(30, 50, 10, seed = "abc"), "seed must be a single whole")
  expect_error(dgen(30, 50, 10, seed = 1, show_results = "no"),
               "show_results must be TRUE or FALSE")
  expect_error(dgen(30, 50, 10, seed = 1, show_results = FALSE,
                    export_csv = 5),
               "export_csv must be")
})
