# Tests for batch_gen()

test_that("batch_gen produces the requested number of distinct datasets", {
  suppressMessages(b <- batch_gen(stat_gen, n_datasets = 3,
                                  n = 15, m = 50, sd = 8, seeds = 42))
  expect_length(b$results, 3)
  expect_equal(nrow(b$answer_key), 3)
  expect_equal(b$seeds, c(42, 43, 44))
  expect_equal(b$generator, "stat_gen")
  # Different seeds produce different data
  expect_false(identical(b$results[[1]]$data, b$results[[2]]$data))
})

test_that("batch_gen is reproducible for the same seeds", {
  suppressMessages(b1 <- batch_gen(stat_gen, n_datasets = 2,
                                   n = 15, m = 50, sd = 8, seeds = 7))
  suppressMessages(b2 <- batch_gen(stat_gen, n_datasets = 2,
                                   n = 15, m = 50, sd = 8, seeds = 7))
  expect_identical(b1$answer_key, b2$answer_key)
  expect_identical(b1$results[[1]]$data, b2$results[[1]]$data)
})

test_that("batch_gen accepts an explicit seed vector and validates length", {
  suppressMessages(b <- batch_gen(stat_gen, n_datasets = 3,
                                  n = 15, m = 50, sd = 8,
                                  seeds = c(5, 50, 500)))
  expect_equal(b$answer_key$seed, c(5, 50, 500))
  expect_error(
    suppressMessages(batch_gen(stat_gen, n_datasets = 3,
                               n = 15, m = 50, sd = 8, seeds = c(1, 2))),
    "length")
})

test_that("batch_gen records random seeds when seeds = NULL", {
  suppressMessages(b <- batch_gen(stat_gen, n_datasets = 2,
                                  n = 15, m = 50, sd = 8))
  expect_length(b$seeds, 2)
  expect_true(all(!is.na(b$answer_key$seed)))
  # A recorded seed regenerates its dataset exactly
  suppressMessages(again <- stat_gen(n = 15, m = 50, sd = 8,
                                     seed = b$seeds[1]))
  expect_identical(again$data, b$results[[1]]$data)
})

test_that("batch_gen warns on duplicated seeds", {
  expect_warning(
    suppressMessages(batch_gen(stat_gen, n_datasets = 2,
                               n = 15, m = 50, sd = 8, seeds = c(3, 3))),
    "Duplicated")
})

test_that("batch_gen rejects seed in ... and non-seed generators", {
  expect_error(
    suppressMessages(batch_gen(stat_gen, n_datasets = 2,
                               n = 15, m = 50, sd = 8, seed = 1)),
    "seeds")
  no_seed_fun <- function(x) list(x = x)
  expect_error(
    suppressMessages(batch_gen(no_seed_fun, n_datasets = 2, x = 1)),
    "seed")
})

test_that("batch_gen accepts a generator by name", {
  suppressMessages(b <- batch_gen("stat_gen", n_datasets = 2,
                                  n = 15, m = 50, sd = 8, seeds = 1))
  expect_equal(b$generator, "stat_gen")
  expect_length(b$results, 2)
})

test_that("default and custom dataset labels work", {
  suppressMessages(b <- batch_gen(stat_gen, n_datasets = 2,
                                  n = 15, m = 50, sd = 8, seeds = 1))
  expect_equal(b$answer_key$dataset, c("Dataset_01", "Dataset_02"))
  expect_named(b$results, c("Dataset_01", "Dataset_02"))
  suppressMessages(b2 <- batch_gen(stat_gen, n_datasets = 2,
                                   n = 15, m = 50, sd = 8, seeds = 1,
                                   dataset_labels = c("Ann", "Bob")))
  expect_equal(b2$answer_key$dataset, c("Ann", "Bob"))
  expect_error(
    suppressMessages(batch_gen(stat_gen, n_datasets = 2,
                               n = 15, m = 50, sd = 8, seeds = 1,
                               dataset_labels = c("Ann", "Ann"))),
    "unique")
})

test_that("the answer key flattens scalars, one-row frames, and htests", {
  suppressMessages(b <- batch_gen(
    t_indep_gen, n_datasets = 2,
    n1 = 12, mean1 = 100, sd1 = 15, label1 = "Control",
    n2 = 12, mean2 = 110, sd2 = 15, label2 = "Treatment",
    seeds = 42))
  key <- b$answer_key
  # scalar effect size from the result
  expect_true("cohens_d" %in% names(key))
  expect_true(is.numeric(key$cohens_d))
  # htest components appear as *_statistic / *_df / *_p_value
  p_cols <- grep("_p_value$", names(key), value = TRUE)
  expect_gt(length(p_cols), 0)
  expect_true(all(key[[p_cols[1]]] >= 0 & key[[p_cols[1]]] <= 1))
  # stat_gen's one-row 'achieved' frame flattens column by column
  suppressMessages(s <- batch_gen(stat_gen, n_datasets = 2,
                                  n = 15, m = 50, sd = 8, seeds = 1))
  expect_true(any(grepl("^achieved_", names(s$answer_key))))
})

test_that("ragged result structures fill with NA", {
  fake_gen <- function(value, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    out <- list(data = data.frame(x = value), score = value)
    if (seed %% 2 == 0) out$extra <- 99
    out
  }
  suppressMessages(b <- batch_gen(fake_gen, n_datasets = 3, value = 1,
                                  seeds = c(1, 2, 3)))
  expect_true("extra" %in% names(b$answer_key))
  expect_equal(is.na(b$answer_key$extra), c(TRUE, FALSE, TRUE))
})

test_that("quiet = TRUE suppresses generator messages, quiet = FALSE shows them", {
  msgs_quiet <- testthat::capture_messages(
    batch_gen(stat_gen, n_datasets = 2, n = 15, m = 50, sd = 8, seeds = 1))
  expect_false(any(grepl("Variable generated", msgs_quiet)))
  msgs_loud <- testthat::capture_messages(
    batch_gen(stat_gen, n_datasets = 2, n = 15, m = 50, sd = 8, seeds = 1,
              quiet = FALSE))
  expect_true(any(grepl("Variable generated", msgs_loud)))
  # batch_gen's own summary appears either way
  expect_true(any(grepl("batch_gen: 2 datasets", msgs_quiet)))
})

test_that("export_csv writes one CSV per dataset plus the answer key", {
  dir <- file.path(tempdir(), "batch_export_test")
  unlink(dir, recursive = TRUE)
  suppressMessages(batch_gen(stat_gen, n_datasets = 3,
                             n = 15, m = 50, sd = 8, seeds = 1,
                             export_csv = dir))
  expect_true(dir.exists(dir))
  files <- list.files(dir)
  expect_setequal(files, c("Dataset_01.csv", "Dataset_02.csv",
                           "Dataset_03.csv", "answer_key.csv"))
  key <- utils::read.csv(file.path(dir, "answer_key.csv"))
  expect_equal(nrow(key), 3)
  expect_true("seed" %in% names(key))
  unlink(dir, recursive = TRUE)
})

test_that("export_csv rejects invalid values", {
  expect_error(
    suppressMessages(batch_gen(stat_gen, n_datasets = 2,
                               n = 15, m = 50, sd = 8, seeds = 1,
                               export_csv = 123)),
    "export_csv")
})

test_that("batch_gen works with mcnemar_gen (closed-form generator)", {
  suppressMessages(b <- batch_gen(mcnemar_gen, n_datasets = 3,
                                  n = 60, p1 = 0.50, p2 = 0.70,
                                  seeds = 10))
  expect_equal(nrow(b$answer_key), 3)
  expect_true("cohens_g" %in% names(b$answer_key))
  expect_true(any(grepl("mcnemar_test_results_p_value",
                        names(b$answer_key))))
})
