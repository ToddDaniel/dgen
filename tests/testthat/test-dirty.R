# Tests for dirty_data()

make_clean <- function(n = 40) {
  data.frame(
    id     = seq_len(n),
    score  = round(stats::rnorm(n, 50, 10), 2),
    group  = rep(c("Male", "Female"), length.out = n),
    stringsAsFactors = FALSE
  )
}

test_that("dirty_data plants exactly the requested defects", {
  set.seed(1)
  clean <- make_clean()
  d <- suppressMessages(dirty_data(clean, missing = 3, outliers = 2,
                                   typos = 2, inconsistent = 2,
                                   duplicates = 2, seed = 42))
  expect_equal(nrow(d$data), 42)                    # 40 + 2 duplicates
  expect_equal(nrow(d$answer_key), 11)              # total defects
  expect_equal(unname(d$defect_summary),
               c(3, 2, 2, 2, 2))
  expect_equal(sum(d$answer_key$defect_type == "missing"), 3)
  expect_equal(sum(d$answer_key$defect_type == "outlier"), 2)
  expect_equal(sum(d$answer_key$defect_type == "typo"), 2)
  expect_equal(sum(d$answer_key$defect_type == "inconsistent"), 2)
  expect_equal(sum(d$answer_key$defect_type == "duplicate"), 2)
  expect_equal(sum(is.na(d$data$score)) + sum(is.na(d$data$id)) +
                 sum(is.na(d$data$group)), 3)
  expect_identical(d$clean_data, clean)
})

test_that("the answer key describes the dirty data accurately", {
  set.seed(2)
  clean <- make_clean()
  d <- suppressMessages(dirty_data(clean, missing = 3, outliers = 2,
                                   typos = 2, inconsistent = 2,
                                   seed = 9))
  key <- d$answer_key
  cell_key <- key[key$defect_type != "duplicate", ]
  for (i in seq_len(nrow(cell_key))) {
    r  <- cell_key$row[i]
    cl <- cell_key$column[i]
    if (cell_key$defect_type[i] == "missing") {
      expect_true(is.na(d$data[r, cl]))
    } else {
      expect_equal(as.character(d$data[r, cl]), cell_key$corrupted[i])
    }
    expect_equal(as.character(d$clean_data[r, cl]), cell_key$original[i])
  }
})

test_that("no cell receives two defects", {
  set.seed(3)
  d <- suppressMessages(dirty_data(make_clean(), missing = 5, outliers = 4,
                                   typos = 4, inconsistent = 4, seed = 5))
  cell_key <- d$answer_key[d$answer_key$defect_type != "duplicate", ]
  ids <- paste(cell_key$row, cell_key$column)
  expect_equal(anyDuplicated(ids), 0)
})

test_that("outliers are statistically extreme relative to the clean column", {
  set.seed(4)
  clean <- make_clean(100)
  d <- suppressMessages(dirty_data(clean, outliers = 3, columns = "score",
                                   seed = 11))
  key <- d$answer_key[d$answer_key$defect_type == "outlier", ]
  m <- mean(clean$score); s <- stats::sd(clean$score)
  for (v in as.numeric(key$corrupted)) {
    expect_gt(abs(v - m), 3 * s)
  }
})

test_that("typos differ from originals and inconsistent codings normalize back", {
  set.seed(5)
  d <- suppressMessages(dirty_data(make_clean(), typos = 3,
                                   inconsistent = 3, seed = 13))
  key <- d$answer_key
  typo_key <- key[key$defect_type == "typo", ]
  expect_true(all(typo_key$original != typo_key$corrupted))
  inc_key <- key[key$defect_type == "inconsistent", ]
  expect_true(all(inc_key$original != inc_key$corrupted))
  expect_true(all(trimws(tolower(inc_key$original)) ==
                    trimws(tolower(inc_key$corrupted))))
})

test_that("duplicate rows are genuine copies at recorded positions", {
  set.seed(6)
  d <- suppressMessages(dirty_data(make_clean(), duplicates = 3, seed = 21))
  expect_equal(nrow(d$data), 43)
  expect_gte(sum(duplicated(d$data) | duplicated(d$data, fromLast = TRUE)), 3)
  dup_key <- d$answer_key[d$answer_key$defect_type == "duplicate", ]
  expect_equal(nrow(dup_key), 3)
  expect_true(all(is.na(dup_key$column)))
  for (i in seq_len(nrow(dup_key))) {
    src <- as.integer(sub("duplicate of row ", "", dup_key$original[i]))
    expect_identical(unname(unlist(d$data[dup_key$row[i], ])),
                     unname(unlist(d$data[src, ])))
  }
})

test_that("answer_key = FALSE returns NULL key but still corrupts", {
  set.seed(7)
  d <- suppressMessages(dirty_data(make_clean(), missing = 2,
                                   answer_key = FALSE, seed = 3))
  expect_null(d$answer_key)
  expect_equal(sum(is.na(d$data)), 2)
})

test_that("dirty_data accepts a dgen result and composes with dist_gen", {
  suppressMessages(gen <- stat_gen(n = 30, m = 50, sd = 10, seed = 42))
  d <- suppressMessages(dirty_data(gen, missing = 2, outliers = 1,
                                   seed = 8))
  expect_equal(nrow(d$data), 30)
  expect_equal(nrow(d$answer_key), 3)

  suppressMessages(sk <- dist_gen(n = 40, m = 100, sd = 15,
                                  shape = "right_skew", seed = 1))
  d2 <- suppressMessages(dirty_data(sk, missing = 3, outliers = 2,
                                    seed = 2))
  expect_equal(nrow(d2$answer_key), 5)
})

test_that("dirty_data accepts a CSV path as input", {
  path <- file.path(tempdir(), "clean_input.csv")
  utils::write.csv(make_clean(20), path, row.names = FALSE)
  d <- suppressMessages(dirty_data(path, missing = 2, seed = 4))
  expect_equal(nrow(d$data), 20)
  expect_equal(sum(is.na(d$data)), 2)
  file.remove(path)
  expect_error(suppressMessages(dirty_data("no_such_file.csv", missing = 1)),
               "not found")
})

test_that("columns restriction protects other columns", {
  set.seed(8)
  clean <- make_clean()
  d <- suppressMessages(dirty_data(clean, missing = 4, outliers = 2,
                                   columns = "score", seed = 17))
  expect_true(all(d$answer_key$column == "score"))
  expect_identical(d$data$id, clean$id)
  expect_identical(d$data$group, clean$group)
})

test_that("dirty_data validates inputs and eligibility", {
  clean <- make_clean()
  expect_error(suppressMessages(dirty_data(clean)), "all defect counts")
  expect_error(suppressMessages(dirty_data(clean, missing = -1)),
               "non-negative")
  expect_error(suppressMessages(dirty_data(clean, missing = 1,
                                           columns = "nope")),
               "not in the data")
  expect_error(suppressMessages(dirty_data(42, missing = 1)),
               "data frame")
  num_only <- clean[, c("id", "score")]
  expect_error(suppressMessages(dirty_data(num_only, inconsistent = 1)),
               "character/factor")
  cat_only <- clean[, "group", drop = FALSE]
  expect_error(suppressMessages(dirty_data(cat_only, outliers = 1)),
               "numeric")
  tiny <- clean[1:2, "score", drop = FALSE]
  expect_error(suppressMessages(dirty_data(tiny, missing = 3)),
               "Not enough eligible cells")
})

test_that("factor columns receiving inconsistent codings become character", {
  clean <- data.frame(g = factor(rep(c("Yes", "No"), 10)),
                      x = rnorm(20))
  d <- suppressMessages(dirty_data(clean, inconsistent = 2, seed = 5))
  expect_type(d$data$g, "character")
})

test_that("dirty_data is reproducible with a seed", {
  clean <- make_clean()
  a <- suppressMessages(dirty_data(clean, missing = 3, outliers = 2,
                                   duplicates = 1, seed = 33))
  b <- suppressMessages(dirty_data(clean, missing = 3, outliers = 2,
                                   duplicates = 1, seed = 33))
  expect_identical(a$data, b$data)
  expect_identical(a$answer_key, b$answer_key)
})

test_that("export_csv writes only the dirty data, never the key", {
  dest <- file.path(tempdir(), "dirty_out.csv")
  if (file.exists(dest)) file.remove(dest)
  suppressMessages(dirty_data(make_clean(20), missing = 2, seed = 6,
                              export_csv = dest))
  expect_true(file.exists(dest))
  reread <- utils::read.csv(dest)
  expect_equal(nrow(reread), 20)
  expect_false("defect_type" %in% names(reread))
  file.remove(dest)
})
