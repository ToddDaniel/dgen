# exact_groups() and exact_counts(): the promise, checked on what is returned.

test_that("every group has exactly its own mean and SD", {
  for (decimals in 0:2) {
    for (seed in 1:5) {
      set.seed(seed)
      groups <- exact_groups(n = c(12, 20, 35), m = c(72, 78.5, 81),
                             sd = c(9, 10.5, 8), decimals = decimals)
      expect_length(groups, 3)
      expect_equal(lengths(groups), c(12, 20, 35))
      step <- 10^-(decimals + 1)
      expect_true(all(abs(vapply(groups, mean, 1) - c(72, 78.5, 81)) <
                        step / 2))
      expect_true(all(abs(vapply(groups, stats::sd, 1) - c(9, 10.5, 8)) <
                        step / 2))
    }
  }
})

test_that("one n is used for every group, and group means are exact", {
  set.seed(1)
  groups <- exact_groups(n = 25, m = c(72, 78), sd = c(9, 10), decimals = 0)
  expect_equal(lengths(groups), c(25, 25))
  expect_equal(mean(groups[[2]]) - mean(groups[[1]]), 6)
})

test_that("group problems are reported with the group they belong to", {
  expect_error(exact_groups(n = c(20, 5), m = c(72, 75.5), sd = c(9, 8),
                            decimals = 0),
               "Group 2: With 5 whole-number values, the mean cannot be 75.5",
               fixed = TRUE)
  expect_error(exact_groups(n = c(20, 20, 20), m = c(72, 75), sd = c(9, 8)),
               "n must be one sample size for all groups, or one for each of the 2 groups",
               fixed = TRUE)
  expect_error(exact_groups(n = 20, m = c(72, 75), sd = 9),
               "2 mean(s) and 1 SD(s)", fixed = TRUE)
})

test_that("a vector of counts becomes one row per observation", {
  set.seed(1)
  data <- exact_counts(c(Happy = 56, Tragic = 24))
  expect_s3_class(data, "data.frame")
  expect_equal(dim(data), c(80, 1))
  expect_equal(as.vector(table(data$category)), c(56, 24))
  expect_equal(levels(data$category), c("Happy", "Tragic"))
  # shuffled, not sorted
  expect_false(identical(as.character(data$category),
                         rep(c("Happy", "Tragic"), c(56, 24))))
})

test_that("a table of counts becomes two columns with the same table", {
  counts <- matrix(c(30, 10, 0,
                     20, 25, 15), nrow = 2, byrow = TRUE,
                   dimnames = list(c("Online", "Classroom"),
                                   c("Pass", "Fail", "Withdrew")))
  set.seed(2)
  data <- exact_counts(counts)
  expect_equal(dim(data), c(100, 2))
  rebuilt <- table(data$row, data$column)
  expect_equal(matrix(rebuilt, nrow = 2), unname(counts))
  expect_equal(rownames(rebuilt), c("Online", "Classroom"))
  expect_equal(colnames(rebuilt), c("Pass", "Fail", "Withdrew"))
  expect_equal(as.vector(table(exact_counts(as.table(counts))$row)),
               c(40, 60))
})

test_that("counts without names are numbered, and bad counts are refused", {
  expect_equal(levels(exact_counts(c(3, 2))$category), c("1", "2"))
  expect_error(exact_counts(c(Yes = 5.5, No = 3)), "counts must be whole")
  expect_error(exact_counts(c(Yes = -1, No = 3)), "counts must be whole")
  expect_error(exact_counts(c(Yes = 0, No = 0)), "counts must be whole")
  expect_error(exact_counts(c(Yes = 2, Yes = 3)), "its own name")
})
