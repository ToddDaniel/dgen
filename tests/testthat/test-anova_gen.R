# anova_gen(): the promise, checked on the returned data, in both layouts.

reported <- function(values, target, decimals) {
  all(abs(values - target) < 10^-(decimals + 1) / 2)
}

test_that("every group's mean and SD are exact in the returned data", {
  m  <- c(70, 75.5, 82, 77)
  sd <- c(8, 9.5, 8, 10)
  for (decimals in 0:2) {
    for (seed in 1:5) {
      data <- anova_gen(n = c(12, 15, 10, 20), m = m, sd = sd,
                        decimals = decimals, seed = seed,
                        show_results = FALSE)
      expect_identical(class(data), "data.frame")
      expect_named(data, c("group", "x"))
      expect_equal(as.vector(table(data$group)), c(12, 15, 10, 20))
      groups <- split(data$x, data$group)
      expect_true(reported(vapply(groups, mean, 1), m, decimals))
      expect_true(reported(vapply(groups, stats::sd, 1), sd, decimals))
      expect_equal(data$x, round(data$x, decimals))
    }
  }
})

test_that("one n serves every group; labels and var_name name the data", {
  data <- anova_gen(n = 12, m = c(70, 75, 82), sd = c(8, 9, 8),
                    labels = c("North", "South", "West"), var_name = "sales",
                    decimals = 0, seed = 1, show_results = FALSE)
  expect_named(data, c("group", "sales"))
  expect_equal(levels(data$group), c("North", "South", "West"))
  expect_equal(as.vector(table(data$group)), c(12, 12, 12))
  default <- anova_gen(n = 12, m = c(70, 75, 82), sd = c(8, 9, 8), seed = 1,
                       show_results = FALSE)
  expect_equal(levels(default$group), c("Group 1", "Group 2", "Group 3"))
})

test_that("wide layout gives one exact column per group", {
  m  <- c(70, 75, 82)
  sd <- c(8, 9, 8)
  wide <- anova_gen(n = 12, m = m, sd = sd,
                    labels = c("North", "South", "West"), layout = "wide",
                    decimals = 1, seed = 1, show_results = FALSE)
  expect_identical(class(wide), "data.frame")
  expect_equal(dim(wide), c(12, 3))
  expect_named(wide, c("North", "South", "West"))
  expect_true(reported(colMeans(wide), m, 1))
  expect_true(reported(vapply(wide, stats::sd, 1), sd, 1))

  # the same seed gives the same numbers in either layout
  long <- anova_gen(n = 12, m = m, sd = sd,
                    labels = c("North", "South", "West"), decimals = 1,
                    seed = 1, show_results = FALSE)
  expect_equal(unlist(wide, use.names = FALSE), long$x)
})

test_that("wide layout pads shorter groups, and the CSV has empty cells", {
  wide <- anova_gen(n = c(12, 10, 14), m = c(70, 75, 82), sd = c(8, 9, 8),
                    labels = c("North", "South", "West"), layout = "wide",
                    decimals = 0, seed = 1, show_results = FALSE)
  expect_equal(dim(wide), c(14, 3))
  expect_equal(colSums(!is.na(wide)), c(North = 12, South = 10, West = 14))
  expect_equal(mean(wide$South, na.rm = TRUE), 75)
  expect_true(all(is.na(wide$South[11:14])))
  expect_match(dgen_results(wide)$note, "Shorter groups end with empty")

  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  suppressMessages(save_csv(wide, file, "anova_gen"))
  expect_equal(readLines(file)[13], "71,,84")   # row 12: South has ended
  expect_equal(readLines(file)[15], ",,80")     # row 14: only West is left
  expect_false(any(grepl("NA", readLines(file))))
})

test_that("the summary shows the ANOVA R would give, with effect sizes", {
  shown <- capture.output(
    data <- anova_gen(n = 12, m = c(70, 75, 82), sd = c(8, 9, 8),
                      labels = c("North", "South", "West"),
                      var_name = "sales", decimals = 0, seed = 1))
  fit <- stats::anova(stats::lm(sales ~ group, data = data))
  expect_true(any(grepl("One-way ANOVA: sales by group", shown)))
  expect_true(any(grepl("North +12 +70.0 +70.0 +8.0 +8.0", shown)))
  expect_true(any(grepl(paste0("Between groups +", show_number(fit[1, "Sum Sq"], 2),
                               " +2 +", show_number(fit[1, "Mean Sq"], 2),
                               " +", show_number(fit[1, "F value"], 2),
                               " +", show_p(fit[1, "Pr(>F)"])), shown)))
  expect_true(any(grepl("Within groups +2286.00 +33", shown)))
  expect_true(any(grepl("Total +3158.00 +35", shown)))

  ss_between <- fit[1, "Sum Sq"]
  ss_within  <- fit[2, "Sum Sq"]
  ms_within  <- fit[2, "Mean Sq"]
  eta   <- ss_between / (ss_between + ss_within)
  omega <- (ss_between - 2 * ms_within) / (ss_between + ss_within + ms_within)
  expect_true(any(grepl(paste0("Eta squared = ", show_number(eta, 2),
                               "   Omega squared = ", show_number(omega, 2)),
                        shown, fixed = TRUE)))
})

test_that("exact means make the between-groups sum of squares exact", {
  # SS between = sum of n * (group mean - grand mean)^2, known from the design
  data <- anova_gen(n = 12, m = c(70, 75, 82), sd = c(8, 9, 8), decimals = 0,
                    seed = 3, show_results = FALSE)
  fit <- stats::anova(stats::lm(x ~ group, data = data))
  grand <- mean(c(70, 75, 82))
  expect_equal(fit[1, "Sum Sq"], 12 * sum((c(70, 75, 82) - grand)^2))
})

test_that("with two groups, F is the square of the equal-variances t", {
  data <- anova_gen(n = c(20, 25), m = c(50, 56), sd = c(10, 11),
                    decimals = 1, seed = 2, show_results = FALSE)
  f <- stats::anova(stats::lm(x ~ group, data = data))[1, "F value"]
  t <- stats::t.test(x ~ group, data = data, var.equal = TRUE)$statistic
  expect_equal(f, unname(t^2))
})

test_that("anova_gen is reproducible, silent on request, and invisible", {
  call <- function() anova_gen(10, m = c(5, 6, 7), sd = c(1, 1, 1), seed = 4,
                               show_results = FALSE)
  expect_silent(first <- withVisible(call()))
  expect_false(first$visible)
  expect_identical(first$value, call())
})

test_that("problems are explained in plain language", {
  expect_error(anova_gen(12, m = c(70, 75, 82)), "anova_gen needs n, m and sd")
  expect_error(anova_gen(12, m = 70, sd = 8), "two or more groups")
  expect_error(anova_gen(12, m = c(70, 75, 82), sd = c(8, 9)),
               "3 mean(s) and 2 SD(s)", fixed = TRUE)
  expect_error(anova_gen(c(12, 10), m = c(70, 75, 82), sd = c(8, 9, 8)),
               "n must be one sample size for all groups, or one for each of the 3 groups",
               fixed = TRUE)
  expect_error(anova_gen(12, m = c(70, 75, 82), sd = c(8, 9, 8),
                         labels = c("A", "B")),
               "labels must give a different name for each of the 3 groups",
               fixed = TRUE)
  expect_error(anova_gen(12, m = c(70, 75, 82), sd = c(8, 9, 8),
                         layout = "tall"),
               "layout must be \"long\" (one row per observation) or \"wide\"",
               fixed = TRUE)
  expect_error(anova_gen(c(12, 5, 12), m = c(70, 75.5, 82), sd = c(8, 9, 8),
                         decimals = 0),
               "Group 2: With 5 whole-number values, the mean cannot be 75.5",
               fixed = TRUE)
})
