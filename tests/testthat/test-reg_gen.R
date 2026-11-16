# reg_gen(): the promise, checked on the returned data.

reported <- function(values, target, decimals) {
  all(abs(values - target) < 10^-(decimals + 1) / 2)
}

test_that("one predictor: means, SDs and the correlation are exact", {
  for (decimals in 0:2) {
    for (r in c(0.6, -0.35, 0.1)) {
      for (seed in 1:4) {
        data <- reg_gen(n = 40, m = c(250, 30), sd = c(40, 8), r = r,
                        decimals = decimals, seed = seed,
                        show_results = FALSE)
        expect_identical(class(data), "data.frame")
        expect_named(data, c("y", "x1"))
        expect_true(reported(colMeans(data), c(250, 30), decimals))
        expect_true(reported(vapply(data, stats::sd, 1), c(40, 8), decimals))
        expect_true(abs(stats::cor(data$y, data$x1) - r) < 0.005)
        expect_equal(data, round(data, decimals))
      }
    }
  }
})

test_that("several predictors: exact r with the outcome, 0.00 with each other", {
  m  <- c(250, 30, 12, 5.5)
  sd <- c(40, 8, 3, 1.2)
  r  <- c(0.45, -0.30, 0.20)
  for (decimals in 1:2) {
    for (seed in 1:5) {
      data <- reg_gen(n = 60, m = m, sd = sd, r = r, decimals = decimals,
                      seed = seed, show_results = FALSE)
      expect_named(data, c("y", "x1", "x2", "x3"))
      expect_true(reported(colMeans(data), m, decimals))
      expect_true(reported(vapply(data, stats::sd, 1), sd, decimals))
      correlations <- stats::cor(data)
      expect_true(all(abs(correlations[1, -1] - r) < 0.005))
      among <- correlations[-1, -1]
      expect_true(all(abs(among[upper.tri(among)]) < 0.005))
    }
  }
})

test_that("two predictors work with whole numbers at classroom sizes", {
  for (seed in 1:8) {
    data <- reg_gen(n = 30, m = c(75, 20, 50), sd = c(10, 5, 12),
                    r = c(0.5, 0.3), decimals = 0, seed = seed,
                    show_results = FALSE)
    expect_true(reported(colMeans(data), c(75, 20, 50), 0))
    expect_true(reported(vapply(data, stats::sd, 1), c(10, 5, 12), 0))
    expect_true(all(abs(stats::cor(data)[1, -1] - c(0.5, 0.3)) < 0.005))
    expect_true(abs(stats::cor(data$x1, data$x2)) < 0.005)
  }
})

test_that("the design shows in the regression: beta = r, R squared = sum r^2", {
  r <- c(0.45, -0.30)
  data <- reg_gen(n = 60, m = c(250, 30, 12), sd = c(40, 8, 3), r = r,
                  var_name = c("sales", "advertising", "price"),
                  decimals = 2, seed = 2, show_results = FALSE)
  fit  <- stats::lm(sales ~ advertising + price, data = data)
  beta <- stats::coef(fit)[-1] * c(8, 3) / 40
  expect_equal(unname(beta), r, tolerance = 0.02)
  expect_equal(summary(fit)$r.squared, sum(r^2), tolerance = 0.03)

  # one predictor is simple regression: slope = r * SD(y) / SD(x)
  ads <- reg_gen(n = 40, m = c(250, 30), sd = c(40, 8), r = 0.6,
                 decimals = 2, seed = 1, show_results = FALSE)
  expect_equal(unname(stats::coef(stats::lm(y ~ x1, data = ads))[2]),
               0.6 * 40 / 8, tolerance = 0.01)
})

test_that("the summary shows the regression R would give", {
  shown <- capture.output(
    data <- reg_gen(n = 60, m = c(250, 30, 12), sd = c(40, 8, 3),
                    r = c(0.45, -0.30),
                    var_name = c("sales", "advertising", "price"),
                    decimals = 1, seed = 2))
  model  <- summary(stats::lm(sales ~ advertising + price, data = data))
  slopes <- model$coefficients
  expect_true(any(grepl(
    "Multiple regression: sales on advertising and price", shown)))
  expect_true(any(grepl("sales and advertising +0.45 +0.45", shown)))
  expect_true(any(grepl("sales and price +-0.30 +-0.30", shown)))
  expect_true(any(grepl(
    "Correlations among the predictors: target 0.00, largest in the data 0.00",
    shown, fixed = TRUE)))
  expect_true(any(grepl(paste0(
    "advertising +", show_estimate(slopes["advertising", "Estimate"]), " +",
    show_estimate(slopes["advertising", "Std. Error"]), " +0.45 +",
    show_number(slopes["advertising", "t value"], 2)), shown)))
  expect_true(any(grepl("price +-3.998 +1.485 +-0.30 +-2.69 +.009", shown)))
  f <- model$fstatistic
  expect_true(any(grepl(paste0("F(2, 57) = ", show_number(f[["value"]], 2)),
                        shown, fixed = TRUE)))
  expect_true(any(grepl(paste0(
    "R squared = ", show_number(model$r.squared, 2),
    "   Adjusted R squared = ", show_number(model$adj.r.squared, 2)),
    shown, fixed = TRUE)))

  simple <- capture.output(
    reg_gen(n = 40, m = c(250, 30), sd = c(40, 8), r = 0.6,
            var_name = c("sales", "advertising"), decimals = 1, seed = 1))
  expect_true(any(grepl("Simple regression: sales on advertising", simple)))
  expect_false(any(grepl("among the predictors", simple)))
})

test_that("titles never show default names", {
  title <- function(data) dgen_results(data)$title
  expect_equal(title(reg_gen(40, c(250, 30), c(40, 8), 0.6, seed = 1,
                             show_results = FALSE)),
               "Simple regression")
  expect_equal(title(reg_gen(60, c(250, 30, 12), c(40, 8, 3), c(0.45, -0.3),
                             seed = 1, show_results = FALSE)),
               "Multiple regression with 2 predictors")
  expect_equal(title(anova_gen(12, c(70, 75, 82), c(8, 9, 8),
                               labels = c("North", "South", "West"),
                               layout = "wide", decimals = 0, seed = 1,
                               show_results = FALSE)),
               "One-way ANOVA: North, South and West")
  expect_equal(title(anova_gen(12, c(70, 75, 82), c(8, 9, 8), decimals = 0,
                               seed = 1, show_results = FALSE)),
               "One-way ANOVA: 3 groups")
  expect_equal(title(anova_gen(12, c(70, 75, 82), c(8, 9, 8),
                               var_name = "sales", decimals = 0, seed = 1,
                               show_results = FALSE)),
               "One-way ANOVA: sales by group")
  expect_equal(title(t_indep_gen(25, m = c(72, 78), sd = c(9, 10),
                                 decimals = 0, seed = 1,
                                 show_results = FALSE)),
               "Independent-samples t test: 2 groups")
  expect_equal(title(t_one_gen(25, m = 32, sd = 5, test_value = 30, seed = 1,
                               show_results = FALSE)),
               "One-sample t test against a test value of 30")
  expect_equal(title(t_paired_gen(30, c(50, 55), c(10, 10), 0.7, seed = 1,
                                  show_results = FALSE)),
               "Paired t test")
  expect_equal(title(cor_gen(30, c(50, 55), c(10, 10), 0.7, seed = 1,
                             show_results = FALSE)),
               "Correlation between two variables")
})

test_that("impossible designs are refused before any data are generated", {
  set.seed(1)
  before <- .Random.seed
  expect_error(reg_gen(40, c(1, 2, 3), c(1, 1, 1), r = c(0.8, 0.7)),
               "the squared values of r must add up to less than 1. Yours add up to 1.13",
               fixed = TRUE)
  expect_error(reg_gen(40, c(1, 2, 3), c(1, 1, 1), r = c(0.8, 0.6)),
               "add up to 1.00", fixed = TRUE)
  expect_error(reg_gen(40, c(1, 2), c(1, 1), r = 1), "between -1 and 1")
  expect_identical(.Random.seed, before)
})

test_that("problems are explained in plain language", {
  expect_error(reg_gen(40, c(250, 30), c(40, 8)), "reg_gen needs n, m, sd and r")
  expect_error(reg_gen(40, c(250, 30), c(40, 8), r = c(0.5, 0.3)),
               "With 2 value(s) of r, give 3 means and 3 SDs, the outcome first",
               fixed = TRUE)
  expect_error(reg_gen(40, c(250, 30, 12), c(40, 8, 3), r = diag(2)),
               "r must be one correlation for each predictor")
  expect_error(reg_gen(40, c(250, 30), c(40, 8), r = 0.455),
               "Use 0.45 or 0.46", fixed = TRUE)
  expect_error(reg_gen(40, c(250, 30), c(40, 8), r = 0.6, var_name = "sales"),
               "var_name must give a different name for each of the 2 variables",
               fixed = TRUE)
  expect_error(reg_gen(4, c(250, 30, 12), c(40, 8, 3), r = c(0.4, 0.3)),
               "n must be at least 5 for 3 variables", fixed = TRUE)
})

test_that("reg_gen is reproducible, silent on request, and invisible", {
  call <- function() reg_gen(40, c(250, 30), c(40, 8), 0.6, seed = 5,
                             show_results = FALSE)
  expect_silent(first <- withVisible(call()))
  expect_false(first$visible)
  expect_identical(first$value, call())
})

test_that("number display helpers behave", {
  expect_equal(show_number(c(-0.001, 0.2, -1.5), 2), c("0.00", "0.20", "-1.50"))
  expect_equal(show_estimate(c(160.02, 2.9996, 0.6492, 0.080012, -3.9977, 0)),
               c("160.0", "3.000", "0.6492", "0.08001", "-3.998", "0"))
  expect_equal(and_list(c("a", "b", "c")), "a, b and c")
  expect_equal(and_list(c("a", "b")), "a and b")
})
