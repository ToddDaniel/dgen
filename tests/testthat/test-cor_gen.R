test_that("cor_gen returns correct structure", {
  result <- cor_gen(
    n = 100,
    m  = c(5, 3),
    sd = c(1, 2),
    r  = 0.5,
    var_names = c("X1", "X2"),
    seed = 1
  )
  expect_true(is.data.frame(result$data))
  expect_equal(ncol(result$data), 2)
  expect_equal(nrow(result$data), 100)
  expect_named(result$data, c("X1", "X2"))
})

test_that("cor_gen achieves approximate target means", {
  result <- cor_gen(
    n  = 500,
    m  = c(10, 20),
    sd = c(2, 4),
    r  = 0.3,
    seed = 5
  )
  expect_equal(round(mean(result$data[[1]])), 10)
  expect_equal(round(mean(result$data[[2]])), 20)
})

test_that("cor_gen seed reproducibility", {
  args <- list(n = 80, m = c(0, 0), sd = c(1, 1), r = 0.4, seed = 22)
  r1 <- do.call(cor_gen, args)
  r2 <- do.call(cor_gen, args)
  expect_identical(r1$data, r2$data)
})

test_that("cor_gen three-variable call", {
  result <- cor_gen(
    n  = 100,
    m  = c(5, 3, 7),
    sd = c(1, 1, 2),
    r  = matrix(c(1, 0.3, 0.2,
                  0.3, 1, 0.4,
                  0.2, 0.4, 1), 3, 3),
    seed = 7
  )
  expect_equal(ncol(result$data), 3)
  expect_equal(nrow(result$data), 100)
})
