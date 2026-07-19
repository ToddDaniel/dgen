# Tests for the nonparametric family: sign_gen, mannwhitney_gen,
# signedrank_gen, kruskal_gen, friedman_gen, spearman_gen

# ---- mannwhitney_gen ----

test_that("mannwhitney_gen hits exact medians with integer data", {
  suppressMessages(r <- mannwhitney_gen(
    n1 = 12, median1 = 66, label1 = "Urban",
    n2 = 14, median2 = 85, label2 = "Resort",
    spread = 25, seed = 42))
  expect_equal(r$group_stats$Median, c(66, 85))
  expect_true(all(r$data$Score == round(r$data$Score)))
  expect_equal(levels(r$data$Group), c("Urban", "Resort"))
  expect_s3_class(r$test_results, "htest")
})

test_that("mannwhitney_gen reported stats agree with recomputation", {
  suppressMessages(r <- mannwhitney_gen(
    n1 = 15, median1 = 50, n2 = 15, median2 = 65,
    spread = 12, seed = 7))
  g1 <- r$data$Score[r$data$Group == "Group1"]
  g2 <- r$data$Score[r$data$Group == "Group2"]
  ref <- suppressWarnings(stats::wilcox.test(g1, g2, correct = TRUE))
  expect_equal(r$test_results$p.value, ref$p.value)
  expect_equal(as.numeric(r$test_results$statistic),
               as.numeric(ref$statistic))
  # rank-biserial from U
  U1 <- as.numeric(ref$statistic)
  expect_equal(r$effect_sizes$rank_biserial, 2 * U1 / (15 * 15) - 1)
  # z consistent with the reported p
  expect_equal(2 * stats::pnorm(-abs(r$z)), r$test_results$p.value,
               tolerance = 1e-10)
})

test_that("mannwhitney_gen respects bounds and skew direction", {
  suppressMessages(r <- mannwhitney_gen(
    n1 = 21, median1 = 40, n2 = 21, median2 = 55,
    spread = 15, scale_min = 1, scale_max = 100,
    shape = "right_skew", severity = "severe", seed = 42))
  expect_true(all(r$data$Score >= 1 & r$data$Score <= 100))
  expect_gt(sample_skewness(r$data$Score), 0)
})

test_that("mannwhitney_gen rejects unachievable medians", {
  # odd n, decimals = 0: median must be an integer
  expect_error(suppressMessages(mannwhitney_gen(
    n1 = 11, median1 = 65.5, n2 = 12, median2 = 70,
    spread = 10)), "not achievable")
  # even n, decimals = 0: half-integers allowed, quarter not
  expect_error(suppressMessages(mannwhitney_gen(
    n1 = 12, median1 = 65.25, n2 = 12, median2 = 70,
    spread = 10)), "not achievable")
  expect_error(suppressMessages(mannwhitney_gen(
    n1 = 12, median1 = 105, n2 = 12, median2 = 70,
    spread = 10, scale_max = 100)), "below scale_max")
})

test_that("mannwhitney_gen even-n half-step medians work", {
  suppressMessages(r <- mannwhitney_gen(
    n1 = 12, median1 = 65.5, n2 = 14, median2 = 70,
    spread = 10, seed = 3))
  expect_equal(r$group_stats$Median[1], 65.5)
})

# ---- kruskal_gen ----

test_that("kruskal_gen hits exact medians and reports tie-corrected H", {
  suppressMessages(r <- kruskal_gen(
    k = 3, n = 12, medians = c(60, 66, 78),
    labels = c("A", "B", "C"), spread = 10,
    scale_min = 1, scale_max = 100, seed = 42))
  expect_equal(r$group_stats$Median, c(60, 66, 78))
  ref <- stats::kruskal.test(r$data$Score, r$data$Group)
  expect_equal(r$test_results$p.value, ref$p.value)
  H <- as.numeric(ref$statistic); N <- 36
  expect_equal(r$effect_sizes$epsilon_squared, H * (N + 1) / (N^2 - 1))
  expect_equal(r$effect_sizes$eta_squared_H, (H - 3 + 1) / (N - 3))
})

test_that("kruskal_gen runs Dunn's post-hoc only when significant", {
  suppressMessages(sig <- kruskal_gen(
    k = 3, n = 15, medians = c(30, 50, 70), spread = 8, seed = 42))
  expect_lt(sig$test_results$p.value, 0.05)
  expect_s3_class(sig$posthoc_dunn, "data.frame")
  expect_equal(nrow(sig$posthoc_dunn), 3)
  expect_true(all(sig$posthoc_dunn$p_holm >= sig$posthoc_dunn$p_raw - 1e-12))

  suppressMessages(ns <- kruskal_gen(
    k = 3, n = 8, medians = c(50, 50, 51), spread = 20, seed = 42))
  expect_gte(ns$test_results$p.value, 0.05)
  expect_null(ns$posthoc_dunn)
})

test_that("kruskal_gen supports unequal group sizes", {
  suppressMessages(r <- kruskal_gen(
    k = 3, n = c(15, 12, 18), medians = c(60, 63, 77),
    spread = 10, seed = 1))
  expect_equal(r$group_stats$n, c(15, 12, 18))
  expect_equal(nrow(r$data), 45)
})

# ---- signedrank_gen ----

test_that("signedrank_gen hits exact medians and matches wilcox.test", {
  suppressMessages(r <- signedrank_gen(
    n = 18, median1 = 6, median2 = 5, spread = 2,
    scale_min = 1, seed = 42))
  expect_equal(r$measure_stats$Median, c(6, 5))
  m1 <- r$data$Measure1; m2 <- r$data$Measure2
  ref <- suppressWarnings(stats::wilcox.test(m2, m1, paired = TRUE,
                                             correct = TRUE))
  expect_equal(r$test_results$p.value, ref$p.value)
  expect_equal(r$n_used, sum(m2 - m1 != 0))
  # matched rank-biserial in [-1, 1]
  expect_gte(r$effect_sizes$rank_biserial, -1)
  expect_lte(r$effect_sizes$rank_biserial, 1)
})

test_that("signedrank_gen correlation is roughly achieved", {
  suppressMessages(r <- signedrank_gen(
    n = 60, median1 = 20, median2 = 24, spread = 6,
    correlation = 0.7, decimals = 1, seed = 42))
  expect_gt(r$correlation_achieved, 0.4)
})

# ---- friedman_gen ----

test_that("friedman_gen hits exact medians and matches friedman.test", {
  suppressMessages(r <- friedman_gen(
    n = 12, k = 3, medians = c(5, 6, 8),
    labels = c("T1", "T2", "T3"), spread = 2,
    scale_min = 1, scale_max = 10, seed = 42))
  expect_equal(unname(r$condition_stats$Median), c(5, 6, 8))
  m <- as.matrix(r$data[, c("T1", "T2", "T3")])
  ref <- stats::friedman.test(m)
  expect_equal(r$test_results$p.value, ref$p.value)
  expect_equal(r$kendalls_w,
               as.numeric(ref$statistic) / (12 * 2))
  expect_true(all(r$data_long$Score >= 1 & r$data_long$Score <= 10))
})

test_that("friedman_gen posthoc appears only when significant", {
  suppressMessages(sig <- friedman_gen(
    n = 20, k = 3, medians = c(3, 5, 8), spread = 1.5, seed = 42))
  expect_lt(sig$test_results$p.value, 0.05)
  expect_s3_class(sig$posthoc, "data.frame")
  expect_equal(nrow(sig$posthoc), 3)
})

# ---- sign_gen ----

test_that("sign_gen median mode plants exactly k above the target", {
  suppressMessages(r <- sign_gen(n = 11, k = 2, target_median = 9000,
                                 spread = 800, seed = 42))
  above <- sum(r$data$Score > 9000)
  below <- sum(r$data$Score < 9000)
  expect_equal(above, 2)
  expect_equal(below, 9)
  expect_equal(sum(r$data$Score == 9000), 0)   # no ties by construction
  # exact test reproduces binom.test
  ref <- stats::binom.test(2, 11, 0.5)
  expect_equal(r$exact_test$p.value, ref$p.value)
})

test_that("sign_gen z matches the continuity-corrected formula", {
  suppressMessages(r <- sign_gen(n = 45, k = 31, target_median = 15000,
                                 spread = 2000, seed = 1))
  z_manual <- (abs(31 - 22.5) - 0.5) / sqrt(0.25 * 45)
  expect_equal(abs(r$z), z_manual)
  expect_gt(r$z, 0)                             # k above half -> positive
  expect_equal(r$effect_sizes$r, r$z / sqrt(45))
  expect_equal(r$effect_sizes$p_hat_minus_half, 31 / 45 - 0.5)
  expect_match(r$recommendation, "n > 20")
})

test_that("sign_gen preference mode plants exactly k choices", {
  suppressMessages(r <- sign_gen(n = 15, k = 12, mode = "preference",
                                 category_labels = c("Premium", "Standard"),
                                 seed = 42))
  expect_equal(sum(r$data$Preference == "Premium"), 12)
  expect_equal(sum(r$data$Preference == "Standard"), 3)
  expect_equal(r$exact_test$p.value,
               stats::binom.test(12, 15, 0.5)$p.value)
})

test_that("sign_gen validates inputs", {
  expect_error(suppressMessages(sign_gen(n = 10, k = 12,
                                         target_median = 5)), "'k'")
  expect_error(suppressMessages(sign_gen(n = 10, k = 5)),
               "target_median")
  expect_error(suppressMessages(sign_gen(n = 10, k = 5,
                                         mode = "preference",
                                         category_labels = c("A", "A"))),
               "distinct")
})

# ---- spearman_gen ----

test_that("spearman_gen targets rho to 2 decimals with exact medians", {
  suppressMessages(r <- spearman_gen(
    n = 25, rho = 0.46, medians = c(75, 60), spreads = c(12, 30),
    scale_min = c(1, 0), scale_max = c(100, 500), seed = 42))
  expect_equal(round(r$rho_achieved, 2), 0.46)
  expect_equal(r$variable_stats$Median, c(75, 60))
  expect_equal(r$r_squared, r$rho_achieved^2)
  ref <- suppressWarnings(stats::cor.test(r$data$X, r$data$Y,
                                          method = "spearman"))
  expect_equal(r$test_results$p.value, ref$p.value)
})

test_that("spearman_gen monotonic relationship is curved, negative rho works", {
  suppressMessages(r <- spearman_gen(
    n = 40, rho = -0.5, medians = c(50, 50), spreads = c(10, 10),
    seed = 7))
  expect_equal(round(r$rho_achieved, 2), -0.5)
  suppressMessages(lin <- spearman_gen(
    n = 40, rho = 0.8, medians = c(0, 0), spreads = c(10, 10),
    relationship = "linear", decimals = 1, seed = 7))
  expect_equal(round(lin$rho_achieved, 2), 0.8)
})

# ---- family-wide behaviors ----

test_that("nonparametric generators are reproducible with a seed", {
  suppressMessages(a <- mannwhitney_gen(n1 = 12, median1 = 66, n2 = 14,
                                        median2 = 85, spread = 25,
                                        seed = 9))
  suppressMessages(b <- mannwhitney_gen(n1 = 12, median1 = 66, n2 = 14,
                                        median2 = 85, spread = 25,
                                        seed = 9))
  expect_identical(a$data, b$data)
})

test_that("run_analysis = FALSE skips tests across the family", {
  suppressMessages(r1 <- mannwhitney_gen(n1 = 10, median1 = 5, n2 = 10,
                                         median2 = 7, spread = 3,
                                         run_analysis = FALSE, seed = 1))
  expect_null(r1$test_results)
  suppressMessages(r2 <- sign_gen(n = 10, k = 3, target_median = 5,
                                  run_analysis = FALSE, seed = 1))
  expect_null(r2$exact_test)
})

test_that("export_csv works for the family", {
  dest <- file.path(tempdir(), "np_out.csv")
  if (file.exists(dest)) file.remove(dest)
  suppressMessages(kruskal_gen(k = 3, n = 8, medians = c(4, 5, 7),
                               spread = 2, seed = 1, export_csv = dest))
  expect_true(file.exists(dest))
  file.remove(dest)
})

test_that("nonparametric generators compose with batch_gen", {
  suppressMessages(b <- batch_gen(mannwhitney_gen, n_datasets = 2,
                                  n1 = 10, median1 = 50, n2 = 10,
                                  median2 = 60, spread = 10, seeds = 5))
  expect_equal(nrow(b$answer_key), 2)
  expect_true(any(grepl("test_results_p_value", names(b$answer_key))))
})
