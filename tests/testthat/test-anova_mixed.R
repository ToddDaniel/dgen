# Tests for anova_mixed_gen()

quick_mixed <- function(seed = 42, run_analysis = TRUE) {
  suppressMessages(anova_mixed_gen(
    n_per_group    = 5,
    between_levels = 2,
    between_labels = c("Treatment", "Control"),
    within_levels  = 2,
    within_labels  = c("Pre", "Post"),
    cell_means     = matrix(c(10, 16,
                              10, 11), nrow = 2, byrow = TRUE),
    within_cell_sd = 2,
    seed           = seed,
    run_analysis   = run_analysis
  ))
}

# Generated once; reused by the read-only tests below
Q <- quick_mixed()

test_that("anova_mixed_gen hits exact cell means and SDs", {
  r <- Q
  expect_equal(round(r$cell_stats$Mean, 3), c(10, 16, 10, 11))
  expect_equal(round(r$cell_stats$SD, 3), rep(2, 4))
})

test_that("data structures are correct", {
  r <- Q
  expect_equal(nrow(r$data_wide), 10)          # 2 groups x 5 subjects
  expect_equal(nrow(r$data_long), 20)          # x 2 measurements
  expect_named(r$data_wide, c("ID", "Group", "Pre", "Post"))
  expect_named(r$data_long, c("ID", "Group", "Time", "Score"))
  expect_equal(length(unique(r$data_long$ID)), 10)
  expect_equal(levels(r$data_long$Group), c("Treatment", "Control"))
  expect_equal(levels(r$data_long$Time), c("Pre", "Post"))
  # default output_format = "long" drives $data
  expect_identical(r$data, r$data_long)
})

test_that("degrees of freedom follow the split-plot partition", {
  r <- Q
  at <- r$anova_table
  # a=2, k=2, n=5: df = 1, 8, 1, 1, 8, 19
  expect_equal(at$df, c(1, 8, 1, 1, 8, 19))
  expect_equal(at$Source[c(1, 3, 4)],
               c("Group", "Time", "Group x Time"))
})

test_that("sums of squares are additive", {
  r <- Q
  at <- r$anova_table
  expect_equal(sum(at$SS[1:5]), at$SS[6], tolerance = 1e-8)
})

test_that("F statistics match R's own aov() Error() decomposition", {
  r <- Q
  d <- r$data_long
  d$ID <- factor(d$ID)
  fit <- stats::aov(Score ~ Group * Time + Error(ID / Time), data = d)
  s <- summary(fit)
  between_tab <- s[["Error: ID"]][[1]]
  within_tab  <- s[["Error: ID:Time"]][[1]]
  F_group_aov <- between_tab["Group", "F value"]
  F_time_aov  <- within_tab["Time", "F value"]
  F_int_aov   <- within_tab["Group:Time", "F value"]
  at <- r$anova_table
  expect_equal(at$F[1], unname(F_group_aov), tolerance = 1e-6)
  expect_equal(at$F[3], unname(F_time_aov),  tolerance = 1e-6)
  expect_equal(at$F[4], unname(F_int_aov),   tolerance = 1e-6)
})

test_that("the built-in interaction example is significant", {
  r <- Q
  p_int <- r$anova_table$p[4]
  expect_lt(p_int, 0.05)
  # interaction significant -> no marginal post-hocs
  expect_null(r$posthoc_between)
  expect_null(r$posthoc_within)
})

test_that("within-subject correlation is achieved", {
  r <- Q
  off_diag <- r$correlation_matrix[lower.tri(r$correlation_matrix)]
  expect_equal(round(mean(off_diag), 2), 0.70)
})

test_that("effect sizes use each effect's own error term", {
  r <- Q
  at <- r$anova_table
  es <- r$effect_sizes
  expect_equal(es$between$eta_squared_partial,
               at$SS[1] / (at$SS[1] + at$SS[2]))
  expect_equal(es$interaction$eta_squared_partial,
               at$SS[4] / (at$SS[4] + at$SS[5]))
  expect_lt(es$between$eta_squared, 1)
})

test_that("sphericity note and Levene assumptions are reported", {
  r <- Q
  expect_match(r$assumptions$sphericity, "Compound symmetry")
  expect_true(is.numeric(r$assumptions$levene_p))
  expect_gte(r$assumptions$variance_ratio, 1)
})

test_that("Tukey post-hoc appears for 3+ groups with null interaction", {
  suppressMessages(r <- anova_mixed_gen(
    n_per_group    = 8,
    between_levels = 3,
    between_labels = c("Low", "Mid", "High"),
    within_levels  = 2,
    within_labels  = c("T1", "T2"),
    cell_means     = matrix(c(10, 12,
                              20, 22,
                              30, 32), nrow = 3, byrow = TRUE),
    within_cell_sd = 3,
    seed           = 42
  ))
  # parallel profiles: interaction null, huge group effect
  expect_gte(r$anova_table$p[4], 0.05)
  expect_lt(r$anova_table$p[1], 0.05)
  expect_s3_class(r$posthoc_between, "data.frame")
  expect_equal(nrow(r$posthoc_between), 3)     # 3 choose 2
  expect_true(all(r$posthoc_between$Significant))
})

test_that("anova_mixed_gen is reproducible and validates inputs", {
  b <- quick_mixed(seed = 42)
  expect_identical(Q$data_long, b$data_long)

  cm <- matrix(c(10, 16, 10, 11), nrow = 2, byrow = TRUE)
  expect_error(suppressMessages(anova_mixed_gen(
    n_per_group = 1, between_levels = 2,
    between_labels = c("A", "B"), within_levels = 2,
    within_labels = c("x", "y"), cell_means = cm, within_cell_sd = 2)),
    "n_per_group")
  expect_error(suppressMessages(anova_mixed_gen(
    n_per_group = 5, between_levels = 2,
    between_labels = c("A", "B", "C"), within_levels = 2,
    within_labels = c("x", "y"), cell_means = cm, within_cell_sd = 2)),
    "between_labels")
  expect_error(suppressMessages(anova_mixed_gen(
    n_per_group = 5, between_levels = 2,
    between_labels = c("A", "B"), within_levels = 2,
    within_labels = c("x", "y"), cell_means = matrix(1:6, 2),
    within_cell_sd = 2)),
    "cell_means")
  expect_error(suppressMessages(anova_mixed_gen(
    n_per_group = 5, between_levels = 2,
    between_labels = c("A", "B"), within_levels = 2,
    within_labels = c("x", "y"), cell_means = cm, within_cell_sd = 2,
    within_subject_correlation = 1.2)),
    "correlation")
  expect_error(suppressMessages(anova_mixed_gen(
    n_per_group = 5, between_levels = 2,
    between_labels = c("A", "B"), within_levels = 2,
    within_labels = c("x", "y"), cell_means = cm, within_cell_sd = 2,
    output_format = "tall")),
    "output_format")
})

test_that("run_analysis = FALSE skips the ANOVA", {
  r <- quick_mixed(run_analysis = FALSE)
  expect_null(r$anova_table)
  expect_null(r$effect_sizes)
  expect_false(is.null(r$data_long))
})

test_that("export_csv writes wide and long files from one path", {
  dest <- file.path(tempdir(), "mixed_out.csv")
  wide <- file.path(tempdir(), "mixed_out_wide.csv")
  long <- file.path(tempdir(), "mixed_out_long.csv")
  for (f in c(dest, wide, long)) if (file.exists(f)) file.remove(f)
  suppressMessages(anova_mixed_gen(
    n_per_group = 5, between_levels = 2,
    between_labels = c("A", "B"), within_levels = 2,
    within_labels = c("x", "y"),
    cell_means = matrix(c(10, 16, 10, 11), nrow = 2, byrow = TRUE),
    within_cell_sd = 2, seed = 42,
    output_format = "both", export_csv = dest))
  expect_false(file.exists(dest))
  expect_true(file.exists(wide))
  expect_true(file.exists(long))
  file.remove(wide, long)
})
