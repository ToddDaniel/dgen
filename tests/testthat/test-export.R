# Tests for the export_csv argument (FALSE / TRUE / file path)

test_that("a path with output_format = 'both' writes _wide and _long files", {
  dest <- file.path(tempdir(), "paired_out.csv")
  wide <- file.path(tempdir(), "paired_out_wide.csv")
  long <- file.path(tempdir(), "paired_out_long.csv")
  for (f in c(dest, wide, long)) if (file.exists(f)) file.remove(f)
  suppressMessages(t_paired_gen(
    n = 20, measure1_mean = 50, measure1_sd = 10,
    measure2_mean = 55, measure2_sd = 10,
    output_format = "both", seed = 1, export_csv = dest))
  expect_false(file.exists(dest))
  expect_true(file.exists(wide))
  expect_true(file.exists(long))
  file.remove(wide, long)
})

test_that("a path with a single output_format is used verbatim", {
  dest <- file.path(tempdir(), "anova_long_only.csv")
  if (file.exists(dest)) file.remove(dest)
  suppressMessages(anova_gen(
    k = 3, n = 10, means = c(10, 12, 14), sds = c(2, 2, 2),
    output_format = "long", seed = 1, export_csv = dest,
    run_analysis = FALSE))
  expect_true(file.exists(dest))
  file.remove(dest)
})

test_that("dgen_suffix_path inserts before the extension, any case", {
  expect_equal(dgen_suffix_path("out.csv", "wide"), "out_wide.csv")
  expect_equal(dgen_suffix_path("out.CSV", "long"), "out_long.CSV")
  expect_equal(dgen_suffix_path("out", "wide"), "out_wide")
  expect_equal(dgen_suffix_path(file.path("a", "b.csv"), "long"),
               file.path("a", "b_long.csv"))
})
