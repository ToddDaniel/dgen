# Tests for the export_csv argument (FALSE / TRUE / file path)


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
