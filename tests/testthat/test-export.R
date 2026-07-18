# Tests for the export_csv argument (FALSE / TRUE / file path)

test_that("export_csv = FALSE writes nothing", {
  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)
  before <- list.files(tempdir(), pattern = "\\.csv$")
  suppressMessages(stat_gen(n = 20, m = 50, sd = 10, seed = 1))
  after <- list.files(tempdir(), pattern = "\\.csv$")
  expect_identical(before, after)
})

test_that("export_csv = TRUE writes the auto-named file to the working dir", {
  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)
  expected <- file.path(tempdir(), "AutoName_dgen.csv")
  if (file.exists(expected)) file.remove(expected)
  suppressMessages(stat_gen(n = 20, m = 50, sd = 10, seed = 1,
                            var_name = "AutoName", export_csv = TRUE))
  expect_true(file.exists(expected))
  file.remove(expected)
})

test_that("export_csv accepts a full file path", {
  dest <- file.path(tempdir(), "my_custom_name.csv")
  if (file.exists(dest)) file.remove(dest)
  suppressMessages(res <- stat_gen(n = 20, m = 50, sd = 10, seed = 1,
                                   export_csv = dest))
  expect_true(file.exists(dest))
  reread <- utils::read.csv(dest)
  expect_equal(nrow(reread), 20)
  file.remove(dest)
})

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

test_that("export_csv works for the newest generator (mcnemar_gen)", {
  dest <- file.path(tempdir(), "mcnemar_out.csv")
  if (file.exists(dest)) file.remove(dest)
  suppressMessages(mcnemar_gen(n = 60, p1 = 0.50, p2 = 0.65, seed = 1,
                               export_csv = dest))
  expect_true(file.exists(dest))
  file.remove(dest)
})

test_that("invalid export_csv values are rejected", {
  expect_error(
    suppressMessages(stat_gen(n = 20, m = 50, sd = 10, seed = 1,
                              export_csv = 123)),
    "export_csv")
  expect_error(
    suppressMessages(stat_gen(n = 20, m = 50, sd = 10, seed = 1,
                              export_csv = c("a.csv", "b.csv"))),
    "export_csv")
  expect_error(
    suppressMessages(stat_gen(n = 20, m = 50, sd = 10, seed = 1,
                              export_csv = "")),
    "export_csv")
})

test_that("dgen_suffix_path inserts before the extension, any case", {
  expect_equal(dgen_suffix_path("out.csv", "wide"), "out_wide.csv")
  expect_equal(dgen_suffix_path("out.CSV", "long"), "out_long.CSV")
  expect_equal(dgen_suffix_path("out", "wide"), "out_wide")
  expect_equal(dgen_suffix_path(file.path("a", "b.csv"), "long"),
               file.path("a", "b_long.csv"))
})
