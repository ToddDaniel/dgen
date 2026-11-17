# The results summary: attached to a plain data frame, printed on request.

example_results <- function() {
  groups <- list(c(70, 72, 74), c(75, 78, 81))
  list(title = "Independent-samples t test: score by group",
       table = compare_table(c("Online", "Classroom"), groups,
                             target_mean = c(72, 78), target_sd = c(2, 3),
                             decimals = 0),
       lines = "t(4) = 2.88, p = .045",
       note  = "6 rows, 2 columns (group, score). Whole numbers.")
}
example_data <- function() {
  data.frame(group = rep(c("Online", "Classroom"), each = 3),
             score = c(70, 72, 74, 75, 78, 81))
}

test_that("deliver() returns a plain data frame, invisibly, with results", {
  out <- withVisible(deliver(example_data(), example_results(),
                             show_results = FALSE, export_csv = FALSE,
                             function_name = "t_indep_gen"))
  expect_false(out$visible)
  expect_identical(class(out$value), "data.frame")
  expect_equal(out$value$score, c(70, 72, 74, 75, 78, 81))
  expect_s3_class(dgen_results(out$value), "dgen_results")
})

test_that("show_results prints the summary once, when the data are created", {
  expect_silent(deliver(example_data(), example_results(), FALSE, FALSE,
                        "t_indep_gen"))
  shown <- capture.output(
    data <- deliver(example_data(), example_results(), TRUE, FALSE,
                    "t_indep_gen"))
  expect_true(any(grepl("Independent-samples t test", shown)))
  expect_true(any(grepl("Mean \\(target\\)", shown)))
  expect_true(any(grepl("t(4) = 2.88, p = .045", shown, fixed = TRUE)))
  expect_true(any(grepl("dgen_results(your_data)", shown, fixed = TRUE)))

  again <- capture.output(print(dgen_results(data)))
  expect_true(any(grepl("Independent-samples t test", again)))
  expect_false(any(grepl("See this again", again)))
  expect_error(deliver(example_data(), example_results(), "yes", FALSE,
                       "t_indep_gen"), "show_results must be TRUE or FALSE")
})

test_that("compare_table() shows targets and data at decimals + 1", {
  table <- example_results()$table
  expect_equal(table[["n"]], c(3, 3))
  expect_equal(table[["Mean (target)"]], c("72.0", "78.0"))
  expect_equal(table[["Mean (data)"]], c("72.0", "78.0"))
  expect_equal(table[["SD (data)"]], c("2.0", "3.0"))
  expect_equal(table[["Smallest"]], c("70", "75"))
  expect_equal(table[["Largest"]], c("74", "81"))
})

test_that("dgen_results() explains itself when nothing is stored", {
  expect_error(dgen_results(example_data()), "No dgen results are stored")
})

test_that("export_csv writes nothing, a named file, or a function-named file", {
  folder <- tempfile("dgen-export")
  dir.create(folder)
  old <- setwd(folder)
  on.exit({
    setwd(old)
    unlink(folder, recursive = TRUE)
  })

  save_csv(example_data(), FALSE, "t_indep_gen")
  expect_length(list.files(folder), 0)

  expect_message(save_csv(example_data(), TRUE, "t_indep_gen"), "Saved: ")
  expect_message(save_csv(example_data(), "exam", "t_indep_gen"), "exam.csv")
  expect_setequal(list.files(folder), c("t_indep_gen.csv", "exam.csv"))
  expect_equal(utils::read.csv("exam.csv"), example_data())
})

test_that("an existing file is never replaced", {
  folder <- tempfile("dgen-export")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE))
  file <- file.path(folder, "exam.csv")

  first <- example_data()
  suppressMessages(save_csv(first, file, "dgen"))
  second <- transform(first, score = score + 1)
  suppressMessages(save_csv(second, file, "dgen"))
  suppressMessages(save_csv(second, file, "dgen"))

  expect_setequal(list.files(folder),
                  c("exam.csv", "exam_2.csv", "exam_3.csv"))
  expect_equal(utils::read.csv(file), first)
  expect_equal(utils::read.csv(file.path(folder, "exam_2.csv")), second)
})

test_that("bad export_csv values get plain messages", {
  expect_error(save_csv(example_data(), 3, "dgen"), "export_csv must be")
  expect_error(save_csv(example_data(),
                        file.path(tempdir(), "no-such-folder", "x.csv"),
                        "dgen"),
               "does not exist")
})
