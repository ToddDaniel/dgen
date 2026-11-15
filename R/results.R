# The results summary that travels with every dgen data set.
# Each generator returns a plain data frame. Its targets, achieved values and
# test results are stored inside the data frame, printed once when the data
# are created, and shown again by dgen_results().

# ---- 1. The last step of every generator -----------------------------------

# Stores the results inside the data, saves a file if asked, prints the
# summary if asked, and hands the data back invisibly so that the data never
# flood the console. `results` is a list with a title, an optional table,
# lines of text and an optional note.
deliver <- function(data, results, show_results, export_csv, function_name) {
  if (!isTRUE(show_results) && !isFALSE(show_results)) {
    stop("show_results must be TRUE or FALSE.", call. = FALSE)
  }
  class(results) <- "dgen_results"
  attr(data, "dgen_results") <- results

  save_csv(data, export_csv, function_name)
  if (show_results) print(results, hint = TRUE)
  invisible(data)
}

# ---- 2. Showing the summary -------------------------------------------------

#' Show the results for a dgen data set
#'
#' Every dgen function stores a short summary inside the data it returns: the
#' targets you asked for, the values the data actually have, and the result
#' of the matching test. `dgen_results()` shows that summary again.
#'
#' @details
#' The summary is attached when the data are created. It stays with the data
#' frame while you use it in R, but it is not saved in a CSV file, and it can
#' be lost if you change the data (for example by removing rows).
#'
#' @param data A data frame created by a dgen function.
#'
#' @return The stored summary (an object of class `dgen_results`), which
#'   prints itself. It is a list with a title, a table of targets and
#'   achieved values, lines of text, and a note.
#'
#' @export
dgen_results <- function(data) {
  results <- attr(data, "dgen_results")
  if (is.null(results)) {
    stop("No dgen results are stored in this data. Results are attached ",
         "when a dgen function creates the data, and are lost if the data ",
         "are changed or read back in from a file.", call. = FALSE)
  }
  results
}

#' @export
print.dgen_results <- function(x, hint = FALSE, ...) {
  cat("\n", x$title, "\n\n", sep = "")
  if (!is.null(x$table)) {
    print(x$table)
    cat("\n")
  }
  if (length(x$lines) > 0) cat(x$lines, sep = "\n")
  if (length(x$note) > 0) cat("\n", x$note, "\n", sep = "")
  if (hint) cat("See this again with dgen_results(your_data).\n")
  invisible(x)
}

# Targets beside the values the data actually have, shown at the precision
# of the promise (decimals + 1). `columns` is a list with one vector of
# values per row of the table.
compare_table <- function(labels, columns, target_mean, target_sd, decimals) {
  shown <- function(x) show_number(x, decimals + 1)
  data.frame(
    "n"             = lengths(columns),
    "Mean (target)" = shown(target_mean),
    "Mean (data)"   = shown(vapply(columns, mean, numeric(1))),
    "SD (target)"   = shown(target_sd),
    "SD (data)"     = shown(vapply(columns, stats::sd, numeric(1))),
    row.names = labels, check.names = FALSE
  )
}

# ---- 3. Saving a file --------------------------------------------------------

# export_csv = FALSE writes nothing; TRUE writes "<function name>.csv" in the
# working directory; a file name writes that file. An existing file is never
# replaced: the new one gets _2, _3, ... added to its name.
save_csv <- function(data, export_csv, function_name) {
  if (isFALSE(export_csv)) return(invisible(NULL))

  is_file_name <- is.character(export_csv) && length(export_csv) == 1 &&
    !is.na(export_csv) && nzchar(export_csv)
  if (!isTRUE(export_csv) && !is_file_name) {
    stop("export_csv must be FALSE, TRUE, or a file name in quotes such as ",
         "\"exam.csv\".", call. = FALSE)
  }
  file <- if (isTRUE(export_csv)) function_name else export_csv
  if (!grepl("\\.csv$", file, ignore.case = TRUE)) file <- paste0(file, ".csv")
  if (!dir.exists(dirname(file))) {
    stop("The folder \"", dirname(file), "\" does not exist, so the file ",
         "could not be saved.", call. = FALSE)
  }

  file <- unused_file_name(file)
  utils::write.csv(data, file, row.names = FALSE)
  message("Saved: ", normalizePath(file))
  invisible(file)
}

unused_file_name <- function(file) {
  stem      <- sub("\\.csv$", "", file, ignore.case = TRUE)
  candidate <- file
  copy      <- 1
  while (file.exists(candidate)) {
    copy      <- copy + 1
    candidate <- paste0(stem, "_", copy, ".csv")
  }
  candidate
}
