# Internal CSV-export helpers shared by all generators. Not exported.

# Write generated data to CSV according to the 'export_csv' argument:
# FALSE writes nothing, TRUE writes 'default_filename' in the current
# working directory, and a single character string is used as the
# destination path. 'label' prefixes the confirmation message.
dgen_export <- function(data, export_csv, default_filename, label = "Data") {
  if (isFALSE(export_csv)) return(invisible(NULL))
  if (isTRUE(export_csv)) {
    fn <- default_filename
  } else if (is.character(export_csv) && length(export_csv) == 1 &&
             nzchar(export_csv)) {
    fn <- export_csv
  } else {
    stop("'export_csv' must be TRUE, FALSE, or a single non-empty file path")
  }
  utils::write.csv(data, file = fn, row.names = FALSE)
  message(label, " saved to: ", normalizePath(fn))
  invisible(fn)
}
