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

# Insert a suffix (e.g., "wide") before the .csv extension of a path, for
# generators that write both wide- and long-format files from one
# user-supplied 'export_csv' path.
dgen_suffix_path <- function(path, suffix) {
  sub("(\\.[Cc][Ss][Vv])?$", paste0("_", suffix, "\\1"), path)
}

# Export a wide/long pair according to 'output_format'. When the user gives
# a file path and both formats are written, _wide/_long is inserted before
# the extension so the two files do not collide.
dgen_export_pair <- function(data_wide, data_long, output_format, export_csv,
                             default_wide, default_long) {
  if (isFALSE(export_csv)) return(invisible(NULL))
  both <- output_format == "both"
  if (output_format %in% c("wide", "both")) {
    dest <- if (is.character(export_csv) && both) {
      dgen_suffix_path(export_csv, "wide")
    } else {
      export_csv
    }
    dgen_export(data_wide, dest, default_wide, "Wide data")
  }
  if (output_format %in% c("long", "both")) {
    dest <- if (is.character(export_csv) && both) {
      dgen_suffix_path(export_csv, "long")
    } else {
      export_csv
    }
    dgen_export(data_long, dest, default_long, "Long data")
  }
  invisible(NULL)
}
