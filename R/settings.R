# Settings that mean the same thing in every dgen function: the seed, the
# variable names, and the one-line description of the data under a summary.

# seed = NULL leaves R's random numbers alone; a number makes the data
# reproducible.
use_seed <- function(seed) {
  if (is.null(seed)) return(invisible(NULL))
  if (!is_whole(seed) || length(seed) != 1) {
    stop("seed must be a single whole number, such as seed = 123.",
         call. = FALSE)
  }
  set.seed(seed)
}

variable_names <- function(var_name, n_variables, default) {
  if (is.null(var_name)) return(default)
  usable <- is.character(var_name) && length(var_name) == n_variables &&
    !anyNA(var_name) && all(nzchar(var_name)) && !anyDuplicated(var_name)
  if (!usable) {
    stop("var_name must give ",
         if (n_variables == 1) "one name in quotes, such as \"score\"."
         else paste0("a different name for each of the ", n_variables,
                     " variables, such as c(\"hours\", \"exam\")."),
         call. = FALSE)
  }
  var_name
}

data_note <- function(data, decimals) {
  paste0(nrow(data), " rows, ", ncol(data),
         if (ncol(data) == 1) " column (" else " columns (",
         paste(names(data), collapse = ", "), "). ",
         if (decimals == 0) "Whole numbers."
         else paste0("Rounded to ", decimals, " decimal place",
                     if (decimals > 1) "s", "."))
}

# p-values the way textbooks print them: .031, or < .001
show_p <- function(p) {
  ifelse(p < 0.001, "< .001", sub("^0", "", show_number(p, 3)))
}
