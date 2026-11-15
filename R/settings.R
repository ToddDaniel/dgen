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

# `setting` is the argument being checked ("var_name" or "labels") and
# `things` is what it names ("variables" or "groups").
variable_names <- function(var_name, n_variables, default,
                           setting = "var_name", things = "variables") {
  if (is.null(var_name)) return(default)
  usable <- is.character(var_name) && length(var_name) == n_variables &&
    !anyNA(var_name) && all(nzchar(var_name)) && !anyDuplicated(var_name)
  if (!usable) {
    stop(setting, " must give ",
         if (n_variables == 1) "one name in quotes, such as \"score\"."
         else paste0("a different name for each of the ", n_variables, " ",
                     things, ", such as c(\"hours\", \"exam\")."),
         call. = FALSE)
  }
  var_name
}

# Stops unless x is exactly `how_many` numbers.
need_numbers <- function(x, how_many, setting, example) {
  if (!is_number(x) || length(x) != how_many) {
    stop(setting, " must be ",
         if (how_many == 1) "a single number" else paste(how_many, "numbers"),
         ", such as ", setting, " = ", example, ".", call. = FALSE)
  }
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

# ---- Effect sizes and test results ------------------------------------------

# Cohen's d is optional; when given it is one number with 2 decimal places.
check_d <- function(d) {
  if (is.null(d)) return(invisible(NULL))
  if (!is_number(d) || length(d) != 1) {
    stop("d must be a single number, such as d = 0.5.", call. = FALSE)
  }
  if (abs(d * 100 - round(d * 100)) > 1e-6) {
    stop("Cohen's d is matched to 2 decimal places, but you asked for ",
         format(d), ". Use ", show_number(floor(d * 100) / 100, 2), " or ",
         show_number(ceiling(d * 100) / 100, 2), ".", call. = FALSE)
  }
}

# The mean that puts the data d spreads above `base`, moved to the nearest
# mean that n rounded values can actually have.
mean_from_d <- function(base, d, spread, n, decimals) {
  steps_per_one <- n * 10^decimals
  round((base + d * spread) * steps_per_one) / steps_per_one
}

# TRUE when a statistic reports as its target at 2 decimal places (no ties)
reports_as <- function(achieved, target) {
  abs(achieved - target) < 0.005 - tie_margin
}

d_line <- function(achieved, target = NULL, definition = NULL) {
  paste0("Cohen's d",
         if (is.null(target)) paste(" =", show_number(achieved, 2))
         else paste0(": target ", show_number(target, 2), ", data ",
                     show_number(achieved, 2)),
         if (!is.null(definition)) paste0("   (", definition, ")"))
}

# "t(48) = 2.23, p = .030", from the result of t.test()
t_line <- function(test, label = NULL) {
  df <- unname(test$parameter)
  paste0("t(", if (abs(df - round(df)) < 1e-8) round(df) else show_number(df, 1),
         ") = ", show_number(unname(test$statistic), 2), ", p ",
         if (test$p.value < 0.001) "< .001" else paste("=", show_p(test$p.value)),
         if (!is.null(label)) paste0("   (", label, ")"))
}

ci_line <- function(test, of_what, decimals) {
  paste0("95% CI for the ", of_what, ": ",
         show_number(test$conf.int[1], decimals + 1), " to ",
         show_number(test$conf.int[2], decimals + 1))
}
