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

# "North, South and West"
and_list <- function(words) {
  if (length(words) < 2) return(words)
  paste(paste(words[-length(words)], collapse = ", "), "and",
        words[length(words)])
}

# Titles never show a default name such as "x" or "Group 1". A grouped test
# is titled by its variable ("score by group") when one was named and the
# data have a column for it; otherwise by its groups, if those were named.
group_title <- function(test, var_name, labels, labels_used, layout) {
  if (!is.null(var_name) && layout == "long") {
    return(paste0(test, ": ", var_name, " by group"))
  }
  if (!is.null(labels)) return(paste0(test, ": ", and_list(labels_used)))
  paste0(test, ": ", length(labels_used), " groups")
}

# Stops unless x is exactly `how_many` numbers.
need_numbers <- function(x, how_many, setting, example) {
  if (!is_number(x) || length(x) != how_many) {
    stop(setting, " must be ",
         if (how_many == 1) "a single number" else paste(how_many, "numbers"),
         ", such as ", setting, " = ", example, ".", call. = FALSE)
  }
}

# One row per observation with a group column ("long"), or one column per
# group ("wide", the layout Excel's Data Analysis ToolPak needs). In wide
# layout, shorter groups are padded with NA so the columns line up.
arrange_groups <- function(groups, labels, name, layout) {
  if (!is.character(layout) || length(layout) != 1 ||
      !(layout %in% c("long", "wide"))) {
    stop("layout must be \"long\" (one row per observation) or \"wide\" ",
         "(one column per group).", call. = FALSE)
  }
  if (layout == "wide") {
    longest <- max(lengths(groups))
    padded  <- lapply(groups, function(values) values[seq_len(longest)])
    return(data.frame(stats::setNames(padded, labels), check.names = FALSE))
  }
  data <- data.frame(group = factor(rep(labels, lengths(groups)),
                                    levels = labels),
                     values = unlist(groups))
  stats::setNames(data, c("group", name))
}

data_note <- function(data, decimals) {
  paste0(nrow(data), " rows, ", ncol(data),
         if (ncol(data) == 1) " column (" else " columns (",
         paste(names(data), collapse = ", "), "). ",
         if (decimals == 0) "Whole numbers."
         else paste0("Rounded to ", decimals, " decimal place",
                     if (decimals > 1) "s", "."),
         if (anyNA(data)) " Shorter groups end with empty (NA) cells.")
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

# Slopes and standard errors can be large or tiny, so they are shown to 4
# significant digits (160.0, 3.000, 0.6492, 0.08001) rather than a fixed
# number of decimals.
show_estimate <- function(x) {
  vapply(x, function(value) {
    if (value == 0) return("0")
    whole_digits <- floor(log10(abs(value))) + 1
    show_number(value, min(max(4 - whole_digits, 0), 12))
  }, character(1), USE.NAMES = FALSE)
}
