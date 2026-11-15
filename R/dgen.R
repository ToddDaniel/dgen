# dgen(): the basic generator. One variable (or several independent
# variables) with an exact mean and SD.
# Method: each variable is built separately by the exact engine.

#' Generate data with an exact mean and standard deviation
#'
#' Creates a variable whose mean and standard deviation are exactly the
#' values you ask for, in the rounded data your students will see.
#'
#' @details
#' **What "exact" means.** The mean and SD of the returned data, rounded to
#' `decimals + 1` places, equal `m` and `sd`. With `decimals = 1`, a target
#' mean of 75.4 gives data whose mean is reported as 75.40. A student who
#' computes the mean and SD from the data gets your numbers.
#'
#' **How it works.** Random bell-shaped values are standardized (so their
#' mean is exactly 0 and their SD exactly 1), rescaled to your mean and SD,
#' and rounded. Rounding shifts the statistics slightly, so a few values are
#' moved by one unit to make the mean exact, and the whole draw is repeated
#' until the SD of the rounded data matches as well.
#'
#' **Limits.** The data are bell-shaped, with no minimum or maximum, so
#' values can be negative when the mean is small compared with the SD. Some
#' requests are impossible: five whole numbers cannot have a mean of 75.5.
#' `dgen()` then stops and names the nearest values that are possible.
#' Rarely, a request that is too demanding for its sample size and rounding
#' cannot be met; `dgen()` then stops after a fixed number of tries, which
#' can take several seconds.
#'
#' **Several variables.** Give several means and SDs to get several columns,
#' each exact. The columns are generated independently of one another; use
#' [cor_gen()] when you want a particular correlation between them.
#'
#' @param n Sample size: the number of rows.
#' @param m Target mean. Give several means to get several variables.
#' @param sd Target standard deviation (the sample SD, with `n - 1` in the
#'   denominator), one for each mean.
#' @param decimals Number of decimal places in the data. Use `0` for whole
#'   numbers.
#' @param var_name Name for the variable, in quotes; one name per variable
#'   if there are several. The default is `"x"` (or `"x1"`, `"x2"`, ...).
#' @param seed A whole number that makes the data reproducible: the same
#'   seed gives the same data. Leave it out for different data each time.
#' @param export_csv `FALSE` (the default) saves nothing. `TRUE` saves the
#'   data as a CSV file named after the function, in the working directory.
#'   A file name in quotes, such as `"exam.csv"`, saves to that file. An
#'   existing file is never replaced; a number is added to the new name.
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   targets beside what the data actually have, and the smallest and
#'   largest value. `FALSE` prints nothing.
#'
#' @return A data frame with `n` rows and one column per variable, returned
#'   invisibly (so the data do not fill the console). The summary is stored
#'   inside it; see it again with [dgen_results()].
#'
#' @examples
#' scores <- dgen(n = 30, m = 75.4, sd = 8.2, decimals = 1,
#'                var_name = "score", seed = 1)
#' mean(scores$score)
#' sd(scores$score)
#'
#' # Whole numbers, and no printed summary
#' ages <- dgen(n = 50, m = 34, sd = 6, decimals = 0, var_name = "age",
#'              seed = 2, show_results = FALSE)
#' head(ages)
#'
#' # Two independent variables at once
#' both <- dgen(n = 40, m = c(100, 50), sd = c(15, 10), decimals = 0,
#'              var_name = c("iq", "t_score"), seed = 3)
#'
#' @export
dgen <- function(n, m, sd, decimals = 2, var_name = NULL, seed = NULL,
                 export_csv = FALSE, show_results = TRUE) {

  # 1. Check inputs
  if (missing(n) || missing(m) || missing(sd)) {
    stop("dgen needs n, m and sd, for example dgen(n = 30, m = 75, sd = 8).",
         call. = FALSE)
  }
  if (length(n) != 1) {
    stop("n (the sample size) must be a single whole number, such as 30.",
         call. = FALSE)
  }
  n_variables <- length(m)
  default     <- if (n_variables == 1) "x" else paste0("x", seq_len(n_variables))
  names       <- variable_names(var_name, n_variables, default)
  use_seed(seed)

  # 2. Build exact data, one variable at a time
  columns <- exact_groups(n, m, sd, decimals, part = "Variable")
  data    <- data.frame(stats::setNames(columns, names), check.names = FALSE)

  # 3. Assemble the summary and hand everything back
  results <- list(
    title = if (n_variables == 1) "One variable with an exact mean and SD"
            else paste(n_variables, "independent variables, each with an",
                       "exact mean and SD"),
    table = compare_table(names, columns, m, sd, decimals),
    lines = character(),
    note  = data_note(data, decimals)
  )
  deliver(data, results, show_results, export_csv, "dgen")
}
