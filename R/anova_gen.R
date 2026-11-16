# anova_gen(): data for a one-way analysis of variance.
# Method: one exact variable per group from the engine, then the ANOVA
# table and effect sizes calculated from the data.

#' Generate data for a one-way ANOVA
#'
#' Creates two or more independent groups, each with exactly the mean and
#' standard deviation you choose, and runs the one-way analysis of variance.
#'
#' @details
#' **What is exact.** The mean and SD of each group, rounded to
#' `decimals + 1` places, equal `m` and `sd`. The sums of squares, F, the
#' p-value and the effect sizes are calculated from the data and shown in
#' the summary; they are not targets.
#'
#' **Effect sizes.** Eta squared is the between-groups sum of squares
#' divided by the total sum of squares: the share of the variation in the
#' data that lies between the group means. Omega squared is a less
#' optimistic estimate of the same share in the population.
#'
#' **Limits.** The data are bell-shaped, with no minimum or maximum. When a
#' request cannot be met, the function stops and says why; in rare cases
#' that can take several seconds.
#'
#' With two groups, F equals the square of the equal-variances t from
#' [t_indep_gen()].
#'
#' @inheritParams t_indep_gen
#' @param n Sample size of each group: one number for equal groups, or one
#'   number per group.
#' @param m Target group means, one per group, such as `c(70, 75, 82)`.
#' @param sd Target standard deviations, one per group.
#' @param labels Names of the groups, in quotes. The default is
#'   `"Group 1"`, `"Group 2"`, ...
#' @param show_results `TRUE` (the default) prints a short summary: the
#'   targets beside what the data actually have, then the ANOVA table and
#'   effect sizes. `FALSE` prints nothing.
#'
#' @return A data frame, returned invisibly: in long layout, one row per
#'   observation and two columns (`group` and the measured variable); in
#'   wide layout, one column per group. The summary is stored inside it;
#'   see it again with [dgen_results()].
#'
#' @examples
#' sales <- anova_gen(n = 12, m = c(70, 75, 82), sd = c(8, 9, 8),
#'                    labels = c("North", "South", "West"),
#'                    var_name = "sales", decimals = 0, seed = 1)
#' summary(aov(sales ~ group, data = sales))
#'
#' # One column per region, for the Excel Data Analysis ToolPak
#' wide <- anova_gen(n = 12, m = c(70, 75, 82), sd = c(8, 9, 8),
#'                   labels = c("North", "South", "West"), layout = "wide",
#'                   decimals = 0, seed = 1, show_results = FALSE)
#' head(wide)
#'
#' @export
anova_gen <- function(n, m, sd, labels = NULL, var_name = "x",
                      layout = "long", decimals = 2, seed = NULL,
                      export_csv = FALSE, show_results = TRUE) {

  # 1. Check inputs
  if (missing(n) || missing(m) || missing(sd)) {
    stop("anova_gen needs n, m and sd, for example ",
         "anova_gen(n = 12, m = c(70, 75, 82), sd = c(8, 9, 8)).",
         call. = FALSE)
  }
  n_groups <- length(m)
  if (n_groups < 2) {
    stop("anova_gen compares two or more groups, so it needs a mean and an ",
         "SD for each one, such as m = c(70, 75, 82) and sd = c(8, 9, 8).",
         call. = FALSE)
  }
  labels <- variable_names(labels, n_groups, paste("Group", seq_len(n_groups)),
                           "labels", "groups")
  name   <- variable_names(var_name, 1, "x")
  use_seed(seed)

  # 2. Build exact data, one group at a time
  groups <- exact_groups(n, m, sd, decimals)
  data   <- arrange_groups(groups, labels, name, layout)

  # 3. Run the ANOVA, assemble the summary and hand everything back
  anova <- anova_table(groups)
  results <- list(
    title = paste0("One-way ANOVA: ", name, " by group"),
    table = compare_table(labels, groups, m, sd, decimals),
    lines = c(utils::capture.output(print(anova$table)), "",
              paste0("Eta squared = ", show_number(anova$eta_squared, 2),
                     "   Omega squared = ",
                     show_number(anova$omega_squared, 2))),
    note  = data_note(data, decimals)
  )
  deliver(data, results, show_results, export_csv, "anova_gen")
}

# The one-way ANOVA table and its effect sizes, from a list of groups.
anova_table <- function(groups) {
  values <- unlist(groups)
  group  <- factor(rep(seq_along(groups), lengths(groups)))
  fit    <- stats::anova(stats::lm(values ~ group))

  ss      <- fit[["Sum Sq"]]
  df      <- fit[["Df"]]
  ms      <- fit[["Mean Sq"]]
  table <- data.frame(
    "SS" = show_number(c(ss, sum(ss)), 2),
    "df" = c(df, sum(df)),
    "MS" = c(show_number(ms, 2), ""),
    "F"  = c(show_number(fit[["F value"]][1], 2), "", ""),
    "p"  = c(show_p(fit[["Pr(>F)"]][1]), "", ""),
    row.names = c("Between groups", "Within groups", "Total")
  )
  list(table         = table,
       eta_squared   = ss[1] / sum(ss),
       # omega squared removes the part of SS-between expected by chance
       omega_squared = (ss[1] - df[1] * ms[2]) / (sum(ss) + ms[2]))
}
