#' Generate Data for a McNemar Test of Paired Proportions
#'
#' Creates paired binary outcome data (e.g., pass/fail measured before and
#' after an intervention on the same participants) with exact marginal
#' proportions at each measurement, then optionally runs McNemar's
#' chi-square test and its exact binomial equivalent. This is the paired
#' counterpart of [twoprop_gen()], just as [t_paired_gen()] is the paired
#' counterpart of [t_indep_gen()].
#'
#' The construction is exact and closed-form: integer cell counts for the
#' 2x2 paired table are computed directly from \code{n}, \code{p1}, and
#' \code{p2} (no iterative refinement), so the achieved proportions equal
#' \code{round(n * p1) / n} and \code{round(n * p2) / n} by construction.
#' McNemar's test depends only on the discordant pairs (participants who
#' changed category between measurements), so \code{n_discordant} controls
#' the number of changers: more discordant pairs with the same marginal
#' proportions means more crossing in both directions. \code{n_discordant}
#' must have the same parity as (be odd or even together with)
#' \code{round(n * p2) - round(n * p1)}, and defaults to roughly a quarter
#' of the pairs, adjusted to satisfy that constraint.
#'
#' @param n Number of pairs (participants measured twice)
#' @param p1 Proportion of successes at measurement 1 (0 < p1 < 1)
#' @param p2 Proportion of successes at measurement 2 (0 < p2 < 1)
#' @param n_discordant Total number of discordant pairs (participants who
#'   changed category). Default \code{NULL} chooses a feasible value near
#'   \code{0.25 * n}
#' @param measure1_name Name for the first measurement column (default =
#'   \code{"Time1"})
#' @param measure2_name Name for the second measurement column (default =
#'   \code{"Time2"})
#' @param category_labels Character vector of length 2 for the outcome
#'   categories, failure first (default = \code{c("Failure", "Success")})
#' @param id_name Name for the participant identifier column (default =
#'   \code{"ID"})
#' @param seed Integer seed for reproducibility of the row order (the cell
#'   counts themselves are deterministic; default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run McNemar's test automatically?
#'   (default = \code{TRUE})
#'
#' @return (Invisibly) a list with:
#'   \item{data}{Data frame with one row per pair: ID, measurement 1,
#'     measurement 2}
#'   \item{contingency_table}{2x2 paired contingency table (measurement 1
#'     by measurement 2)}
#'   \item{cell_counts}{Named counts: both_success, success_to_failure,
#'     failure_to_success, both_failure}
#'   \item{measure1_stats}{n, successes, and proportion at measurement 1}
#'   \item{measure2_stats}{n, successes, and proportion at measurement 2}
#'   \item{difference}{Difference in proportions (measurement 2 -
#'     measurement 1)}
#'   \item{n_discordant}{Total discordant pairs used}
#'   \item{odds_ratio}{Discordant-pair odds ratio: failure-to-success
#'     changes over success-to-failure changes}
#'   \item{cohens_g}{Cohen's g: proportion of discordant pairs moving
#'     toward success, minus 0.5}
#'   \item{mcnemar_test_results}{\code{mcnemar.test()} result with
#'     continuity correction (if \code{run_analysis = TRUE})}
#'   \item{mcnemar_exact_results}{Exact binomial version of the test via
#'     \code{binom.test()} on the discordant pairs (if
#'     \code{run_analysis = TRUE})}
#'   \item{assumptions}{Discordant-pair count check for the chi-square
#'     approximation (if \code{run_analysis = TRUE})}
#'
#' @seealso [twoprop_gen()] for independent groups, [t_paired_gen()] for
#'   the continuous paired design, [h_to_prop()] for choosing proportions
#'   from a target effect size
#'
#' @examples
#' # 100 trainees: 55% passing before instruction, 75% after
#' result <- mcnemar_gen(
#'   n  = 100,
#'   p1 = 0.55,
#'   p2 = 0.75,
#'   measure1_name = "Before",
#'   measure2_name = "After",
#'   category_labels = c("Fail", "Pass"),
#'   seed = 42
#' )
#' result$contingency_table
#' result$mcnemar_test_results
#'
#' @concept McNemar test
#' @concept paired proportions
#'
#' @export
mcnemar_gen <- function(n,
                        p1,
                        p2,
                        n_discordant    = NULL,
                        measure1_name   = "Time1",
                        measure2_name   = "Time2",
                        category_labels = c("Failure", "Success"),
                        id_name         = "ID",
                        seed            = NULL,
                        export_csv      = FALSE,
                        run_analysis    = TRUE) {

  if (!is.numeric(n) || length(n) != 1 || n <= 1 || n != round(n))
    stop("'n' must be a single integer >= 2")
  if (!is.numeric(p1) || length(p1) != 1 || p1 <= 0 || p1 >= 1)
    stop("'p1' must be between 0 and 1 (exclusive)")
  if (!is.numeric(p2) || length(p2) != 1 || p2 <= 0 || p2 >= 1)
    stop("'p2' must be between 0 and 1 (exclusive)")
  if (length(category_labels) != 2)
    stop("'category_labels' must have exactly 2 elements")
  if (measure1_name == measure2_name)
    stop("'measure1_name' and 'measure2_name' must differ")

  if (!is.null(seed)) set.seed(seed)

  n1   <- round(n * p1)   # successes at measurement 1
  n2   <- round(n * p2)   # successes at measurement 2
  diff <- n2 - n1         # = failure_to_success - success_to_failure

  # Feasible discordant totals D satisfy: D >= |diff|, D has the same
  # parity as diff, success_to_failure = (D - diff)/2 <= n1, and
  # failure_to_success = (D + diff)/2 <= n - n1.
  d_min <- abs(diff)
  d_max <- min(2 * n1 + diff, 2 * (n - n1) - diff)
  if (d_max < d_min)
    stop("No feasible discordant count for these inputs; ",
         "check 'n', 'p1', and 'p2'")

  if (is.null(n_discordant)) {
    D <- max(d_min, round(0.25 * n))
    if ((D - diff) %% 2 != 0) D <- D + 1
    if (D > d_max) D <- d_max - ((d_max - diff) %% 2)
  } else {
    if (!is.numeric(n_discordant) || length(n_discordant) != 1 ||
        n_discordant != round(n_discordant))
      stop("'n_discordant' must be a single integer")
    D <- n_discordant
    if ((D - diff) %% 2 != 0)
      stop("'n_discordant' must have the same parity as ",
           "round(n * p2) - round(n * p1) = ", diff,
           " (both odd or both even)")
    if (D < d_min || D > d_max)
      stop("'n_discordant' must be between ", d_min, " and ", d_max,
           " for these inputs")
  }

  success_to_failure <- (D - diff) / 2                 # b
  failure_to_success <- (D + diff) / 2                 # c
  both_success       <- n1 - success_to_failure        # a
  both_failure       <- n - n1 - failure_to_success    # d

  m1 <- c(rep(1, both_success), rep(1, success_to_failure),
          rep(0, failure_to_success), rep(0, both_failure))
  m2 <- c(rep(1, both_success), rep(0, success_to_failure),
          rep(1, failure_to_success), rep(0, both_failure))
  shuffle <- sample(n)

  data_out <- data.frame(
    id = seq_len(n),
    m1 = factor(m1[shuffle], levels = c(0, 1), labels = category_labels),
    m2 = factor(m2[shuffle], levels = c(0, 1), labels = category_labels),
    stringsAsFactors = FALSE
  )
  colnames(data_out) <- c(id_name, measure1_name, measure2_name)

  ctab <- table(data_out[[measure1_name]], data_out[[measure2_name]],
                dnn = c(measure1_name, measure2_name))

  p1_ach   <- n1 / n
  p2_ach   <- n2 / n
  diff_val <- p2_ach - p1_ach
  odds_ratio <- if (success_to_failure > 0) {
    failure_to_success / success_to_failure
  } else {
    Inf
  }
  cohens_g <- if (D > 0) failure_to_success / D - 0.5 else 0

  measure1_stats <- list(measure = measure1_name, n = n,
                         successes = n1, proportion = p1_ach)
  measure2_stats <- list(measure = measure2_name, n = n,
                         successes = n2, proportion = p2_ach)

  mcnemar_test_results  <- NULL
  mcnemar_exact_results <- NULL
  assumptions           <- NULL

  if (run_analysis && D == 0) {
    warning("No discordant pairs; McNemar's test is undefined and was ",
            "not run")
    run_analysis <- FALSE
  }
  if (run_analysis) {
    mcnemar_test_results  <- stats::mcnemar.test(ctab, correct = TRUE)
    mcnemar_exact_results <- stats::binom.test(failure_to_success, D,
                                               p = 0.5)
    assumptions <- list(
      discordant_pairs          = D,
      discordant_adequate       = D >= 25,
      recommendation = if (D >= 25) {
        "Chi-square approximation is adequate"
      } else {
        "Fewer than 25 discordant pairs; prefer the exact binomial test"
      }
    )
  }

  dgen_export(data_out, export_csv,
              paste0(measure1_name, "_", measure2_name, "_mcnemar_dgen.csv"))

  message(sprintf("McNemar paired proportions: %s = %.3f | %s = %.3f",
                  measure1_name, p1_ach, measure2_name, p2_ach))
  message(sprintf(
    "Discordant pairs = %d (%s->%s: %d | %s->%s: %d) | Cohen's g = %.3f",
    D, category_labels[1], category_labels[2], failure_to_success,
    category_labels[2], category_labels[1], success_to_failure, cohens_g))
  if (run_analysis) {
    message(sprintf("mcnemar.test: chi2 = %.3f, p = %s",
                    as.numeric(mcnemar_test_results$statistic),
                    format.pval(mcnemar_test_results$p.value,
                                digits = 3, eps = 0.001)))
    message(sprintf("exact binomial test: p = %s",
                    format.pval(mcnemar_exact_results$p.value,
                                digits = 3, eps = 0.001)))
  }

  result <- list(
    data               = data_out,
    contingency_table  = ctab,
    cell_counts        = list(both_success       = both_success,
                              success_to_failure = success_to_failure,
                              failure_to_success = failure_to_success,
                              both_failure       = both_failure),
    measure1_stats     = measure1_stats,
    measure2_stats     = measure2_stats,
    difference         = diff_val,
    n_discordant       = D,
    odds_ratio         = odds_ratio,
    cohens_g           = cohens_g
  )
  if (run_analysis) {
    result$mcnemar_test_results  <- mcnemar_test_results
    result$mcnemar_exact_results <- mcnemar_exact_results
    result$assumptions           <- assumptions
  }

  invisible(result)
}
