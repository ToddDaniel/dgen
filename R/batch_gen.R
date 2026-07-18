#' Generate a Batch of Unique Datasets with an Instructor Answer Key
#'
#' Calls a dgen generator once per dataset with a different seed each
#' time, producing many unique datasets from one set of target
#' parameters — one per student — plus a compiled answer key giving each
#' dataset's achieved statistics, effect sizes, and test results. Because
#' every student's data are different, answers cannot be copied, and
#' because the key records every seed, any individual dataset can be
#' regenerated later from the same call.
#'
#' The answer key is built by flattening each generator result: scalar
#' components (effect sizes, achieved values, counts), one-row summary
#' data frames, and hypothesis-test objects (reported as
#' \code{*_statistic}, \code{*_df}, and \code{*_p_value}) become key
#' columns. Multi-row data frames, matrices, tables, and the generated
#' data itself are not flattened — the complete per-dataset results
#' remain available in the returned \code{results} list. If a component
#' is present for some datasets but not others (e.g., post-hoc tests that
#' only run when significant), the missing entries are \code{NA}.
#'
#' @param generator A dgen generator function (or its name as a string),
#'   e.g. \code{t_indep_gen} — any function that accepts a \code{seed}
#'   argument and returns a list
#' @param n_datasets Number of unique datasets to generate (one per
#'   student)
#' @param ... Arguments passed to \code{generator}: the shared target
#'   parameters (n, means, SDs, proportions, ...). Do not pass
#'   \code{seed} here — use \code{seeds} — and do not pass
#'   \code{export_csv} for the generator; per-dataset files are written
#'   by \code{batch_gen} itself (see \code{export_csv} below)
#' @param seeds Seed control. A single value expands to consecutive
#'   seeds (\code{42} becomes 42, 43, ...); a vector of length
#'   \code{n_datasets} is used as-is; \code{NULL} (default) draws random
#'   seeds. Whatever is used is recorded in the answer key
#' @param dataset_labels Character vector of length \code{n_datasets}
#'   naming each dataset (e.g., student IDs). Default \code{NULL} uses
#'   \code{"Dataset_01"}, \code{"Dataset_02"}, ...
#' @param quiet Logical; suppress the per-dataset console summaries from
#'   the generator? (default = \code{TRUE}). Warnings (e.g., iterative
#'   refinement hitting its cap) are never suppressed
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write into the current working directory), or a
#'   directory path. When enabled, one CSV per dataset (named by its
#'   label) plus \code{answer_key.csv} are written to the directory,
#'   which is created if needed
#'
#' @return (Invisibly) a list with:
#'   \item{results}{Named list of the full generator results, one per
#'     dataset}
#'   \item{answer_key}{Data frame with one row per dataset: label, seed,
#'     and the flattened statistics described above}
#'   \item{generator}{Name of the generator used}
#'   \item{seeds}{Integer vector of the seeds used}
#'
#' @seealso Any dgen generator, e.g. [t_indep_gen()], [anova_gen()],
#'   [mcnemar_gen()]
#'
#' @examples
#' # Three unique one-sample datasets, same targets, different data
#' batch <- batch_gen(stat_gen, n_datasets = 3,
#'                    n = 15, m = 50, sd = 8, seeds = 42)
#' batch$answer_key
#'
#' # Per-student independent-groups t-test homework with an answer key
#' hw <- batch_gen(t_indep_gen, n_datasets = 4,
#'                 n1 = 12, mean1 = 100, sd1 = 15, label1 = "Control",
#'                 n2 = 12, mean2 = 110, sd2 = 15, label2 = "Treatment",
#'                 seeds = c(11, 22, 33, 44),
#'                 dataset_labels = paste0("Student_", LETTERS[1:4]))
#' hw$answer_key[, c("dataset", "seed", "cohens_d")]
#'
#' @concept batch generation
#' @concept answer key
#'
#' @export
batch_gen <- function(generator,
                      n_datasets,
                      ...,
                      seeds          = NULL,
                      dataset_labels = NULL,
                      quiet          = TRUE,
                      export_csv     = FALSE) {

  if (is.character(generator)) {
    gen_name  <- generator
    generator <- match.fun(generator)
  } else {
    gen_name <- deparse(substitute(generator))
    if (!is.function(generator)) stop("'generator' must be a function")
  }
  if (!"seed" %in% names(formals(generator)))
    stop("'generator' must accept a 'seed' argument; ",
         "every dgen generator does")
  if (!is.numeric(n_datasets) || length(n_datasets) != 1 ||
      n_datasets < 1 || n_datasets != round(n_datasets))
    stop("'n_datasets' must be a single positive integer")

  dots <- list(...)
  if ("seed" %in% names(dots))
    stop("Pass 'seeds' to batch_gen, not 'seed'; ",
         "each dataset gets its own seed")

  if (is.null(seeds)) {
    seeds <- sample.int(1e6, n_datasets)
  } else if (length(seeds) == 1) {
    seeds <- seeds + seq_len(n_datasets) - 1
  } else if (length(seeds) != n_datasets) {
    stop("'seeds' must be NULL, a single starting value, or a vector ",
         "of length n_datasets (", n_datasets, ")")
  }
  if (anyDuplicated(seeds))
    warning("Duplicated seeds: some datasets will be identical")

  if (is.null(dataset_labels)) {
    width <- max(2, nchar(as.character(n_datasets)))
    dataset_labels <- sprintf("Dataset_%0*d", width, seq_len(n_datasets))
  }
  if (length(dataset_labels) != n_datasets)
    stop("'dataset_labels' must have length n_datasets (",
         n_datasets, ")")
  if (anyDuplicated(dataset_labels))
    stop("'dataset_labels' must be unique")

  results <- vector("list", n_datasets)
  names(results) <- dataset_labels
  for (i in seq_len(n_datasets)) {
    call_args <- c(dots, list(seed = seeds[i]))
    results[[i]] <- if (quiet) {
      suppressMessages(do.call(generator, call_args))
    } else {
      do.call(generator, call_args)
    }
    if (!is.list(results[[i]]))
      stop("'generator' must return a list (as all dgen generators do)")
  }

  rows      <- lapply(results, flatten_for_key)
  key_names <- unique(unlist(lapply(rows, names)))
  key_names <- setdiff(key_names, c("dataset", "seed"))
  answer_key <- data.frame(dataset = dataset_labels, seed = seeds,
                           stringsAsFactors = FALSE)
  for (nm in key_names) {
    answer_key[[nm]] <- unlist(lapply(rows, function(r) {
      if (nm %in% names(r) && !is.null(r[[nm]])) r[[nm]] else NA
    }), use.names = FALSE)
  }

  if (!isFALSE(export_csv)) {
    if (isTRUE(export_csv)) {
      dir <- getwd()
    } else if (is.character(export_csv) && length(export_csv) == 1 &&
               nzchar(export_csv)) {
      dir <- export_csv
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    } else {
      stop("'export_csv' must be TRUE, FALSE, or a single directory path")
    }
    for (i in seq_len(n_datasets)) {
      dat <- extract_primary_data(results[[i]])
      if (is.null(dat)) {
        warning("No data frame found in result for '", dataset_labels[i],
                "'; no CSV written for it")
        next
      }
      utils::write.csv(dat, file.path(dir, paste0(dataset_labels[i], ".csv")),
                       row.names = FALSE)
    }
    utils::write.csv(answer_key, file.path(dir, "answer_key.csv"),
                     row.names = FALSE)
    message("Files written to: ", normalizePath(dir))
  }

  message(sprintf("batch_gen: %d datasets from '%s'", n_datasets, gen_name))
  message(sprintf("Answer key: %d rows x %d columns",
                  nrow(answer_key), ncol(answer_key)))

  invisible(list(
    results    = results,
    answer_key = answer_key,
    generator  = gen_name,
    seeds      = seeds
  ))
}

# Internal: flatten one generator result into a named list of scalars
# for the answer key. Captures atomic scalars, one-row data frames
# (column by column), htest objects (statistic/df/p-value), and recurses
# into plain lists with underscore-joined name prefixes. Skips the
# generated data, multi-row data frames, matrices, tables, and
# non-scalar vectors.
flatten_for_key <- function(x, prefix = "") {
  out <- list()
  for (nm in names(x)) {
    el  <- x[[nm]]
    key <- if (nzchar(prefix)) paste(prefix, nm, sep = "_") else nm
    if (inherits(el, "htest")) {
      out[[paste0(key, "_statistic")]] <-
        if (is.null(el$statistic)) NA_real_ else unname(el$statistic)[1]
      out[[paste0(key, "_df")]] <-
        if (is.null(el$parameter)) NA_real_ else unname(el$parameter)[1]
      out[[paste0(key, "_p_value")]] <-
        if (is.null(el$p.value)) NA_real_ else el$p.value
    } else if (is.data.frame(el)) {
      if (nrow(el) == 1) {
        for (cn in names(el)) {
          val <- el[[cn]][1]
          if (is.atomic(val) && length(val) == 1)
            out[[paste(key, cn, sep = "_")]] <- val
        }
      }
    } else if (is.matrix(el) || is.table(el)) {
      # skip
    } else if (is.list(el)) {
      out <- c(out, flatten_for_key(el, key))
    } else if (is.atomic(el) && length(el) == 1 && !is.function(el)) {
      out[[key]] <- el
    }
  }
  out
}

# Internal: pick the student-facing data frame out of a generator
# result, preferring the primary 'data' element, then long, then wide.
extract_primary_data <- function(result) {
  for (nm in c("data", "data_long", "data_wide")) {
    if (!is.null(result[[nm]]) && is.data.frame(result[[nm]]))
      return(result[[nm]])
  }
  NULL
}
