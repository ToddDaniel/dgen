#' Plant Known Defects in a Dataset for Teaching Data Cleaning
#'
#' Takes a clean dataset and deliberately damages it in specified,
#' recorded ways: missing values, statistically plausible outliers,
#' data-entry typos, inconsistent category coding, and duplicated rows.
#' Returns the dirty student-facing data plus an instructor answer key
#' listing every planted defect — the tool for teaching data screening
#' and cleaning with data you can actually grade. This is the
#' general-purpose descendant of [scale_gen()]'s instructor key.
#'
#' \code{data} may be a plain data frame, a full dgen generator result
#' (the \code{data} element is extracted automatically, so
#' \code{dirty_data(dist_gen(...), ...)} works directly), or a single
#' file path to a CSV, which is read with \code{utils::read.csv()}.
#'
#' Defect types (each planted the requested number of times):
#' \describe{
#'   \item{\code{missing}}{A cell is blanked to \code{NA}. Any column.}
#'   \item{\code{outliers}}{A cell is replaced by a statistically
#'     extreme but plausible value: the column mean plus or minus
#'     3.5-4.5 column SDs, rounded to the column's apparent decimal
#'     precision. Numeric, non-constant columns only.}
#'   \item{\code{typos}}{A data-entry error: a shifted decimal point
#'     (4.5 becomes 45) or two transposed digits (27 becomes 72).
#'     Numeric columns only.}
#'   \item{\code{inconsistent}}{An existing category value is re-coded
#'     as a case/whitespace variant ("Male" becomes "male", "MALE", or
#'     " Male") — the lesson about tabulating categorical variables
#'     before trusting them. Character or factor columns only; a factor
#'     column receiving this defect is converted to character, as raw
#'     data files would be.}
#'   \item{\code{duplicates}}{An entire row is copied and inserted at a
#'     random position.}
#' }
#'
#' Cell-level defects are planted without replacement across the whole
#' set — no cell receives two defects, and cells that are already
#' \code{NA} in the input are never targeted. Row numbers in the answer
#' key refer to the returned (dirty) data, after any duplicate
#' insertions.
#'
#' If \code{export_csv} is used, only the dirty student-facing data are
#' written — never the answer key, which stays in the returned R object.
#'
#' @param data A data frame, a dgen generator result, or a CSV file path
#' @param missing Number of cells to blank to \code{NA} (default = 0)
#' @param outliers Number of statistical outliers to plant (default = 0)
#' @param typos Number of data-entry typos to plant (default = 0)
#' @param inconsistent Number of inconsistent category codings to plant
#'   (default = 0)
#' @param duplicates Number of duplicated rows to insert (default = 0)
#' @param columns Optional character vector restricting which columns
#'   may be corrupted (default \code{NULL} = all columns are eligible;
#'   useful for protecting an ID column)
#' @param answer_key Logical; compute and return the answer key?
#'   (default = \code{TRUE}). When \code{FALSE}, \code{answer_key} in
#'   the result is \code{NULL} — for when you just need a dirty dataset
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#'
#' @return (Invisibly) a list with:
#'   \item{data}{The dirty data frame}
#'   \item{answer_key}{One row per planted defect: \code{row},
#'     \code{column}, \code{defect_type}, \code{original},
#'     \code{corrupted} (\code{column}/\code{corrupted} are \code{NA}
#'     for duplicates, whose \code{original} names the copied row) —
#'     or \code{NULL} if \code{answer_key = FALSE}}
#'   \item{clean_data}{The untouched input data, for grading
#'     comparisons}
#'   \item{defect_summary}{Named vector of planted counts}
#'
#' @seealso [dist_gen()] — the two compose naturally:
#'   \code{dirty_data(dist_gen(...), ...)} yields non-normal *and*
#'   dirty data, about as close to real raw data as a generator gets
#'
#' @examples
#' clean <- stat_gen(n = 40, m = 50, sd = 10, seed = 42)
#' dirty <- dirty_data(clean, missing = 3, outliers = 2, typos = 1,
#'                     duplicates = 1, seed = 42)
#' dirty$answer_key
#'
#' # Skewed AND dirty: the full "raw data" experience
#' messy <- dirty_data(
#'   dist_gen(n = 60, m = 100, sd = 15, shape = "right_skew", seed = 1),
#'   missing = 4, outliers = 2, seed = 1
#' )
#'
#' # Just the dirty dataset, no key
#' quiz <- dirty_data(clean, missing = 2, outliers = 1,
#'                    answer_key = FALSE, seed = 7)
#'
#' @concept data cleaning
#' @concept data screening
#' @concept dirty data
#' @concept outliers
#' @concept missing data
#'
#' @export
dirty_data <- function(data,
                       missing      = 0,
                       outliers     = 0,
                       typos        = 0,
                       inconsistent = 0,
                       duplicates   = 0,
                       columns      = NULL,
                       answer_key   = TRUE,
                       seed         = NULL,
                       export_csv   = FALSE) {

  # --- Resolve the input to a data frame ---
  if (is.character(data) && length(data) == 1) {
    if (!file.exists(data)) stop("File not found: ", data)
    data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    # use as-is
  } else if (is.list(data)) {
    extracted <- extract_primary_data(data)
    if (is.null(extracted))
      stop("'data' is a list but contains no data frame ",
           "(expected a dgen generator result)")
    data <- extracted
  } else {
    stop("'data' must be a data frame, a dgen generator result, ",
         "or a CSV file path")
  }
  if (nrow(data) < 1 || ncol(data) < 1)
    stop("'data' must have at least one row and one column")

  counts <- c(missing = missing, outliers = outliers, typos = typos,
              inconsistent = inconsistent, duplicates = duplicates)
  for (nm in names(counts)) {
    v <- counts[[nm]]
    if (!is.numeric(v) || length(v) != 1 || v < 0 || v != round(v))
      stop("'", nm, "' must be a single non-negative integer")
  }
  if (sum(counts) == 0)
    stop("Nothing to plant: all defect counts are zero")

  if (is.null(columns)) {
    columns <- names(data)
  } else if (!all(columns %in% names(data))) {
    stop("'columns' contains names not in the data: ",
         paste(setdiff(columns, names(data)), collapse = ", "))
  }

  if (!is.null(seed)) set.seed(seed)

  clean_data <- data
  n_rows <- nrow(data)

  # --- Eligible columns per defect type ---
  is_num <- vapply(data, is.numeric, logical(1))
  is_cat <- vapply(data, function(x) is.character(x) || is.factor(x),
                   logical(1))
  non_constant <- vapply(data, function(x) {
    x <- x[!is.na(x)]
    length(unique(x)) > 1
  }, logical(1))

  cols_any     <- columns
  cols_numeric <- intersect(columns, names(data)[is_num])
  cols_outlier <- intersect(columns, names(data)[is_num & non_constant])
  cols_cat     <- intersect(columns, names(data)[is_cat])

  if (outliers > 0 && length(cols_outlier) == 0)
    stop("'outliers' requested but no eligible non-constant numeric ",
         "columns are available")
  if (typos > 0 && length(cols_numeric) == 0)
    stop("'typos' requested but no eligible numeric columns are available")
  if (inconsistent > 0 && length(cols_cat) == 0)
    stop("'inconsistent' requested but no eligible character/factor ",
         "columns are available")

  # --- Sample target cells without replacement across all types ---
  cell_pool <- function(cols) {
    cells <- expand.grid(row = seq_len(n_rows), column = cols,
                         stringsAsFactors = FALSE)
    keep <- !vapply(seq_len(nrow(cells)), function(i)
      is.na(data[cells$row[i], cells$column[i]]), logical(1))
    cells[keep, , drop = FALSE]
  }
  used <- character(0)
  draw_cells <- function(pool, n_wanted, type) {
    pool_ids <- paste(pool$row, pool$column, sep = "\r")
    avail    <- which(!pool_ids %in% used)
    if (length(avail) < n_wanted)
      stop("Not enough eligible cells for '", type, "': wanted ",
           n_wanted, ", only ", length(avail), " available")
    take <- avail[sample.int(length(avail), n_wanted)]
    used <<- c(used, pool_ids[take])
    pool[take, , drop = FALSE]
  }

  # Most constrained pools first
  cells_inc  <- if (inconsistent > 0)
    draw_cells(cell_pool(cols_cat), inconsistent, "inconsistent") else NULL
  cells_out  <- if (outliers > 0)
    draw_cells(cell_pool(cols_outlier), outliers, "outliers") else NULL
  cells_typo <- if (typos > 0)
    draw_cells(cell_pool(cols_numeric), typos, "typos") else NULL
  cells_miss <- if (missing > 0)
    draw_cells(cell_pool(cols_any), missing, "missing") else NULL

  # --- Plant cell-level defects ---
  key_rows <- list()
  add_key <- function(row, column, type, original, corrupted) {
    key_rows[[length(key_rows) + 1]] <<- data.frame(
      row = row, column = column, defect_type = type,
      original = original, corrupted = corrupted,
      stringsAsFactors = FALSE)
  }

  if (!is.null(cells_inc)) {
    for (i in seq_len(nrow(cells_inc))) {
      r <- cells_inc$row[i]; cl <- cells_inc$column[i]
      if (is.factor(data[[cl]])) data[[cl]] <- as.character(data[[cl]])
      orig <- data[r, cl]
      variants <- unique(c(tolower(orig), toupper(orig),
                           paste0(" ", orig), paste0(orig, " ")))
      variants <- variants[variants != orig]
      corrupted <- if (length(variants) == 0) paste0(orig, " ")
                   else variants[sample.int(length(variants), 1)]
      data[r, cl] <- corrupted
      add_key(r, cl, "inconsistent", orig, corrupted)
    }
  }

  if (!is.null(cells_out)) {
    for (i in seq_len(nrow(cells_out))) {
      r <- cells_out$row[i]; cl <- cells_out$column[i]
      orig  <- data[r, cl]
      col_m <- mean(clean_data[[cl]], na.rm = TRUE)
      col_s <- stats::sd(clean_data[[cl]], na.rm = TRUE)
      direction <- sample(c(-1, 1), 1)
      value <- col_m + direction * stats::runif(1, 3.5, 4.5) * col_s
      value <- round(value, infer_decimals(clean_data[[cl]]))
      data[r, cl] <- value
      add_key(r, cl, "outlier", as.character(orig), as.character(value))
    }
  }

  if (!is.null(cells_typo)) {
    for (i in seq_len(nrow(cells_typo))) {
      r <- cells_typo$row[i]; cl <- cells_typo$column[i]
      orig  <- data[r, cl]
      value <- make_typo(orig)
      data[r, cl] <- value
      add_key(r, cl, "typo", as.character(orig), as.character(value))
    }
  }

  if (!is.null(cells_miss)) {
    for (i in seq_len(nrow(cells_miss))) {
      r <- cells_miss$row[i]; cl <- cells_miss$column[i]
      orig <- data[r, cl]
      data[r, cl] <- NA
      add_key(r, cl, "missing", as.character(orig), NA_character_)
    }
  }

  # --- Insert duplicate rows at random positions ---
  if (duplicates > 0) {
    src <- if (duplicates <= n_rows) {
      sample.int(n_rows, duplicates)
    } else {
      sample.int(n_rows, duplicates, replace = TRUE)
    }
    row_seq <- seq_len(n_rows)          # markers > n_rows are duplicates
    for (d in seq_len(duplicates)) {
      pos <- sample.int(length(row_seq) + 1, 1)
      row_seq <- append(row_seq, n_rows + d, after = pos - 1)
    }
    resolve <- row_seq
    is_dup  <- row_seq > n_rows
    resolve[is_dup] <- src[row_seq[is_dup] - n_rows]
    new_data <- data[resolve, , drop = FALSE]
    rownames(new_data) <- NULL

    # Remap cell-defect key rows to post-insertion positions
    new_pos <- match(seq_len(n_rows), row_seq)
    for (i in seq_along(key_rows))
      key_rows[[i]]$row <- new_pos[key_rows[[i]]$row]
    for (d in seq_len(duplicates)) {
      add_key(which(row_seq == n_rows + d), NA_character_, "duplicate",
              paste("duplicate of row", new_pos[src[d]]), NA_character_)
    }
    data <- new_data
  }

  key <- NULL
  if (answer_key) {
    key <- do.call(rbind, key_rows)
    key <- key[order(key$row, key$column, na.last = TRUE), , drop = FALSE]
    rownames(key) <- NULL
  }

  dgen_export(data, export_csv, "dirty_data_dgen.csv")

  message(sprintf("dirty_data: planted %d defect(s) in a %d x %d dataset",
                  sum(counts), nrow(data), ncol(data)))
  message(sprintf(
    "missing = %d | outliers = %d | typos = %d | inconsistent = %d | duplicates = %d",
    missing, outliers, typos, inconsistent, duplicates))

  invisible(list(
    data           = data,
    answer_key     = key,
    clean_data     = clean_data,
    defect_summary = counts
  ))
}

# Internal: apparent decimal precision of a numeric column (capped at 4).
infer_decimals <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(0)
  s <- format(x, scientific = FALSE, trim = TRUE)
  dec <- ifelse(grepl("\\.", s), nchar(sub("^[^.]*\\.", "", s)), 0)
  min(max(dec), 4)
}

# Internal: produce a data-entry typo of a numeric value — transposed
# adjacent digits when possible, otherwise a shifted decimal point.
# Guaranteed to differ from the original.
make_typo <- function(value) {
  if (!is.na(value) && value != 0 && stats::runif(1) < 0.5) {
    transposed <- transpose_digits(value)
    if (!is.null(transposed) && transposed != value) return(transposed)
  }
  shifted <- value * sample(c(10, 0.1), 1)
  if (!is.na(shifted) && shifted != value) return(shifted)
  99  # last-resort replacement (value was 0)
}

# Internal: swap two adjacent differing digits of a number, preserving
# sign and decimal point. Returns NULL when no such pair exists.
transpose_digits <- function(value) {
  chars <- strsplit(as.character(value), "")[[1]]
  digit_idx <- which(chars %in% as.character(0:9))
  if (length(digit_idx) < 2) return(NULL)
  cand <- Filter(function(k) chars[digit_idx[k]] != chars[digit_idx[k + 1]],
                 seq_len(length(digit_idx) - 1))
  if (length(cand) == 0) return(NULL)
  k  <- cand[sample.int(length(cand), 1)]
  i1 <- digit_idx[k]; i2 <- digit_idx[k + 1]
  tmp <- chars[i1]; chars[i1] <- chars[i2]; chars[i2] <- tmp
  as.numeric(paste(chars, collapse = ""))
}
