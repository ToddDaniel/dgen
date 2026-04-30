#' Generate Scale Data for Teaching Reliability Analysis
#'
#' Creates a multi-item Likert scale with intentionally problematic items
#' for teaching reliability analysis. Students must identify bad items via
#' Cronbach's alpha, item-total correlations, and "alpha if item deleted"
#' statistics.
#'
#' @param n Sample size (default = 250)
#' @param n_items Total number of items (default = 8)
#' @param n_bad_items Number of problematic items to include (default = 2)
#' @param bad_item_type Type of problem: \code{"low_r"} (low inter-item
#'   correlation), \code{"reverse"} (reverse-coded but not reversed),
#'   \code{"random"} (pure noise), or \code{"low_var"} (low variance)
#'   (default = \code{"low_r"})
#' @param scale_min Minimum Likert value (default = 1)
#' @param scale_max Maximum Likert value (default = 7)
#' @param scale_mean Mean for the composite. \code{NULL} uses the midpoint
#'   (default = \code{NULL})
#' @param scale_sd SD for the composite. \code{NULL} uses range/4
#'   (default = \code{NULL})
#' @param target_alpha Desired Cronbach's alpha for good items only
#'   (default = 0.80)
#' @param var_prefix Prefix for item names (default = \code{"Item"})
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Logical; export student data to CSV? (default = \code{FALSE})
#'
#' @return A list with:
#'   \item{data}{Data frame with all items (student version — no hints)}
#'   \item{instructor_key}{Data frame identifying problematic items}
#'   \item{alpha_all}{Cronbach's alpha with all items}
#'   \item{alpha_good}{Cronbach's alpha with only good items}
#'   \item{bad_item_indices}{Integer vector of bad item positions}
#'   \item{bad_item_names}{Character vector of bad item names}
#'   \item{n_items}{Total number of items}
#'   \item{n_bad_items}{Number of bad items}
#'   \item{target_alpha}{Target alpha for good items}
#'
#' @details
#' Bad item types:
#' \describe{
#'   \item{\code{"low_r"}}{Items with low inter-item correlations (r ~ 0.10-0.20)}
#'   \item{\code{"reverse"}}{Items with negative correlations (reverse-coded
#'     but not reversed)}
#'   \item{\code{"random"}}{Pure noise, no relationship to the construct (r ~ 0)}
#'   \item{\code{"low_var"}}{Low-variance items where most respondents answer
#'     similarly}
#' }
#'
#' @examples
#' scale <- scale_gen(n = 200, n_items = 8, n_bad_items = 2,
#'                    target_alpha = 0.80, var_prefix = "Engagement",
#'                    seed = 42)
#' print(scale$instructor_key)
#'
#' @export
scale_gen <- function(n             = 250,
                      n_items       = 8,
                      n_bad_items   = 2,
                      bad_item_type = "low_r",
                      scale_min     = 1,
                      scale_max     = 7,
                      scale_mean    = NULL,
                      scale_sd      = NULL,
                      target_alpha  = 0.80,
                      var_prefix    = "Item",
                      seed          = NULL,
                      export_csv    = FALSE) {

  if (!is.numeric(n) || n <= 0 || n != round(n))
    stop("'n' must be a positive integer")
  if (!is.numeric(n_items) || n_items < 3)
    stop("'n_items' must be at least 3")
  if (!is.numeric(n_bad_items) || n_bad_items < 0)
    stop("'n_bad_items' must be non-negative")
  if (n_bad_items >= n_items)
    stop("'n_bad_items' must be less than 'n_items'")
  if (!bad_item_type %in% c("low_r", "reverse", "random", "low_var"))
    stop("'bad_item_type' must be one of: 'low_r', 'reverse', 'random', 'low_var'")
  if (!is.numeric(target_alpha) || target_alpha <= 0 || target_alpha >= 1)
    stop("'target_alpha' must be between 0 and 1 (exclusive)")

  if (!is.null(seed)) set.seed(seed)

  if (is.null(scale_mean)) scale_mean <- (scale_min + scale_max) / 2
  if (is.null(scale_sd))   scale_sd   <- (scale_max - scale_min) / 4

  n_good_items <- n_items - n_bad_items

  if (n_good_items < 3)
    warning("Very few good items (", n_good_items,
            "). Target alpha may not be achievable.")

  # Target inter-item correlation from alpha (Spearman-Brown formula)
  # alpha = (k * r_bar) / (1 + (k - 1) * r_bar)  =>  r_bar = alpha / (k - alpha*(k-1))
  target_r <- target_alpha / (n_good_items - target_alpha * (n_good_items - 1))

  cor_matrix_good <- matrix(target_r, nrow = n_good_items, ncol = n_good_items)
  diag(cor_matrix_good) <- 1

  good_items_latent <- MASS::mvrnorm(n     = n,
                                     mu    = rep(0, n_good_items),
                                     Sigma = cor_matrix_good)

  good_items_scaled <- scale(good_items_latent) * scale_sd + scale_mean

  good_items <- apply(good_items_scaled, 2, function(x) {
    pmin(pmax(round(x), scale_min), scale_max)
  })

  bad_items <- matrix(NA_real_, nrow = n, ncol = n_bad_items)

  if (n_bad_items > 0) {
    signal <- rowMeans(good_items)

    for (j in seq_len(n_bad_items)) {
      if (bad_item_type == "low_r") {
        noise  <- stats::rnorm(n)
        mixed  <- 0.15 * scale(signal)[, 1] + 0.85 * scale(noise)[, 1]
        scaled <- scale(mixed)[, 1] * scale_sd + scale_mean

      } else if (bad_item_type == "reverse") {
        scaled <- -scale(signal)[, 1] * scale_sd + scale_mean

      } else if (bad_item_type == "random") {
        scaled <- stats::rnorm(n, mean = scale_mean, sd = scale_sd)

      } else { # low_var
        scaled <- stats::rnorm(n, mean = scale_mean, sd = scale_sd / 4)
      }

      bad_items[, j] <- pmin(pmax(round(scaled), scale_min), scale_max)
    }
  }

  bad_positions  <- sort(sample(seq_len(n_items), n_bad_items))
  good_positions <- setdiff(seq_len(n_items), bad_positions)

  all_items <- matrix(NA_real_, nrow = n, ncol = n_items)
  all_items[, good_positions] <- good_items
  if (n_bad_items > 0) all_items[, bad_positions] <- bad_items

  item_names          <- paste0(var_prefix, seq_len(n_items))
  colnames(all_items) <- item_names
  scale_data          <- as.data.frame(all_items)

  # Cronbach's alpha calculation (internal helper)
  calc_alpha <- function(mat) {
    k         <- ncol(mat)
    item_vars <- apply(mat, 2, stats::var)
    total_var <- stats::var(rowSums(mat))
    (k / (k - 1)) * (1 - sum(item_vars) / total_var)
  }

  alpha_all  <- calc_alpha(all_items)
  alpha_good <- if (n_bad_items > 0) calc_alpha(good_items) else alpha_all

  # Item-total correlations
  total_score     <- rowMeans(all_items)
  item_total_cors <- sapply(seq_len(n_items), function(i) {
    stats::cor(all_items[, i], total_score)
  })

  item_type <- rep("GOOD", n_items)
  if (n_bad_items > 0)
    item_type[bad_positions] <- paste0("BAD (", bad_item_type, ")")

  instructor_key <- data.frame(
    Item         = item_names,
    Position     = seq_len(n_items),
    Type         = item_type,
    Item_Total_r = round(item_total_cors, 3),
    Mean         = round(colMeans(all_items), 2),
    SD           = round(apply(all_items, 2, stats::sd), 2),
    stringsAsFactors = FALSE
  )

  if (export_csv) {
    fn <- paste0(var_prefix, "_scale_dgen.csv")
    utils::write.csv(scale_data, file = fn, row.names = FALSE)
    message("Student data saved to: ", file.path(getwd(), fn))
  }

  cat(sprintf("scale_gen: %d items (%d good, %d bad [%s]), n = %d\n",
              n_items, n_good_items, n_bad_items, bad_item_type, n))
  cat(sprintf("Alpha (all items) = %.3f | Alpha (good items only) = %.3f\n",
              alpha_all, alpha_good))
  cat("Instructor key: bad item(s) at position(s)",
      paste(bad_positions, collapse = ", "), "\n")

  invisible(list(
    data            = scale_data,
    instructor_key  = instructor_key,
    alpha_all       = alpha_all,
    alpha_good      = alpha_good,
    bad_item_indices = if (n_bad_items > 0) bad_positions else integer(0),
    bad_item_names  = if (n_bad_items > 0) item_names[bad_positions] else character(0),
    n_items         = n_items,
    n_bad_items     = n_bad_items,
    target_alpha    = target_alpha
  ))
}
