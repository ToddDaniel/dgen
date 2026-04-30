#' Generate Individual Items from Composite Scores
#'
#' Back-solves individual scale items from composite scores. Items are numeric
#' values (integer by default) that average to the composite variable value.
#' Useful for giving students raw item-level data to aggregate.
#'
#' @param composite Numeric vector of composite scores
#' @param n_items Number of items to generate (default = 5)
#' @param scale_min Minimum scale value (default = 1)
#' @param scale_max Maximum scale value (default = 10)
#' @param decimals Decimal places for items (default = 0 for integers)
#' @param var_prefix Prefix for item names (default = \code{"Item"})
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Logical; export to CSV? (default = \code{FALSE})
#' @param run_analysis Logical; run item analysis? (default = \code{TRUE})
#'
#' @return A list with:
#'   \item{data}{Data frame with generated item columns}
#'   \item{composite_original}{Original composite scores}
#'   \item{composite_achieved}{Achieved composite (row means of items)}
#'   \item{accuracy}{List: mean absolute error, max error, error vector}
#'   \item{alpha}{Cronbach's alpha of generated items}
#'   \item{n_items}{Number of items}
#'   \item{item_analysis}{Item statistics and inter-item correlations (if
#'     \code{run_analysis = TRUE})}
#'
#' @details
#' With \code{n_items = 5} integer items, mean absolute error is typically
#' about ±0.10. With \code{n_items = 10}, error drops to ~±0.05. Set
#' \code{decimals = 1} for exact matching with fewer items.
#'
#' @examples
#' # Generate a composite variable then back-solve items
#' preds <- make_predictors(n = 100, FP = c(m = 5.0, sd = 1.5), seed = 1)
#' items <- item_gen(composite = preds$data$FP, n_items = 5,
#'                   scale_min = 1, scale_max = 10, var_prefix = "FP")
#'
#' @export
item_gen <- function(composite,
                     n_items      = 5,
                     scale_min    = 1,
                     scale_max    = 10,
                     decimals     = 0,
                     var_prefix   = "Item",
                     seed         = NULL,
                     export_csv   = FALSE,
                     run_analysis = TRUE) {

  if (!is.numeric(composite))
    stop("'composite' must be a numeric vector")
  if (any(is.na(composite)))
    stop("'composite' contains NA values")
  if (!is.numeric(n_items) || n_items < 2 || n_items != round(n_items))
    stop("'n_items' must be an integer >= 2")
  if (scale_min >= scale_max)
    stop("'scale_min' must be less than 'scale_max'")
  if (min(composite) < scale_min)
    stop("Some composite values (", round(min(composite), 2),
         ") are below scale_min (", scale_min, ")")
  if (max(composite) > scale_max)
    stop("Some composite values (", round(max(composite), 2),
         ") are above scale_max (", scale_max, ")")

  if (!is.null(seed)) set.seed(seed)

  n <- length(composite)

  items_matrix <- matrix(NA_real_, nrow = n, ncol = n_items)

  for (i in seq_len(n)) {
    target <- composite[i]

    items_raw     <- stats::rnorm(n_items, mean = target,
                                  sd = (scale_max - scale_min) / 8)
    items_raw     <- pmin(pmax(items_raw, scale_min), scale_max)
    items_rounded <- round(items_raw, decimals)

    current_mean <- mean(items_rounded)
    error        <- target - current_mean

    tol <- 10^(-(decimals + 1))

    if (abs(error) > tol) {
      items_adj <- round(items_rounded + error / n_items, decimals)
      items_adj <- pmin(pmax(items_adj, scale_min), scale_max)

      remaining <- target - mean(items_adj)
      if (abs(remaining) > tol) {
        adj_idx         <- sample.int(n_items, 1)
        items_adj[adj_idx] <- min(max(
          round(items_adj[adj_idx] + remaining * n_items, decimals),
          scale_min), scale_max)
      }
      items_matrix[i, ] <- items_adj
    } else {
      items_matrix[i, ] <- items_rounded
    }
  }

  item_names          <- paste0(var_prefix, seq_len(n_items))
  item_data           <- as.data.frame(items_matrix)
  colnames(item_data) <- item_names

  composite_achieved <- rowMeans(item_data)
  errors             <- composite - composite_achieved
  mean_abs_error     <- mean(abs(errors))
  max_error_val      <- max(abs(errors))

  # Cronbach's alpha
  item_vars <- apply(item_data, 2, stats::var)
  total_var <- stats::var(rowSums(item_data))
  alpha     <- (n_items / (n_items - 1)) * (1 - sum(item_vars) / total_var)

  item_analysis <- NULL

  if (run_analysis) {
    item_total_cors <- sapply(seq_len(n_items), function(j) {
      stats::cor(item_data[, j], composite_achieved)
    })

    item_means <- colMeans(item_data)
    item_sds   <- apply(item_data, 2, stats::sd)
    inter_cors <- stats::cor(item_data)

    alpha_if_deleted <- sapply(seq_len(n_items), function(j) {
      sub_mat   <- item_data[, -j, drop = FALSE]
      k_m       <- n_items - 1
      v_m       <- apply(sub_mat, 2, stats::var)
      tv_m      <- stats::var(rowSums(sub_mat))
      (k_m / (k_m - 1)) * (1 - sum(v_m) / tv_m)
    })

    item_stats <- data.frame(
      Item             = item_names,
      Mean             = round(item_means, 3),
      SD               = round(item_sds, 3),
      Item_Total_r     = round(item_total_cors, 3),
      Alpha_if_Deleted = round(alpha_if_deleted, 3),
      stringsAsFactors = FALSE
    )

    item_analysis <- list(
      item_stats            = item_stats,
      inter_item_correlations = inter_cors
    )
  }

  if (export_csv) {
    fn <- paste0(var_prefix, "_items_dgen.csv")
    utils::write.csv(item_data, file = fn, row.names = FALSE)
    message("Data saved to: ", file.path(getwd(), fn))
  }

  cat(sprintf("item_gen: %d items, n = %d\n", n_items, n))
  cat(sprintf("Mean abs error = %.4f | Cronbach's alpha = %.3f\n",
              mean_abs_error, alpha))

  result <- list(
    data               = item_data,
    composite_original = composite,
    composite_achieved = composite_achieved,
    accuracy           = list(mean_abs_error = mean_abs_error,
                               max_error      = max_error_val,
                               errors         = errors),
    alpha              = alpha,
    n_items            = n_items
  )

  if (run_analysis) result$item_analysis <- item_analysis

  invisible(result)
}
