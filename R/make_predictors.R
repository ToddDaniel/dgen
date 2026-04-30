#' Create a Set of Predictor Variables with Specified Properties
#'
#' Convenience function to generate multiple predictor variables in a single
#' call. Handles continuous and binary variables with optional correlation
#' structures. Works seamlessly with \code{\link{reg_gen}} for complete model
#' generation.
#'
#' @param n Sample size
#' @param ... Named variable specifications. Each named argument is a numeric
#'   vector:
#'   \itemize{
#'     \item Continuous: \code{c(m = mean, sd = sd)} (optionally add
#'       \code{decimals = value})
#'     \item Binary: \code{c(prev = prevalence)}
#'   }
#' @param correlations Optional. Correlations between variables. Either:
#'   \itemize{
#'     \item A named list: \code{list("X1-X2" = 0.4, "X1-M" = 0.5)}
#'     \item A correlation matrix with row/column names matching variable names
#'     \item \code{NULL} for uncorrelated variables (default)
#'   }
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Logical; export data to CSV? (default = \code{FALSE})
#'
#' @return A list with:
#'   \item{data}{Data frame with all generated variables}
#'   \item{var_info}{List describing each variable (type, mean, sd, etc.)}
#'   \item{correlations_target}{Target correlation matrix (or \code{NULL})}
#'   \item{correlations_achieved}{Achieved correlation matrix}
#'   \item{n}{Sample size}
#'
#' @details
#' Variable specification format:
#' \itemize{
#'   \item Continuous: \code{VarName = c(m = 50, sd = 10, decimals = 2)}
#'     (\code{decimals} defaults to 2)
#'   \item Binary: \code{VarName = c(prev = 0.5)}
#' }
#'
#' Correlation pairs use the format \code{"Var1-Var2" = r_value}.
#'
#' @examples
#' # Simple continuous predictors
#' preds <- make_predictors(n = 250,
#'   X1 = c(m = 50, sd = 10),
#'   X2 = c(m = 30, sd = 8))
#'
#' # Mixed continuous and binary with correlations
#' preds <- make_predictors(
#'   n = 250,
#'   FP          = c(m = 2.8, sd = 0.9),
#'   MD          = c(m = 2.6, sd = 0.85),
#'   Opportunity = c(prev = 0.5),
#'   correlations = list("FP-MD" = 0.40, "FP-Opportunity" = 0.15)
#' )
#'
#' @export
make_predictors <- function(n,
                             ...,
                             correlations = NULL,
                             seed         = NULL,
                             export_csv   = FALSE) {

  if (!is.numeric(n) || n <= 0 || n != round(n))
    stop("'n' must be a positive integer")

  if (!is.null(seed)) set.seed(seed)

  var_specs <- list(...)

  if (length(var_specs) == 0)
    stop("No variable specifications provided. ",
         "Provide at least one variable, e.g., X1 = c(m = 50, sd = 10)")

  var_names <- names(var_specs)
  if (is.null(var_names) || any(var_names == ""))
    stop("All variable specifications must be named")

  n_vars <- length(var_specs)

  # Classify variables
  var_info       <- list()
  continuous_vars <- character()
  binary_vars     <- character()

  for (i in seq_along(var_specs)) {
    vn   <- var_names[i]
    spec <- var_specs[[i]]

    if ("prev" %in% names(spec) || "prevalence" %in% names(spec)) {
      prev_val <- if ("prev" %in% names(spec)) spec["prev"] else spec["prevalence"]
      if (prev_val <= 0 || prev_val >= 1)
        stop("Prevalence for '", vn, "' must be between 0 and 1")
      var_info[[vn]] <- list(type = "binary", prevalence = unname(prev_val))
      binary_vars    <- c(binary_vars, vn)

    } else if ("m" %in% names(spec) && "sd" %in% names(spec)) {
      m_val  <- unname(spec["m"])
      sd_val <- unname(spec["sd"])
      dec_val <- if ("decimals" %in% names(spec)) unname(spec["decimals"]) else 2
      if (sd_val <= 0)
        stop("Standard deviation for '", vn, "' must be positive")
      var_info[[vn]] <- list(type = "continuous", mean = m_val,
                             sd = sd_val, decimals = dec_val)
      continuous_vars <- c(continuous_vars, vn)

    } else {
      stop("Invalid specification for '", vn, "'. ",
           "Use c(m = mean, sd = sd) for continuous or c(prev = p) for binary")
    }
  }

  # Parse correlation structure
  cor_matrix <- NULL

  if (!is.null(correlations)) {
    if (is.matrix(correlations)) {
      cor_matrix <- correlations
      if (!all(rownames(cor_matrix) %in% var_names) ||
          !all(colnames(cor_matrix) %in% var_names))
        stop("Correlation matrix row/column names must match variable names")

    } else if (is.list(correlations)) {
      cor_matrix              <- diag(n_vars)
      rownames(cor_matrix)    <- var_names
      colnames(cor_matrix)    <- var_names

      for (pair_name in names(correlations)) {
        r_val <- correlations[[pair_name]]
        parts <- strsplit(pair_name, "-")[[1]]
        if (length(parts) != 2)
          stop("Correlation pair '", pair_name,
               "' must be in 'Var1-Var2' format")
        v1 <- trimws(parts[1])
        v2 <- trimws(parts[2])
        if (!v1 %in% var_names || !v2 %in% var_names)
          stop("Variable(s) in '", pair_name,
               "' not found in variable specifications")
        if (abs(r_val) > 1)
          stop("Correlation for '", pair_name, "' must be between -1 and 1")
        i1 <- which(var_names == v1)
        i2 <- which(var_names == v2)
        cor_matrix[i1, i2] <- r_val
        cor_matrix[i2, i1] <- r_val
      }

    } else {
      stop("'correlations' must be a matrix or a named list")
    }

    eigenvalues <- eigen(cor_matrix)$values
    if (any(eigenvalues < -1e-8))
      stop("Correlation matrix is not positive definite. ",
           "Adjust correlation values so they are mathematically compatible.")
  }

  # Generate variables
  if (is.null(cor_matrix)) {
    data_out <- data.frame(matrix(nrow = n, ncol = 0))
    for (vn in var_names) {
      info <- var_info[[vn]]
      if (info$type == "continuous") {
        data_out[[vn]] <- round(stats::rnorm(n, info$mean, info$sd),
                                info$decimals)
      } else {
        data_out[[vn]] <- stats::rbinom(n, 1, info$prevalence)
      }
    }
  } else {
    latent           <- MASS::mvrnorm(n = n,
                                      mu    = rep(0, n_vars),
                                      Sigma = cor_matrix)
    colnames(latent) <- var_names
    data_out         <- data.frame(matrix(nrow = n, ncol = 0))
    for (vn in var_names) {
      info <- var_info[[vn]]
      z    <- latent[, vn]
      if (info$type == "continuous") {
        data_out[[vn]] <- round(z * info$sd + info$mean, info$decimals)
      } else {
        threshold      <- stats::qnorm(1 - info$prevalence)
        data_out[[vn]] <- as.integer(z > threshold)
      }
    }
  }

  achieved_cors <- stats::cor(data_out)

  if (export_csv) {
    if (n_vars <= 2) {
      fn <- paste0(paste(var_names, collapse = "_"), "_dgen.csv")
    } else {
      fn <- paste0(var_names[1], "_", var_names[2], "_plus",
                   n_vars - 2, "_dgen.csv")
    }
    utils::write.csv(data_out, file = fn, row.names = FALSE)
    message("Data saved to: ", file.path(getwd(), fn))
  }

  cat(sprintf("make_predictors: N = %d, %d variable(s): %s\n",
              n, n_vars, paste(var_names, collapse = ", ")))

  invisible(list(
    data                  = data_out,
    var_info              = var_info,
    correlations_target   = cor_matrix,
    correlations_achieved = achieved_cors,
    n                     = n
  ))
}
