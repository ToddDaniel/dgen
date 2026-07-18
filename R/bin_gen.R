#' Generate Binary Variable Correlated with Continuous Variable
#'
#' Creates a binary variable (0/1) with a specified correlation to an
#' existing continuous variable using a latent variable threshold method.
#'
#' @param continuous_var Numeric vector of the continuous variable
#' @param prevalence Proportion of 1s in the binary variable (0 < prevalence < 1,
#'   default = 0.5)
#' @param target_cor Target point-biserial correlation with the continuous
#'   variable
#' @param var_name Name for binary variable (default = \code{"Binary_Var"})
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#'
#' @return A list with:
#'   \item{data}{Data frame with continuous and binary variables}
#'   \item{target_cor}{Target correlation specified}
#'   \item{achieved_cor}{Achieved point-biserial correlation}
#'   \item{prevalence_target}{Target prevalence specified}
#'   \item{prevalence_achieved}{Achieved prevalence}
#'
#' @examples
#' scores <- stat_gen(n = 100, m = 50, sd = 10, var_name = "Score")
#' result <- bin_gen(
#'   continuous_var = scores$data$Score,
#'   prevalence = 0.5,
#'   target_cor = 0.4,
#'   var_name = "Group",
#'   seed = 12345
#' )
#' cor(result$data$Score, result$data$Group)
#'
#' @export
bin_gen <- function(continuous_var,
                    prevalence  = 0.5,
                    target_cor,
                    var_name    = "Binary_Var",
                    seed        = NULL,
                    export_csv  = FALSE) {

  if (!is.numeric(continuous_var))
    stop("'continuous_var' must be numeric")
  if (any(is.na(continuous_var)))
    stop("'continuous_var' contains NA values")
  n <- length(continuous_var)
  if (n < 2)
    stop("'continuous_var' must have at least 2 observations")
  if (stats::sd(continuous_var) == 0)
    stop("'continuous_var' has zero variance")
  if (!is.numeric(prevalence) || prevalence <= 0 || prevalence >= 1)
    stop("'prevalence' must be between 0 and 1 (exclusive)")
  if (!is.numeric(target_cor) || abs(target_cor) > 1)
    stop("'target_cor' must be between -1 and 1")

  max_r_approx <- 2 * sqrt(prevalence * (1 - prevalence))
  if (abs(target_cor) > max_r_approx) {
    warning(sprintf(
      "target_cor (%.3f) may not be achievable with prevalence = %.3f; max ~ %.3f",
      target_cor, prevalence, max_r_approx))
  }

  if (!is.null(seed)) set.seed(seed)

  continuous_z  <- scale(continuous_var)[, 1]
  independent_z <- stats::rnorm(n)
  latent_z      <- target_cor * continuous_z +
                   sqrt(1 - target_cor^2) * independent_z

  threshold  <- stats::qnorm(1 - prevalence)
  binary_var <- as.numeric(latent_z > threshold)

  achieved_prevalence <- mean(binary_var)
  achieved_cor        <- stats::cor(continuous_var, binary_var)

  continuous_var_name <- deparse(substitute(continuous_var))
  # Clean up name if it came from a data frame column reference
  if (grepl("\\$", continuous_var_name)) {
    continuous_var_name <- sub(".*\\$", "", continuous_var_name)
  }

  data_out           <- data.frame(continuous_var, binary_var,
                                   stringsAsFactors = FALSE)
  colnames(data_out) <- c(continuous_var_name, var_name)

  dgen_export(data_out, export_csv,
              paste0(continuous_var_name, "_", var_name, "_dgen.csv"))

  message(sprintf("bin_gen: N = %d | prevalence target = %.3f, achieved = %.3f",
                  n, prevalence, achieved_prevalence))
  message(sprintf("Correlation target = %.3f, achieved = %.3f",
                  target_cor, achieved_cor))

  invisible(list(
    data               = data_out,
    target_cor         = target_cor,
    achieved_cor       = achieved_cor,
    prevalence_target  = prevalence,
    prevalence_achieved = achieved_prevalence
  ))
}
