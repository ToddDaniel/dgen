#' Generate Outcome Variable from Regression Model
#'
#' Creates an outcome variable based on specified predictors and beta
#' coefficients. Supports both continuous and binary outcomes with precise
#' control over effect sizes. Optionally runs the regression automatically
#' and reports full results.
#'
#' @param predictors Data frame of predictor variables. Column names must
#'   match the names in \code{betas} (excluding \code{"intercept"}).
#' @param betas Named numeric vector of regression coefficients. Must include
#'   \code{"intercept"} plus names matching predictor columns.
#'   Example: \code{c(intercept = 10, X1 = 0.5, X2 = 0.3)}
#' @param outcome_name Name for the outcome variable (default = \code{"Y"})
#' @param outcome_type \code{"continuous"} or \code{"binary"}
#'   (default = \code{"continuous"})
#' @param r_squared Target R-squared for continuous outcomes (0 to 1,
#'   default = 0.50)
#' @param residual_sd Optional direct specification of residual SD for
#'   continuous outcomes; overrides \code{r_squared} if provided
#' @param prevalence Target proportion of 1s for binary outcomes (0 < p < 1,
#'   default = 0.5)
#' @param decimals Number of decimal places for continuous outcomes
#'   (default = 2)
#' @param seed Integer seed for reproducibility (default = \code{NULL})
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination
#' @param run_analysis Logical; run regression automatically?
#'   (default = \code{TRUE})
#'
#' @return A list with:
#'   \item{data}{Data frame with predictors and generated outcome}
#'   \item{betas_target}{Named vector of target beta coefficients}
#'   \item{betas_achieved}{Coefficients from fitted regression model}
#'   \item{outcome_stats}{Outcome summary statistics}
#'   \item{model_fit}{R-squared, residual SD, and related fit statistics}
#'   \item{regression_results}{Full \code{lm} or \code{glm} summary}
#'   \item{residuals}{Model residuals}
#'   \item{fitted_values}{Predicted values from the fitted model}
#'   \item{assumptions}{Assumption checks}
#'
#' @details
#' Uses matrix multiplication: \code{Y_hat = X %*% beta}.
#'
#' For continuous outcomes the residual variance is derived from
#' \code{r_squared}: \code{Var(res) = Var(Y_hat) * (1 - R2) / R2}.
#'
#' For binary outcomes a latent variable approach is used: the latent score is
#' standardised, Gaussian noise is added, and a threshold based on
#' \code{prevalence} is applied.
#'
#' @examples
#' set.seed(42)
#' preds <- data.frame(FP = rnorm(250, 2.8, 0.9), MD = rnorm(250, 2.6, 0.85))
#'
#' # Continuous outcome
#' result <- reg_gen(
#'   predictors   = preds,
#'   betas        = c(intercept = 10.5, FP = 0.20, MD = 0.48),
#'   outcome_name = "FS_log",
#'   r_squared    = 0.50
#' )
#'
#' # Binary outcome
#' fraud <- reg_gen(
#'   predictors   = preds,
#'   betas        = c(intercept = -1, FP = 0.5, MD = 0.8),
#'   outcome_name = "Fraud",
#'   outcome_type = "binary",
#'   prevalence   = 0.3
#' )
#'
#' @export
reg_gen <- function(predictors,
                    betas,
                    outcome_name = "Y",
                    outcome_type = "continuous",
                    r_squared    = 0.50,
                    residual_sd  = NULL,
                    prevalence   = 0.5,
                    decimals     = 2,
                    seed         = NULL,
                    export_csv   = FALSE,
                    run_analysis = TRUE) {

  if (!is.data.frame(predictors))
    stop("'predictors' must be a data frame")
  if (!is.numeric(betas) || is.null(names(betas)))
    stop("'betas' must be a named numeric vector")
  if (!"intercept" %in% names(betas))
    stop("'betas' must include an 'intercept' term")
  if (!outcome_type %in% c("continuous", "binary"))
    stop("'outcome_type' must be 'continuous' or 'binary'")
  if (!is.numeric(r_squared) || r_squared < 0 || r_squared > 1)
    stop("'r_squared' must be between 0 and 1")
  if (!is.numeric(prevalence) || prevalence <= 0 || prevalence >= 1)
    stop("'prevalence' must be between 0 and 1 (exclusive)")

  predictor_names <- setdiff(names(betas), "intercept")
  missing_preds   <- setdiff(predictor_names, names(predictors))
  if (length(missing_preds) > 0)
    stop("Predictor(s) in 'betas' not found in 'predictors': ",
         paste(missing_preds, collapse = ", "))

  extra_preds <- setdiff(names(predictors), predictor_names)
  if (length(extra_preds) > 0)
    warning("Column(s) in 'predictors' not in model: ",
            paste(extra_preds, collapse = ", "))

  if (!is.null(seed)) set.seed(seed)

  n <- nrow(predictors)

  # Design matrix
  X            <- as.matrix(cbind(intercept = 1,
                                  predictors[, predictor_names, drop = FALSE]))
  betas_ordered <- betas[colnames(X)]
  Y_hat         <- as.vector(X %*% betas_ordered)
  var_Y_hat     <- stats::var(Y_hat)

  # Generate outcome
  if (outcome_type == "continuous") {

    if (!is.null(residual_sd)) {
      res_sd <- residual_sd
    } else {
      if (r_squared == 0) {
        res_sd <- sqrt(var_Y_hat)
      } else {
        var_res <- var_Y_hat * (1 - r_squared) / r_squared
        res_sd  <- sqrt(var_res)
      }
    }

    eps   <- stats::rnorm(n, 0, res_sd)
    Y_raw <- Y_hat + eps
    Y     <- round(Y_raw, decimals)

    r_squared_achieved    <- var_Y_hat / stats::var(Y)
    residual_sd_achieved  <- stats::sd(Y - Y_hat)

    outcome_stats <- list(
      type   = "continuous",
      mean   = mean(Y),
      sd     = stats::sd(Y),
      min    = min(Y),
      max    = max(Y),
      median = stats::median(Y),
      n      = n
    )

    model_fit <- list(
      r_squared_target  = r_squared,
      r_squared_initial = r_squared_achieved,
      residual_sd_target = res_sd,
      var_predicted     = var_Y_hat,
      var_outcome       = stats::var(Y)
    )

  } else { # binary

    Y_hat_std <- scale(Y_hat)[, 1]
    epsilon   <- stats::rnorm(n, 0, 1)
    Y_star    <- Y_hat_std + epsilon
    threshold <- stats::quantile(Y_star, probs = 1 - prevalence)
    Y         <- as.integer(Y_star > threshold)

    prevalence_achieved <- mean(Y)
    pseudo_r2           <- stats::cor(Y, Y_hat)^2

    outcome_stats <- list(
      type                = "binary",
      prevalence_target   = prevalence,
      prevalence_achieved = prevalence_achieved,
      n_ones              = sum(Y),
      n_zeros             = sum(Y == 0),
      n                   = n
    )

    model_fit <- list(
      pseudo_r_squared  = pseudo_r2,
      point_biserial_cor = stats::cor(Y, Y_hat),
      threshold         = as.numeric(threshold)
    )
  }

  output_data               <- predictors
  output_data[[outcome_name]] <- Y

  # Regression analysis
  regression_results   <- NULL
  betas_achieved       <- NULL
  residuals_model      <- NULL
  fitted_values_model  <- NULL
  assumptions          <- NULL

  if (run_analysis) {
    formula_str <- paste(outcome_name, "~",
                         paste(predictor_names, collapse = " + "))
    formula_obj <- stats::as.formula(formula_str)

    if (outcome_type == "continuous") {
      model              <- stats::lm(formula_obj, data = output_data)
      regression_results <- summary(model)
      betas_achieved     <- stats::coef(model)
      names(betas_achieved)[1] <- "intercept"
      residuals_model    <- stats::residuals(model)
      fitted_values_model <- stats::fitted(model)

      model_fit$r_squared_achieved  <- regression_results$r.squared
      model_fit$r_squared_adjusted  <- regression_results$adj.r.squared
      model_fit$residual_sd_achieved <- regression_results$sigma
      model_fit$f_statistic         <- regression_results$fstatistic[1]
      model_fit$df_model            <- regression_results$fstatistic[2]
      model_fit$df_residual         <- regression_results$fstatistic[3]
      model_fit$p_value <- stats::pf(regression_results$fstatistic[1],
                                      regression_results$fstatistic[2],
                                      regression_results$fstatistic[3],
                                      lower.tail = FALSE)

      assumptions <- list()
      if (n <= 5000) {
        sw <- stats::shapiro.test(residuals_model)
        assumptions$normality <- list(test = "Shapiro-Wilk",
                                      statistic = sw$statistic,
                                      p_value   = sw$p.value)
      }

      fitted_grp <- cut(fitted_values_model, breaks = 3,
                        labels = c("Low", "Medium", "High"))
      grp_vars   <- tapply(residuals_model, fitted_grp, stats::var)
      assumptions$homoscedasticity <- list(
        variance_ratio = max(grp_vars) / min(grp_vars)
      )

    } else {
      model              <- stats::glm(formula_obj, data = output_data,
                                        family = stats::binomial(link = "logit"))
      regression_results <- summary(model)
      betas_achieved     <- stats::coef(model)
      names(betas_achieved)[1] <- "intercept"
      residuals_model    <- stats::residuals(model, type = "response")
      fitted_values_model <- stats::fitted(model)

      null_dev   <- model$null.deviance
      resid_dev  <- model$deviance
      model_fit$pseudo_r2_mcfadden  <- 1 - (resid_dev / null_dev)
      model_fit$pseudo_r2_nagelkerke <-
        (1 - exp((resid_dev - null_dev) / n)) /
        (1 - exp(-null_dev / n))
      model_fit$null_deviance     <- null_dev
      model_fit$residual_deviance <- resid_dev
      model_fit$aic               <- model$aic

      large_coefs <- abs(stats::coef(model)) > 10
      assumptions <- list(
        separation = if (any(large_coefs, na.rm = TRUE))
          list(warning = "Possible complete separation detected",
               large_coefficients = names(stats::coef(model))[large_coefs])
        else
          list(note = "No evidence of complete separation")
      )
    }
  }

  default_fn <- if (length(predictor_names) == 1) {
    paste0(predictor_names[1], "_", outcome_name, "_dgen.csv")
  } else {
    paste0(predictor_names[1], "_", predictor_names[2],
           if (length(predictor_names) > 2)
             paste0("_plus", length(predictor_names) - 2)
           else "", "_", outcome_name, "_dgen.csv")
  }
  dgen_export(output_data, export_csv, default_fn)

  message(sprintf("reg_gen: %s outcome '%s', N = %d, predictors: %s",
                  outcome_type, outcome_name, n,
                  paste(predictor_names, collapse = ", ")))
  if (outcome_type == "continuous" && run_analysis) {
    message(sprintf("R2 target = %.3f, achieved = %.3f | F = %.3f, p = %s",
                    r_squared,
                    if (!is.null(model_fit$r_squared_achieved))
                      model_fit$r_squared_achieved
                    else model_fit$r_squared_initial,
                    model_fit$f_statistic,
                    format.pval(model_fit$p_value, digits = 3, eps = 0.001)))
  } else if (outcome_type == "binary") {
    message(sprintf("prevalence target = %.3f, achieved = %.3f",
                    outcome_stats$prevalence_target,
                    outcome_stats$prevalence_achieved))
  }

  result <- list(
    data                = output_data,
    betas_target        = betas,
    outcome_stats       = outcome_stats,
    model_fit           = model_fit
  )
  if (run_analysis) {
    result$betas_achieved      <- betas_achieved
    result$regression_results  <- regression_results
    result$residuals           <- residuals_model
    result$fitted_values       <- fitted_values_model
    result$assumptions         <- assumptions
  }

  invisible(result)
}
