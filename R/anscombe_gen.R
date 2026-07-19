#' Generate Anscombe-Style Datasets: Same Statistics, Different Shapes
#'
#' Creates several bivariate datasets that share identical summary
#' statistics — the same x mean/SD, y mean/SD, correlation, and
#' therefore the same fitted regression line — while looking completely
#' different when plotted. This is the "always plot your data" lesson in
#' generator form, in the spirit of Anscombe's (1973) quartet, but with
#' any sample size, any target statistics, and exact (not approximate)
#' matches.
#'
#' Available shapes, each teaching a distinct lesson:
#' \describe{
#'   \item{\code{"linear"}}{A well-behaved linear cloud — the one panel
#'     where the summary statistics tell the truth}
#'   \item{\code{"curved"}}{A strong curvilinear (parabolic) relationship
#'     that the correlation coefficient misdescribes}
#'   \item{\code{"outlier"}}{A weak cloud plus one extreme high-leverage
#'     point}
#'   \item{\code{"clusters"}}{Two separated groups; the correlation is
#'     carried entirely by group membership, with no relationship inside
#'     either group}
#'   \item{\code{"fan"}}{Heteroscedastic spread — residual variance grows
#'     with x, for teaching residual plots}
#' }
#'
#' Exactness works by construction: each panel's visual shape is built
#' first, then y is recomposed as \code{r * z_x + sqrt(1 - r^2) * z_e},
#' where \code{z_e} is the standardized regression residual of the shape
#' on x. Because residuals are orthogonal to x, the sample correlation
#' equals \code{r} exactly; both variables are then rescaled to the
#' target means/SDs and the usual dgen rounding refinement runs until
#' the rounded data still match. Since a regression line is determined
#' entirely by the five summary statistics, every panel's fitted line is
#' identical.
#'
#' The default targets (x mean 9, x SD 3.3, y mean 7.5, y SD 2,
#' r = 0.816) are an homage to Anscombe's original quartet.
#'
#' For the univariate version of this lesson — several samples with the
#' same mean and SD but different histograms — no separate function is
#' needed: call [dist_gen()] repeatedly with the same \code{m}/\code{sd}
#' and different \code{shape}s; each is exact by construction.
#'
#' @param n Points per dataset (minimum 10; default = 50)
#' @param x_mean,x_sd Target mean and SD of x (defaults 9 and 3.3)
#' @param y_mean,y_sd Target mean and SD of y (defaults 7.5 and 2)
#' @param r Target correlation, strictly between -1 and 1
#'   (default = 0.816)
#' @param shapes Character vector of shapes to generate, from
#'   \code{"linear"}, \code{"curved"}, \code{"outlier"},
#'   \code{"clusters"}, \code{"fan"} (default = the quartet
#'   \code{c("linear", "curved", "outlier", "clusters")})
#' @param decimals Number of decimal places for rounding (default = 2)
#' @param x_name,y_name Column names for the variables (defaults
#'   \code{"X"} and \code{"Y"})
#' @param seed Integer seed for reproducibility (default = NULL)
#' @param export_csv Either \code{FALSE} (default; nothing is written),
#'   \code{TRUE} (write a CSV with an auto-generated name to the current
#'   working directory), or a file path giving the CSV destination. The
#'   long-format data (all datasets stacked) is written
#' @param run_analysis Logical; fit the per-dataset regressions?
#'   (default = \code{TRUE})
#'
#' @return A list with:
#'   \item{data}{Long-format data frame: \code{Dataset}, x, y — ready
#'     for faceted plotting}
#'   \item{datasets}{Named list of the individual data frames}
#'   \item{stats_table}{One row per dataset: achieved means, SDs,
#'     correlation, and the (identical) regression intercept and slope}
#'   \item{lm_fits}{Named list of per-dataset \code{lm(y ~ x)} fits (if
#'     \code{run_analysis = TRUE})}
#'   \item{iterations}{Named vector of refinement iterations per
#'     dataset}
#'
#' @references Anscombe, F. J. (1973). Graphs in statistical analysis.
#'   The American Statistician, 27(1), 17-21.
#'
#' @seealso [cor_gen()] for a single correlated pair, [dist_gen()] for
#'   the univariate same-stats-different-histograms version
#'
#' @examples
#' quartet <- anscombe_gen(n = 50, seed = 42)
#' quartet$stats_table
#' # plot(quartet$data$X, quartet$data$Y) faceted by quartet$data$Dataset,
#' # e.g. with lattice or ggplot2:
#' # ggplot(quartet$data, aes(X, Y)) + geom_point() + facet_wrap(~Dataset)
#'
#' # Custom targets and shape selection
#' pair <- anscombe_gen(n = 80, x_mean = 100, x_sd = 15,
#'                      y_mean = 3, y_sd = 0.8, r = 0.6,
#'                      shapes = c("linear", "fan"), seed = 7)
#'
#' @concept Anscombe quartet
#' @concept data visualization
#' @concept same statistics different graphs
#'
#' @export
anscombe_gen <- function(n          = 50,
                         x_mean     = 9,
                         x_sd       = 3.3,
                         y_mean     = 7.5,
                         y_sd       = 2,
                         r          = 0.816,
                         shapes     = c("linear", "curved", "outlier",
                                        "clusters"),
                         decimals   = 2,
                         x_name     = "X",
                         y_name     = "Y",
                         seed       = NULL,
                         export_csv = FALSE,
                         run_analysis = TRUE) {

  all_shapes <- c("linear", "curved", "outlier", "clusters", "fan")
  if (!is.numeric(n) || length(n) != 1 || n < 10 || n != round(n))
    stop("'n' must be an integer >= 10")
  if (!is.numeric(x_sd) || x_sd <= 0 || !is.numeric(y_sd) || y_sd <= 0)
    stop("'x_sd' and 'y_sd' must be positive")
  if (!is.numeric(r) || length(r) != 1 || r <= -1 || r >= 1)
    stop("'r' must be strictly between -1 and 1")
  if (length(shapes) < 1 || !all(shapes %in% all_shapes))
    stop("'shapes' must be drawn from: ", paste(all_shapes, collapse = ", "))
  if (anyDuplicated(shapes)) stop("'shapes' must not contain duplicates")
  if (!is.numeric(decimals) || decimals < 0 || decimals != round(decimals))
    stop("'decimals' must be a non-negative integer")
  if (x_name == y_name) stop("'x_name' and 'y_name' must differ")

  if (!is.null(seed)) set.seed(seed)

  target_xm <- round(x_mean, decimals + 1)
  target_xs <- round(x_sd,   decimals + 1)
  target_ym <- round(y_mean, decimals + 1)
  target_ys <- round(y_sd,   decimals + 1)
  target_r  <- round(r, 3)
  max_iterations <- 100000

  datasets   <- vector("list", length(shapes))
  iterations <- integer(length(shapes))
  names(datasets) <- names(iterations) <- shapes

  for (si in seq_along(shapes)) {
    sh <- shapes[si]
    iteration <- 0

    repeat {
      iteration <- iteration + 1

      base <- anscombe_shape_base(sh, n)
      zx   <- as.vector(scale(base$x))
      e    <- stats::residuals(stats::lm(base$y0 ~ base$x))
      ze   <- as.vector(scale(e))
      zy   <- r * zx + sqrt(1 - r^2) * ze

      X <- round(zx * target_xs + target_xm, decimals)
      Y <- round(zy * target_ys + target_ym, decimals)

      ok <- round(mean(X), decimals + 1) == target_xm &&
            round(stats::sd(X), decimals + 1) == target_xs &&
            round(mean(Y), decimals + 1) == target_ym &&
            round(stats::sd(Y), decimals + 1) == target_ys &&
            round(stats::cor(X, Y), 3) == target_r
      if (ok) break
      if (iteration >= max_iterations) {
        warning("Maximum iterations reached for shape '", sh,
                "'. Results may not exactly match targets.")
        break
      }
    }

    df <- data.frame(X, Y, stringsAsFactors = FALSE)
    colnames(df) <- c(x_name, y_name)
    datasets[[si]]  <- df
    iterations[si]  <- iteration
  }

  data_long <- do.call(rbind, lapply(shapes, function(sh) {
    cbind(data.frame(Dataset = sh, stringsAsFactors = FALSE),
          datasets[[sh]])
  }))
  data_long$Dataset <- factor(data_long$Dataset, levels = shapes)
  rownames(data_long) <- NULL

  stats_table <- do.call(rbind, lapply(shapes, function(sh) {
    x <- datasets[[sh]][[x_name]]
    y <- datasets[[sh]][[y_name]]
    slope     <- stats::cor(x, y) * stats::sd(y) / stats::sd(x)
    intercept <- mean(y) - slope * mean(x)
    data.frame(Dataset   = sh,
               Mean_X    = round(mean(x), decimals + 1),
               SD_X      = round(stats::sd(x), decimals + 1),
               Mean_Y    = round(mean(y), decimals + 1),
               SD_Y      = round(stats::sd(y), decimals + 1),
               r         = round(stats::cor(x, y), 3),
               Intercept = round(intercept, 3),
               Slope     = round(slope, 3),
               stringsAsFactors = FALSE)
  }))
  rownames(stats_table) <- NULL

  lm_fits <- NULL
  if (run_analysis) {
    lm_fits <- lapply(datasets, function(df) {
      stats::lm(stats::as.formula(paste(y_name, "~", x_name)), data = df)
    })
  }

  dgen_export(data_long, export_csv,
              paste0(x_name, "_", y_name, "_anscombe_dgen.csv"))

  message(sprintf("anscombe_gen: %d dataset(s) x n = %d, identical statistics",
                  length(shapes), n))
  message(paste(utils::capture.output(
    print(stats_table, row.names = FALSE)), collapse = "\n"))

  invisible(list(
    data        = data_long,
    datasets    = datasets,
    stats_table = stats_table,
    lm_fits     = lm_fits,
    iterations  = iterations
  ))
}

# Internal: raw visual shape for one anscombe_gen panel, before the
# exact-statistics recomposition. Returns list(x, y0).
anscombe_shape_base <- function(shape, n) {
  switch(shape,
    linear = {
      x  <- stats::rnorm(n)
      y0 <- x + stats::rnorm(n, sd = 0.5)
      list(x = x, y0 = y0)
    },
    curved = {
      x  <- stats::runif(n, -1, 1)
      y0 <- -(x^2) + stats::rnorm(n, sd = 0.08)
      list(x = x, y0 = y0)
    },
    outlier = {
      x  <- stats::rnorm(n, sd = 0.6)
      y0 <- stats::rnorm(n, sd = 0.6)
      x[n]  <- 4
      y0[n] <- 4
      list(x = x, y0 = y0)
    },
    clusters = {
      comp <- rep(c(-1, 1), length.out = n)
      x  <- stats::rnorm(n, mean = comp * 1.8, sd = 0.55)
      y0 <- stats::rnorm(n, mean = comp * 1.8, sd = 0.55)
      list(x = x, y0 = y0)
    },
    fan = {
      x  <- stats::runif(n, 0, 1)
      y0 <- stats::rnorm(n, sd = 0.05 + x)
      list(x = x, y0 = y0)
    }
  )
}
