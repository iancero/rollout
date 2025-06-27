#' Fit models in parallel across a list-column of datasets
#'
#' Applies a user-specified model-fitting function to each element of a list-column
#' of datasets in `.data`, fitting models in parallel with a progress bar, and returns
#' the original data frame with a new `model` column containing the fitted models.
#'
#' @param .data A data frame containing a list-column of datasets to which the model
#' function will be applied.
#' @param .x Unquoted column name of the list-column containing the datasets.
#' @param .f A function or formula to apply to each dataset to fit the desired model
#' (e.g., `~ lm(y ~ x, data = .)` or `~ lme4::lmer(y ~ x + (x | group), data = .)`).
#' @param packages A character vector of package names to load on each parallel worker,
#' if your model-fitting function requires additional packages. Defaults to `NULL`.
#' @param n_cores Number of cores to use for parallel processing.
#' Defaults to `parallel::detectCores() - 1`.
#'
#' @return The original `.data` data frame with an additional `model` column containing
#' the fitted model objects returned by `.f`.
#'
#' @details
#' This function is intended for use in simulation pipelines where multiple datasets
#' are generated (e.g., via `simulate_datasets()`), and models need to be fitted to
#' each dataset efficiently in parallel.
#'
#' It uses `pbapply::pblapply()` to provide a progress bar during model fitting,
#' and `parallel::makeCluster()` for multi-core processing.
#'
#' Packages specified in `packages` will be loaded on each worker to ensure model-fitting
#' functions that depend on those packages work correctly in parallel.
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(lme4)
#'
#' # Create example grouped datasets for mixed models
#' datasets <- tibble(
#'   id = 1:5,
#'   data = map(1:5, ~ {
#'     df <- sleepstudy[sample(nrow(sleepstudy), 50, replace = TRUE), ]
#'     df$Subject <- factor(df$Subject)
#'     df
#'   })
#' )
#'
#' # Fit linear mixed models in parallel
#' fitted_models <- fit_models(
#'   datasets,
#'   .x = data,
#'   .f = ~ lme4::lmer(Reaction ~ Days + (Days | Subject), data = .),
#'   packages = c("lme4")
#' )
#'
#' # Inspect the first fitted mixed model
#' summary(fitted_models$model[[1]])
#'
#' # Tidy the fitted models using extract_model_results() for further evaluation
#' extracted <- extract_model_results(fitted_models)
#' head(extracted)
#'
#' # Summarise estimates for 'Days' across simulated fits
#' extracted |>
#'   filter(term == "Days") |>
#'   evaluate_model_results(
#'     mean_estimate = mean(estimate, na.rm = TRUE),
#'     sd_estimate = sd(estimate, na.rm = TRUE)
#'   )
#'
#' @export
fit_models <- function(.data, .x, .f, packages = NULL,
                       n_cores = parallel::detectCores() - 1) {
  x_col <- rlang::enquo(.x)
  fn <- purrr::as_mapper(.f)

  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Export required variables to the cluster
  parallel::clusterExport(cl, varlist = c("fn", "packages"), envir = environment())

  # Load necessary packages on each worker
  parallel::clusterEvalQ(cl, {
    lapply(packages, require, character.only = TRUE)
    NULL
  })

  # Evaluate the .x column inside .data to get the list-column of data frames
  x_list <- rlang::eval_tidy(x_col, .data)

  # Apply the model function in parallel with progress bar
  models <- pbapply::pblapply(x_list, function(df) fn(df), cl = cl)

  # Add results back to the data frame
  .data$model <- models
  .data
}
