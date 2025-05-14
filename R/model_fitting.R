#' @export
fit_models_old <- function(.data, .x, .f, ..., .progress = TRUE, .col = "model") {
  x_quo <- rlang::enquo(.x)
  f_fn  <- rlang::as_function(.f)
  input_list <- rlang::eval_tidy(x_quo, .data)

  # Choose parallel backend
  is_windows <- .Platform$OS.type == "windows"
  cores <- max(1L, parallel::detectCores() - 1L)

  # Parallel apply with progress bar
  if (is_windows) {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    result <- pbapply::pblapply(input_list, f_fn, ..., cl = cl)
  } else {
    options(mc.cores = cores)
    result <- pbapply::pblapply(input_list, f_fn, ...)
  }

  # Add result column
  .data |>
    dplyr::mutate(!!.col := result)
}

#' @export
# fit_models <- function(.data, .x, .f, ..., .progress = TRUE, .col = "model", .packages) {
#   x_quo <- rlang::enquo(.x)
#   f_fn  <- rlang::as_function(.f)
#   input_list <- rlang::eval_tidy(x_quo, .data)
#
#   is_windows <- .Platform$OS.type == "windows"
#   cores <- max(1L, parallel::detectCores() - 1L)
#
#   if (missing(.packages) || length(.packages) == 0) {
#     stop("You must explicitly provide the .packages argument, e.g., .packages = c(\"lme4\").")
#   }
#
#   if (is_windows) {
#     cl <- parallel::makeCluster(cores)
#     on.exit(parallel::stopCluster(cl), add = TRUE)
#
#     # Make .packages available inside the workers
#     parallel::clusterExport(cl, varlist = ".packages", envir = environment())
#
#     # Set working directory (optional but often helpful)
#     # parallel::clusterEvalQ(cl, setwd(getwd()))
#
#     # Load packages
#     parallel::clusterEvalQ(cl, {
#       for (pkg in .packages) {
#         suppressPackageStartupMessages(library(pkg, character.only = TRUE))
#       }
#       NULL
#     })
#
#     result <- pbapply::pblapply(input_list, f_fn, ..., cl = cl)
#   } else {
#     options(mc.cores = cores)
#
#     lapply(.packages, function(pkg) {
#       suppressPackageStartupMessages(library(pkg, character.only = TRUE))
#     })
#
#     result <- pbapply::pblapply(input_list, f_fn, ...)
#   }
#
#   .data |>
#     dplyr::mutate(!!.col := result)
# }



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


