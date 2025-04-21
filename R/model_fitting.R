#' @export
fit_models <- function(.data, .x, .f, ..., .progress = TRUE, .col = "model") {
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
