fixed_effects <- function(.data, ...) {
  dots <- rlang::enquos(...)

  .data |>
    dplyr::mutate(!!!dots)
}
