add_random_intercept <- function(.data, variance = 1) {
  grouping_vars <- dplyr::group_vars(.data)
  intercept_name <- paste(c(".i", grouping_vars), collapse = "_")

  .data |>
    dplyr::mutate({{intercept_name}} := rnorm(1, sd = sqrt(variance)))
}

add_random_intercepts <- function(.data, nesting_structure, variance = 1){
  stopifnot(length(variance) == 1 | length(variance) == length(nesting_structure))

  if(length(variance) == 1){
    variance = rep(variance, times = length(nesting_structure))
  }

  nestings <- purrr::map(
    .x = seq_along(nesting_structure),
    .f = ~ nesting_structure[1:.x])

  for (i in seq_along(nestings)) {
    .data <- .data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(nestings[[i]]))) |>
      add_random_intercept(variance = variance[[i]])
  }

  .data
}

add_error <- function(.data, variance = 1){
  .data |>
    ungroup() |>
    mutate(.error = rnorm(n(), sd = sqrt(variance)))
}
