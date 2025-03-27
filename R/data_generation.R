#' Title
#'
#' @param a the first number
#' @param b the second number
#'
#' @returns the product
#' @export
#'
#' @examples
#' multiply(2, 2)
multiply = function (a, b) {
  a * b
}




pivot_schedule_longer <- function(schedule,
                                  time_cols,
                                  names_to = "time",
                                  names_pattern = ".*(\\d+)",
                                  names_transform = as.numeric,
                                  values_to = "condition",
                                  values_transform = as.factor,
                                  cohort_name = cohort,
                                  local_time = TRUE,
                                  local_time_int = 0) {

  schedule <- schedule |>
    tidyr::pivot_longer(
      cols = {{ time_cols }},
      names_to = names_to,
      names_pattern = names_pattern,
      names_transform = list(chron_time = names_transform),
      values_to = values_to,
      values_transform = list(condition = values_transform)
    )

  if (local_time) {
    cohort_name_char <- rlang::as_name(rlang::enquo(cohort_name))

    schedule <- schedule |>
      dplyr::group_by({{ cohort_name }}) |>
      dplyr::mutate(local_time = .data[[names_to]] - min(.data[[names_to]]) + local_time_int) |>
      dplyr::ungroup()
  }

  schedule
}

