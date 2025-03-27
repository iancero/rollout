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
                                  local_time = TRUE) {

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

    print(schedule)

    schedule <- schedule |>
      dplyr::group_by({{ cohort_name }}, condition) |>
      dplyr::mutate(local_time = row_number() - 1) |>
      dplyr::ungroup()
  }

  schedule
}

join_info <- function(long_schedule, unit_info, by = NULL,
                      uncount_vars = NULL, .ids = NULL) {

  schedule <- dplyr::left_join(long_schedule, unit_info, by = by)

  # Allow flexible input: character vector or list of quosures
  if (!is.null(uncount_vars)) {
    if (is.character(uncount_vars)) {
      uncount_vars <- rlang::syms(uncount_vars)
    }
    if (!is.list(uncount_vars)) {
      stop("`uncount_vars` must be a character vector or list of quosures.")
    }

    # .ids gives optional new column names for each uncount level
    if (is.null(.ids)) {
      .ids <- rep(NA_character_, length(uncount_vars))
    }

    for (i in seq_along(uncount_vars)) {
      var_quo <- uncount_vars[[i]]
      id_col <- .ids[[i]]

      var_name <- rlang::as_name(var_quo)

      schedule <- tidyr::uncount(schedule, weights = !!var_quo, .id = id_col)
    }
  }

  schedule
}






