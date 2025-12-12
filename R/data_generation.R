#' Pivot a rollout schedule from wide to long format with local time calculation
#'
#' Transforms a wide-format rollout schedule into a long-format schedule, extracting chronological time from column names,
#' converting condition columns to factors, and adding local time within each cohort if desired.
#'
#' @param schedule A data frame containing the rollout schedule in wide format.
#' @param time_cols Columns containing time-specific condition assignments (tidyselect syntax).
#' @param names_to Name of the new column to store extracted chronological time (default `"chron_time"`).
#' @param names_pattern Regular expression to extract the numeric time from column names (default `".*(\\d+)"`).
#' @param names_transform Function to transform extracted time values (default `as.numeric`).
#' @param values_to Name of the new column to store condition values (default `"condition"`).
#' @param values_transform Function to transform condition values (default `as.factor`).
#' @param cohort_name The column indicating cohort membership for local time calculation (default `cohort`).
#' @param local_time Logical; if `TRUE`, adds a `local_time` column indicating time since rollout start for each cohort and condition (default `TRUE`).
#'
#' @return A long-format `tibble` with columns for cohort, condition, chronological time, and optionally local time.
#' @examples
#' library(dplyr)
#' library(tidyr)
#' schedule <- tibble::tibble(
#'   site = c("A", "B"),
#'   cohort = c(1, 2),
#'   t1 = c("control", "intervention"),
#'   t2 = c("intervention", "intervention")
#' )
#' pivot_schedule_longer(schedule, time_cols = starts_with("t"))
#' @export
pivot_schedule_longer <- function(schedule,
                                  time_cols,
                                  names_to = "chron_time",
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
      names_transform = setNames(list(names_transform), names_to),
      values_to = values_to,
      values_transform = setNames(list(values_transform), values_to)
    )

  if (local_time) {
    cohort_name_char <- rlang::as_name(rlang::enquo(cohort_name))

    schedule <- schedule |>
      dplyr::group_by(
        rlang::.data[[cohort_name_char]],
        rlang::.data[[values_to]]
      ) |>
      dplyr::mutate(local_time = dplyr::row_number() - 1L) |>
      dplyr::ungroup()
  }

  schedule
}


#' Join unit-level information to a long-format rollout schedule
#'
#' Merges unit-level characteristics or parameters into a long-format rollout schedule and optionally expands rows
#' based on count variables to create multiple units per site.
#'
#' @param long_schedule A long-format schedule (output from `pivot_schedule_longer`).
#' @param unit_info A data frame with unit-level information to join.
#' @param by Columns used to join `long_schedule` and `unit_info` (default `NULL` uses shared columns).
#' @param uncount_vars Optional character vector or list of quosures indicating count variables to expand rows.
#' @param .ids Optional character vector specifying names of id columns when uncounting, one per `uncount_var`.
#'
#' @return A `tibble` with joined and optionally expanded rows to reflect unit counts.
#' @examples
#' schedule <- tibble::tibble(site = "A", cohort = 1, chron_time = 0, condition = "control")
#' unit_info <- tibble::tibble(site = "A", n_units = 3)
#' join_info(schedule, unit_info, by = "site", uncount_vars = "n_units")
#' @export
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
      .ids <- uncount_vars |>
        as.character() |>
        paste0("_id")
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

#' Add replicate identifiers for simulation replicates
#'
#' Expands a long-format schedule to include a replicate identifier for running multiple simulation replicates efficiently.
#'
#' @param long_schedule A long-format rollout schedule.
#' @param n Integer specifying the number of replicates to generate.
#'
#' @return A `tibble` with an added `sample_id` column for replicate indexing.
#' @examples
#' schedule <- tibble::tibble(site = "A", cohort = 1, chron_time = 0, condition = "control")
#' initialize_replicates(schedule, n = 3)
#' @export
initialize_replicates <- function(long_schedule, n) {
  long_schedule |>
    tidyr::expand_grid(sample_id = seq_len(n)) |>
    dplyr::select("sample_id", dplyr::everything())
}


#' Expand a data frame with parameter combinations for simulation
#'
#' Adds combinations of specified parameter values to a data frame for simulation by expanding over all combinations.
#'
#' @param df A data frame to expand.
#' @param ... Named vectors specifying parameter values to expand, provided as `param_name = values`.
#'
#' @return A `tibble` with added parameter columns for each combination of values.
#' @examples
#' df <- tibble::tibble(site = "A", condition = "control")
#' add_parameter(df, beta = c(0, 0.5), sigma = c(1, 2))
#' @export
add_parameter <- function(df, ...) {
  dots <- rlang::enquos(...)

  # Evaluate expressions in the calling environment
  evaluated <- purrr::imap(dots, ~ rlang::eval_tidy(.x))

  df |>
    tidyr::expand_grid(!!!evaluated)
}









