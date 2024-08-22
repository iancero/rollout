#' Helper function to check if columns exist in a data frame
#'
#' @param df A data frame
#' @param cols A character vector of column names
#' @param fun_name The name of the calling function (for error messages)
#'
#' @return NULL (invisibly) if all columns exist, otherwise stops with an error
check_columns_exist <- function(df, cols, fun_name) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("In %s: The following columns are missing from the input data frame: %s",
                 fun_name, paste(missing_cols, collapse = ", ")))
  }
  invisible(NULL)
}

#' Pivot Schedule to Longer Format
#'
#' @param schedule A data frame containing the schedule.
#' @param time_cols Columns to pivot.
#' @param names_to Name of the new 'names' column.
#' @param names_pattern Regex pattern for extracting time from column names.
#' @param names_transform Function to transform the time values.
#' @param values_to Name of the new 'values' column.
#' @param values_transform Function to transform the condition values.
#' @param cohort_name Name of the cohort column.
#' @param condition_time Whether to calculate condition time.
#' @param condition_time_int Initial value for condition time.
#'
#' @return A data frame in long format.
#' @export
#'
#' @examples
#' schedule <- data.frame(
#'   cohort = c("A", "B"),
#'   t1 = c("control", "control"),
#'   t2 = c("intervention", "control"),
#'   t3 = c("intervention", "intervention")
#' )
#' pivot_schedule_longer(schedule, t1:t3)
pivot_schedule_longer <- function(schedule, time_cols,
                                  names_to = "time",
                                  names_pattern = ".*(\\d+)",
                                  names_transform = as.numeric,
                                  values_to = "condition",
                                  values_transform = as.factor,
                                  cohort_name = cohort,
                                  condition_time = TRUE,
                                  condition_time_int = 0) {

  # Check if required columns exist
  check_columns_exist(schedule, as.character(substitute(time_cols)), "pivot_schedule_longer")

  if (!is.data.frame(schedule)) {
    stop("In pivot_schedule_longer: 'schedule' must be a data frame.")
  }

  schedule <- schedule |>
    tidyr::pivot_longer(
      cols = {{ time_cols }},
      names_to = names_to,
      names_pattern = names_pattern,
      names_transform = list(time = names_transform),
      values_to = values_to,
      values_transform = list(condition = values_transform)
    )

  if (condition_time) {
    cohort_name_char <- rlang::as_name(rlang::enquo(cohort_name))
    check_columns_exist(schedule, c(cohort_name_char, names_to), "pivot_schedule_longer")

    schedule <- schedule |>
      dplyr::group_by({{ cohort_name }}) |>
      dplyr::mutate(condition_time = .data[[names_to]] - min(.data[[names_to]]) + condition_time_int) |>
      dplyr::ungroup()
  }

  schedule
}

#' Join Unit Information to Schedule
#'
#' @param long_schedule A data frame containing the long-format schedule.
#' @param unit_info A data frame containing unit information.
#' @param by Column(s) to join by.
#' @param uncount_unit Column to use for uncounting (expanding) rows.
#' @param .id Name of the new ID column created when uncounting.
#'
#' @return A data frame with unit information joined to the schedule.
#' @export
#'
#' @examples
#' long_schedule <- data.frame(
#'   cohort = c("A", "A", "B", "B"),
#'   time = c(1, 2, 1, 2),
#'   condition = c("control", "intervention", "control", "control")
#' )
#' unit_info <- data.frame(
#'   cohort = c("A", "B"),
#'   num_units = c(2, 3)
#' )
#' join_unit_info(long_schedule, unit_info, by = "cohort", uncount_unit = num_units)
join_unit_info <- function(long_schedule, unit_info, by = NULL, uncount_unit = NULL, .id = NULL) {
  if (!is.data.frame(long_schedule) || !is.data.frame(unit_info)) {
    stop("In join_unit_info: Both 'long_schedule' and 'unit_info' must be data frames.")
  }

  if (!is.null(by)) {
    check_columns_exist(long_schedule, by, "join_unit_info")
    check_columns_exist(unit_info, by, "join_unit_info")
  }

  schedule <- long_schedule |>
    dplyr::left_join(unit_info, by = by)

  uncount_unit_quo <- rlang::enquo(uncount_unit)
  if (!rlang::is_null(uncount_unit_quo)) {
    uncount_unit_char <- rlang::as_name(uncount_unit_quo)
    check_columns_exist(schedule, uncount_unit_char, "join_unit_info")

    schedule <- schedule |>
      tidyr::uncount(weights = !!uncount_unit_quo, .id = .id)
  }

  schedule
}

#' Initialize Replicates for Simulation
#'
#' @param long_schedule A data frame containing the long-format schedule.
#' @param n Number of replicates to create.
#'
#' @return A data frame with replicated rows for simulation.
#' @export
#'
#' @examples
#' long_schedule <- data.frame(
#'   cohort = c("A", "A", "B", "B"),
#'   time = c(1, 2, 1, 2),
#'   condition = c("control", "intervention", "control", "control")
#' )
#' initialize_replicates(long_schedule, n = 3)
initialize_replicates <- function(long_schedule, n) {
  if (!is.data.frame(long_schedule)) {
    stop("In initialize_replicates: 'long_schedule' must be a data frame.")
  }

  if (!is.numeric(n) || n <= 0 || n != round(n)) {
    stop("In initialize_replicates: 'n' must be a positive integer.")
  }

  long_schedule |>
    tidyr::expand_grid(sim_sample = seq(n)) |>
    dplyr::select(sim_sample, dplyr::everything())
}

#' Read Example Excel File
#'
#' @param path Path to the Excel file. If NULL, lists available example files.
#'
#' @return Either a character vector of available files or the path to the specified file.
#' @export
#'
#' @examples
#' # List available example files
#' excel_example_path()
#'
#' # Get path to a specific example file
#' excel_example_path("common_rollout_designs.xlsx")
excel_example_path <- function(path = NULL) {
  if (is.null(path)) {
    return(dir(system.file("extdata", package = "rollout")))
  }

  full_path <- system.file("extdata", path, package = "rollout", mustWork = FALSE)
  if (full_path == "") {
    stop(sprintf("In excel_example_path: File '%s' not found in the package's extdata directory.", path))
  }

  full_path
}

#' Read Example Excel Sheet
#'
#' @param example Sheet number to read from the example Excel file.
#' @param file_name Name of the Excel file to read from (default: "common_rollout_designs.xlsx").
#'
#' @return A data frame containing the data from the specified Excel sheet.
#' @export
#'
#' @examples
#' # Read the first sheet from the default example Excel file
#' read_excel_example(1)
#'
#' # Read the second sheet from a specific example Excel file
#' read_excel_example(2, "another_example_file.xlsx")
read_excel_example <- function(example = 1, file_name = "common_rollout_designs.xlsx") {
  if (!is.numeric(example) || example <= 0 || example != round(example)) {
    stop("In read_excel_example: 'example' must be a positive integer.")
  }

  path <- excel_example_path(path = file_name)

  tryCatch(
    readxl::read_xlsx(path = path, sheet = example),
    error = function(e) {
      stop(sprintf("In read_excel_example: Error reading sheet %d from file '%s': %s",
                   example, file_name, e$message))
    }
  )
}
