excel_example_path <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "rollout"))
  } else {
    system.file("extdata", path, package = "rollout", mustWork = TRUE)
  }
}

read_excel_example <- function(example = 1) {
  path <- excel_example_path(path = "common_rollout_designs.xlsx")

  readxl::read_xlsx(path = path, sheet = example)
}

pivot_schedule_longer <- function(schedule, time_cols,
                                  names_to = "time",
                                  names_pattern = ".*(\\d+)",
                                  names_transform = as.numeric,
                                  values_to = "condition",
                                  values_transform = as.factor,
                                  cohort_name = cohort,
                                  condition_time = TRUE,
                                  condition_time_int = 0) {
  schedule <- schedule |>
    tidyr::pivot_longer(
      cols = {{ time_cols }},
      names_to = names_to,
      names_pattern = names_pattern,
      names_transform = names_transform,
      values_to = values_to,
      values_transform = values_transform
    )

  if (condition_time){
    schedule <- schedule |>
      dplyr::group_by({{cohort_name}}) |>
      # TODO: dplyr::arrange({{ names_to }}) |>
      dplyr::mutate(condition_time = time - min(time)) |>
      dplyr::ungroup()
  }

  schedule
}

# join_unit_info <- function(long_schedule, unit_info, by = NULL, uncount_unit = NULL, .id = NULL){
#   schedule <- long_schedule |>
#     dplyr::left_join(unit_info, by = by)
#
#   if(!is.null(uncount_unit)){
#
#     print(schedule)
#
#     schedule <- schedule |>
#       tidyr::uncount(weights = !!rlang::ensym(uncount_unit))
#   }
#
#   schedule
# }

# join_unit_info <- function(long_schedule, unit_info, by = NULL, uncount_unit = NULL, .id = NULL){
#   schedule <- long_schedule |>
#     dplyr::left_join(unit_info, by = by)
#
#   print("Columns in schedule after join:")
#   print(names(schedule))
#
#   if(!is.null(uncount_unit)){
#     print("uncount_unit:")
#     print(uncount_unit)
#
#     uncount_unit_sym <- ensym(uncount_unit)
#     print("uncount_unit_sym:")
#     print(uncount_unit_sym)
#
#     print("Head of schedule:")
#     print(head(schedule))
#
#     schedule <- schedule |>
#       tidyr::uncount(weights = !!uncount_unit_sym)
#   }
#   schedule
# }

# join_unit_info <- function(long_schedule, unit_info, by = NULL, uncount_unit = NULL, .id = NULL){
#   schedule <- long_schedule |>
#     dplyr::left_join(unit_info, by = by)
#
#   print("Columns in schedule after join:")
#   print(names(schedule))
#
#   if(!is.null(uncount_unit)){
#     print(paste("Uncounting by:", uncount_unit))
#
#     schedule <- schedule |>
#       tidyr::uncount(weights = .data[[uncount_unit]])
#   }
#   schedule
# }


join_unit_info <- function(long_schedule, unit_info, by = NULL, uncount_unit = NULL, .id = NULL){
  schedule <- long_schedule |>
    dplyr::left_join(unit_info, by = by)

  uncount_unit_quo <- rlang::enquo(uncount_unit)
  if(!rlang::is_null(uncount_unit_quo)){
    schedule <- schedule |>
      tidyr::uncount(weights = rlang::eval_tidy(uncount_unit_quo, data = schedule), .id = .id)
  }

  schedule
}


initialize_replicates <- function(long_schedule, n){
  long_schedule |>
    tidyr::expand_grid(sim_sample = seq(n)) |>
    dplyr::select(sim_sample, everything())
}


# simulate_unbalanced_units <- function(design_df, var_name, var_fn){
#   design_df |>
#     dplyr::mutate("{{var_name}}" := var_fn) |>
#     tidyr::uncount({{var_name}}, .id = var_name)
# }

simulate_random_unit <- function(design_df, ...) {
  # Capture the dots
  dots <- rlang::enquos(...)

  # There should be only one named argument
  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the variable to simulate.")
  }

  # Extract the name and expression
  var_name <- names(dots)[1]
  var_expr <- dots[[1]]

  # Perform the simulation
  design_df |>
    dplyr::mutate(!!var_name := !!var_expr) |>
    tidyr::uncount(!!rlang::sym(var_name), .id = var_name)
}

add_fixed_effect <- function(design_df, ...) {
  # Capture the dots
  dots <- rlang::enquos(...)

  # There should be only one named argument
  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the variable to simulate.")
  }

  # Extract the name and expression
  var_name <- paste0(".", names(dots)[1])
  var_expr <- dots[[1]]

  # Perform the simulation
  design_df |>
    dplyr::mutate(!!var_name := !!var_expr)
}

add_random_intercept <- function(.data, variance = 1) {
  grouping_vars <- dplyr::group_vars(.data)
  intercept_name <- paste(c(".i", grouping_vars), collapse = "_")

  .data |>
    dplyr::mutate({{intercept_name}} := rnorm(1, sd = sqrt(variance)))
}

add_random_effect <- function(design_df, ..., nesting_strc) {
  # Capture the dots
  dots <- rlang::enquos(...)



  # There should be only one named argument
  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the variable to simulate.")
  }

  # Extract the name and expression
  var_name <- paste(".", names(dots)[1])
  var_expr <- dots[[1]]

  # Perform the simulation
  design_df |>
    dplyr::mutate(!!var_name := !!var_expr)
}


summarize_models <- function(.data, pop_coefs = NULL){

  results <- .data |>
    ungroup() |>
    unnest(results) |>
    filter(effect == "fixed")

  if (! is.null(pop_coefs)){

    pop_coefs <- pop_coefs |>
      as.data.frame() |>
      t() |>
      as.data.frame() |>
      rename(pop_value = V1) |>
      rownames_to_column("term")

    results <- results |>
      left_join(pop_coefs, by = "term") |>
      mutate(bias = pop_value - estimate)
  }

  results <- results |>
    group_by(term) |>
    summarise(
      power = mean(p.value < .05),
      bias = mean(bias, na.rm = TRUE)) |>
    ungroup() |>
    mutate(across(where(is.numeric), .fns = ~ round(.x, 3)))

  results
}




