stepped_wedge_crossover <- function(cohort_indices, init_baseline, step_duration){
  step_duration*(cohort_indices - 1) + init_baseline + 1
}


assign_stepped_wedge <- function(.data, step_duration = 1, init_baseline = 1, condition_names = c("a", "b"), drop = TRUE) {

  .data <- .data |>
    mutate(
      cur_id = cur_group_id(),
      .crossover_time = stepped_wedge_crossover(
        cohort_indices = group_indices(),
        init_baseline = init_baseline,
        step_duration = step_duration),
      .condition = if_else(
        condition = time < .crossover_time,
        true = condition_names[1],
        false = condition_names[2]) |>
        factor(levels = condition_names, ordered = TRUE)
    )

  if(drop){
    .data <- .data |>
      select(-.crossover_time)
  }

  .data
}
