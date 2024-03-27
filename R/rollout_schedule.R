cur_phase <- function(cohort, time, step_dur = 1, phase_durs = c(2, 3, Inf), use_names = TRUE){
  offset <- step_dur * (cohort - 1)
  crossovers <- offset + cumsum(phase_durs)

  phase <- findInterval(time, vec = crossovers, left.open = TRUE) + 1

  if(use_names & !is.null(names(phase_durs))){
    phase <- names(phase_durs)[phase]
  }

  phase
}

stepped_wedge_crossover <- function(site_indices, init_baseline, step_duration) {
  step_duration * (site_indices - 1) + init_baseline + 1
}


assign_stepped_wedge <- function(.data, step_duration = 1, init_baseline = 1, condition_names = c("a", "b"), drop = TRUE) {
  .data <- .data |>
    mutate(
      .crossover_time = stepped_wedge_crossover(
        site_indices = group_indices(),
        init_baseline = init_baseline,
        step_duration = step_duration
      ),
      .condition = if_else(
        condition = time < .crossover_time,
        true = condition_names[1],
        false = condition_names[2]
      ) |>
        factor(levels = condition_names)
    )

  if (drop) {
    .data <- .data |>
      select(-.crossover_time)
  }

  .data
}


head2head_cohort <- function(site_indices) {
  (site_indices - 1) %/% 2 + 1
}

head2head_block <- function(site_indices) {
  (site_indices - 1) %% 2 + 1
}

assign_head2head <- function(.data, condition_names = list("a", c("b", "c")), step_duration = 1, init_baseline = 1) {
  grouping_vars <- groups(.data)

  .data |>
    mutate(
      .cohort = head2head_cohort(cur_group_id()),
      .block = head2head_block(cur_group_id())
    ) |>
    group_by(.cohort) |>
    assign_stepped_wedge(
      init_baseline = init_baseline,
      step_duration = step_duration,
      drop = FALSE
    ) |>
    mutate(
      .condition = if_else(
        condition = time < .crossover_time,
        true = condition_names[[1]],
        false = condition_names[[2]][.block]
      ) |>
        factor(levels = unlist(condition_names))
    ) |>
    group_by(!!!grouping_vars)
}


assign_single_wedge <- function(.data, step_duration = 1, condition_name = "a", drop = TRUE, prestudy_missing = FALSE) {
  .data <- .data |>
    assign_stepped_wedge(
      step_duration = step_duration,
      init_baseline = 0,
      condition_names = c(".prestudy", condition_name),
      drop = FALSE) |>
    mutate(.condition = if_else(
      condition = .condition == ".prestudy",
      true = NA_character_,
      false = .condition))

  if (!prestudy_missing){
    .data <- .data |>
      dplyr::filter(time >= .crossover_time)
  }

  if (drop) {
    .data <- .data |>
      select(-.crossover_time)
  }

  .data
}
