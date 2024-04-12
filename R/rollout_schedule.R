phase_start_offset <- function(cohort, step_dur = 1) {
  step_dur * (cohort - 1)
}

phase_transitions <- function(phase_durs, initial_offset = 0) {
  initial_offset + cumsum(phase_durs)
}

extract_phase_block <- function(phase, block) {
  if (length(phase) > 1) phase[block] else phase
}

extract_block_schedule <- function(phase_durs, block) {
  phase_durs |>
    purrr::map(~ extract_phase_block(.x, block)) |>
    purrr::flatten()
}

phase_interval_num <- function(time, transition_times) {
  findInterval(time, vec = transition_times, left.open = TRUE) + 1
}

cur_phase <- function(
    time,
    cohort,
    block = NULL,
    step_dur = 1,
    phase_durs = c(step_dur, Inf),
    use_names = TRUE,
    extend_first_phase = TRUE,
    extend_last_phase = TRUE) {

  initial_offset <- phase_start_offset(cohort, step_dur)

  # if time is before the first phase technically starts, return NA
  if (!extend_first_phase & time <= initial_offset) {
    return(NA)
  }

  if (is.null(block)) {
    block <- 1
  }
  block_specific_phases <- extract_block_schedule(phase_durs, block)

  transition_times <- phase_transitions(block_specific_phases, initial_offset)

  phase <- phase_interval_num(time, transition_times)

  # if time is after the last phase technically ends, return NA
  # otherwise just extend the last phase
  if (!extend_last_phase & length(block_specific_phases) < phase) {
    return(NA)
  }

  phase <- min(phase, length(block_specific_phases))

  if (use_names & !is.null(names(phase_durs))) {
    phase <- names(phase_durs)[phase]
  }

  phase
}
cur_phase <- Vectorize(cur_phase, vectorize.args = c("time"))

assign_rollout_phases <- function(
    .data,
    cohort,
    time,
    phase_durs,
    step_dur = 1,
    extend_first_phase = TRUE,
    extend_last_phase = TRUE,
    use_names = TRUE,
    block = NULL) {

  initial_groups <- dplyr::groups(.data)

  .data <- .data |>
    dplyr::group_by({{ cohort }}) |>
    dplyr::mutate(
      phase = cur_phase(
        time = {{ time }},
        cohort = dplyr::cur_group_id(),
        block = {{ block }},
        step_dur = step_dur,
        phase_durs = phase_durs,
        use_names = use_names,
        extend_first_phase = extend_first_phase,
        extend_last_phase = extend_last_phase
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(phase = factor(phase, levels = unique(phase)))

  .data <- .data |>
    dplyr::group_by(!!!initial_groups)

  .data
}
