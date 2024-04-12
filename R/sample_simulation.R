#' @export
simulate_rollout_samples <- function(params, n) {

  total_study_time <- sum(unlist(params$phase_durations))

  # Initialize data frame
  df <- tidyr::expand_grid(
      .cohort = seq(params$cohorts),
      .time = seq(total_study_time)) |>
    assign_rollout_phases(.cohort, .time, params$phase_durations) |>
    tidyr::expand_grid(
      .sample = seq(n),
      .subject = seq(params$subjects_per_cohort))

  # Assign phase effects
  df <- df |>
    dplyr::mutate(phase_score = purrr::map_dbl(phase, ~ params[["phase_effects"]][[.x]]))

  # Assign random intercepts for cohorts
  df <- df |>
    dplyr::group_by(.cohort) |>
    dplyr::mutate(cohort_score = rnorm(n = 1, sd = 1))

  # Assign random intercept for subjects
  df <- df |>
    dplyr::group_by(.cohort, .subject) |>
    dplyr::mutate(subject_score = rnorm(n = 1, sd = 1))

  # Assign random error for all observations
  df <- df |>
    dplyr::ungroup() |>
    dplyr::mutate(error = rnorm(n = dplyr::n(), sd = 1))

  # Assign y for all observations
  df <- df |>
    dplyr::mutate(y = phase_score + cohort_score + subject_score + error)

  df
}
