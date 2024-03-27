
#' Add normally-distributed error to a dataset
#'
#' This function adds a column of normally distributed error to a dataset and is
#' designed for the simulation of data for a rollout design.
#'
#' @param .data A dataframe with simulated rollout data
#' @param sd The standard deviation of the error term
#'
#' @details
#' This function is intended to be used during the simulation of a rollout-based
#' dataset. The error term is normally distributed with a mean of 0 and a standard
#' deviation of 1 (by default).
#'
#'
#' @return The original \code{.data} dataframe, with a new column called
#' \code{.error} appended to it.
#'
#' @examples
#' mtcars |>
#'  sim_normal_error()
#'
#' mtcars |>
#'  sim_normal_error(sd = 2)
#'
#' @export
sim_normal_error <- function(.data, sd = 1){
  .data |>
    dplyr::mutate(.error = rnorm(n = dplyr::n(), mean = 0, sd = sd))
}
