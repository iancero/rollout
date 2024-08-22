#' Simulate Random Unit
#'
#' This function simulates a random variable for each unit in the design dataframe.
#'
#' @param design_df A dataframe containing the design structure
#' @param ... A single named argument specifying the variable to simulate and its distribution
#'
#' @return A dataframe with the new simulated variable added
#' @export
#'
#' @examples
#' design_df <- data.frame(cluster = rep(1:5, each = 10))
#' simulate_random_unit(design_df, patients = rpois(n(), lambda = 5))
simulate_random_unit <- function(design_df, ...) {
  dots <- rlang::enquos(...)

  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the variable to simulate.")
  }

  var_name <- names(dots)[1]
  var_expr <- dots[[1]]

  design_df %>%
    dplyr::mutate(!!var_name := !!var_expr) %>%
    tidyr::uncount(!!rlang::sym(var_name), .id = var_name)
}

#' Add Fixed Effect
#'
#' This function adds a fixed effect to the design dataframe.
#'
#' @param design_df A dataframe containing the design structure
#' @param ... A single named argument specifying the fixed effect to add
#'
#' @return A dataframe with the new fixed effect added
#' @export
#'
#' @examples
#' design_df <- data.frame(time = 1:10)
#' add_fixed_effect(design_df, time_effect = 0.1 * time)
add_fixed_effect <- function(design_df, ...) {
  dots <- rlang::enquos(...)

  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the fixed effect to add.")
  }

  var_name <- paste0(".", names(dots)[1])
  var_expr <- dots[[1]]

  design_df %>%
    dplyr::mutate(!!var_name := !!var_expr)
}

#' Add Random Intercept
#'
#' This function adds a random intercept to the design dataframe.
#'
#' @param .data A dataframe containing the design structure
#' @param variance The variance of the random intercept
#'
#' @return A dataframe with the random intercept added
#' @export
#'
#' @examples
#' design_df <- data.frame(cluster = rep(1:5, each = 10))
#' add_random_intercept(design_df, variance = 1)
add_random_intercept <- function(.data, variance = 1) {
  grouping_vars <- dplyr::group_vars(.data)
  intercept_name <- paste(c(".i", grouping_vars), collapse = "_")

  .data %>%
    dplyr::mutate({{intercept_name}} := rnorm(1, sd = sqrt(variance)))
}

#' Add Random Effect
#'
#' This function adds a random effect to the design dataframe.
#'
#' @param design_df A dataframe containing the design structure
#' @param ... A single named argument specifying the random effect to add
#' @param nesting_strc The nesting structure for the random effect
#'
#' @return A dataframe with the new random effect added
#' @export
#'
#' @examples
#' design_df <- data.frame(cluster = rep(1:5, each = 10), time = rep(1:10, 5))
#' add_random_effect(design_df, time_effect = rnorm(n(), sd = 0.1), nesting_strc = "cluster")
add_random_effect <- function(design_df, ..., nesting_strc) {
  dots <- rlang::enquos(...)

  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the random effect to add.")
  }

  var_name <- paste0(".", names(dots)[1])
  var_expr <- dots[[1]]

  design_df %>%
    dplyr::mutate(!!var_name := !!var_expr)
}

#' Add Error Term
#'
#' This function adds a random error term to the design dataframe.
#'
#' @param .data A dataframe containing the design structure
#' @param variance The variance of the error term
#'
#' @return A dataframe with the error term added
#' @export
#'
#' @examples
#' design_df <- data.frame(cluster = rep(1:5, each = 10))
#' add_error(design_df, variance = 0.5)
add_error <- function(.data, variance = 1) {
  .data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.error = rnorm(n(), sd = sqrt(variance)))
}
