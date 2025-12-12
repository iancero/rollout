#' Add a fixed effect column for simulation
#'
#' Adds a fixed effect column (prefixed with `"."`) to the design data frame for simulation purposes.
#'
#' @param design_df A data frame containing the rollout design and any parameters.
#' @param ... A single named expression specifying the fixed effect to add (e.g., `beta = 0.5 * x`).
#'
#' @return A `tibble` with the added fixed effect column.
#' @examples
#' df <- tibble::tibble(x = rnorm(5))
#' add_fixed_effect(df, beta = 0.5 * x)
#' @export
add_fixed_effect <- function(design_df, ...) {
  dots <- rlang::enquos(...)

  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the fixed effect to add.")
  }

  var_name <- paste0(".", names(dots)[1])
  var_expr <- dots[[1]]

  design_df |>
    dplyr::mutate(!!var_name := !!var_expr)
}


#' Add a random effect column for simulation
#'
#' Adds a random effect column (prefixed with `"."`) to the design data frame, with optional grouping for nested random effects.
#'
#' @param design_df A data frame containing the rollout design and any parameters.
#' @param ... A single named expression specifying the random effect to add (e.g., `u = rnorm(1, 0, 1)`).
#' @param .nesting Optional character vector specifying grouping columns for nested random effects (default `NULL`).
#'
#' @return A `tibble` with the added random effect column.
#' @examples
#' df <- tibble::tibble(site = rep(1:2, each = 3))
#' add_random_effect(df, u = rnorm(1, 0, 1), .nesting = "site")
#' @export
add_random_effect <- function(design_df, ..., .nesting = NULL) {
  dots <- rlang::enquos(...)

  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the random effect to add.")
  }

  var_name <- paste0(".", names(dots)[1])
  var_expr <- dots[[1]]

  # Save original grouping
  original_groups <- dplyr::group_vars(design_df)

  # Apply nesting groups temporarily (if any)
  if (!is.null(.nesting)) {
    design_df <- design_df |>
      dplyr::group_by(dplyr::across(all_of(.nesting)))
  }

  # Add random effect and restore original grouping
  design_df <- design_df |>
    dplyr::mutate(!!var_name := !!var_expr)

  # Restore original grouping
  if (length(original_groups) > 0) {
    design_df <- design_df |>
      dplyr::group_by(dplyr::across(all_of(original_groups)))
  } else {
    design_df <- design_df |> dplyr::ungroup()
  }

  design_df
}


#' Add an error term for simulation
#'
#' Adds a residual error term (column `.error`) to the data frame, drawn from a normal distribution with specified variance.
#'
#' @param .data A data frame to which the error term will be added.
#' @param variance Numeric; variance of the residual error (default `1`).
#'
#' @return A `tibble` with an added `.error` column.
#' @examples
#' df <- tibble::tibble(x = 1:5)
#' add_error(df, variance = 2)
#' @export
add_error <- function(.data, variance = 1) {
  variance <- rlang::enquo(variance)  # capture variance expression

  # Save original grouping
  original_groups <- dplyr::group_vars(.data)

  .data <- .data |>
    dplyr::ungroup() |>
    dplyr::mutate(.error = rnorm(n(), sd = sqrt(!!variance)))

  # Restore original grouping
  if (length(original_groups) > 0) {
    .data <- .data |>
      dplyr::group_by(dplyr::across(all_of(original_groups)))
  }

  .data
}


#' Create a linear outcome by summing effects
#'
#' Generates a linear outcome variable by summing all columns that start with `"."` (representing fixed, random, and error effects).
#'
#' @param data A data frame containing effect columns prefixed with `"."`.
#' @param output_col Name of the column to store the linear outcome (default `"y_linear"`).
#'
#' @return A `tibble` with the added linear outcome column.
#' @examples
#' df <- tibble::tibble(.beta = 0.5, .u = rnorm(5), .error = rnorm(5))
#' add_linear_outcome(df)
#' @export
add_linear_outcome <- function(data, output_col = "y_linear") {
  dot_cols <- names(data)[startsWith(names(data), ".")]

  if (length(dot_cols) == 0) {
    stop("No effect columns found (no columns starting with '.')")
  }

  data |>
    dplyr::mutate(!!output_col := rowSums(dplyr::pick(all_of(dot_cols))))
}


#' Create a binary outcome from linear predictors
#'
#' Generates a binary outcome by summing effects, computing probabilities via the logistic function, and drawing binary outcomes.
#'
#' @param data A data frame containing effect columns prefixed with `"."`.
#' @param linear_col Name of the column to store the summed linear predictor (default `"y_linear"`).
#' @param prob_col Name of the column to store probabilities (default `"y_prob"`).
#' @param binary_col Name of the column to store binary outcomes (default `"y_bin"`).
#'
#' @return A `tibble` with added linear predictor, probability, and binary outcome columns.
#' @examples
#' df <- tibble::tibble(.beta = 0.5, .u = rnorm(5), .error = rnorm(5))
#' add_binary_outcome(df)
#' @export
add_binary_outcome <- function(data,
                               linear_col = "y_linear",
                               prob_col = "y_prob",
                               binary_col = "y_bin") {

  dot_cols <- names(data)[startsWith(names(data), ".")]

  if (length(dot_cols) == 0) {
    stop("No effect columns found (no columns starting with '.')")
  }

  data |>
    dplyr::mutate(
      !!linear_col := rowSums(dplyr::pick(all_of(dot_cols))),
      !!prob_col   := plogis(.data[[linear_col]]),
      !!binary_col := rbinom(dplyr::n(), size = 1, prob = .data[[prob_col]])
    )
}


#' Create a Poisson outcome from linear predictors
#'
#' Generates a Poisson-distributed count outcome by summing effects, exponentiating to obtain rates, and drawing counts.
#'
#' @param data A data frame containing effect columns prefixed with `"."`.
#' @param linear_col Name of the column to store the summed linear predictor (default `"y_linear"`).
#' @param rate_col Name of the column to store Poisson rates (default `"y_rate"`).
#' @param count_col Name of the column to store Poisson counts (default `"y_count"`).
#'
#' @return A `tibble` with added linear predictor, rate, and count columns.
#' @examples
#' df <- tibble::tibble(.beta = 0.5, .u = rnorm(5), .error = rnorm(5))
#' add_poisson_outcome(df)
#' @export
add_poisson_outcome <- function(data,
                                linear_col = "y_linear",
                                rate_col = "y_rate",
                                count_col = "y_count") {

  dot_cols <- names(data)[startsWith(names(data), ".")]
  if (length(dot_cols) == 0) stop("No effect columns found.")

  data |>
    dplyr::mutate(
      !!linear_col := rowSums(dplyr::pick(all_of(dot_cols))),
      !!rate_col := exp(.data[[linear_col]]),
      !!count_col := rpois(dplyr::n(), lambda = .data[[rate_col]])
    )
}




