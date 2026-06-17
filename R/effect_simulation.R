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
      dplyr::group_by(dplyr::across(tidyr::all_of(.nesting)))
  }

  # Add random effect and restore original grouping
  design_df <- design_df |>
    dplyr::mutate(!!var_name := !!var_expr)

  # Restore original grouping
  if (length(original_groups) > 0) {
    design_df <- design_df |>
      dplyr::group_by(dplyr::across(tidyr::all_of(original_groups)))
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
    dplyr::mutate(.error = stats::rnorm(dplyr::n(), sd = sqrt(!!variance)))

  # Restore original grouping
  if (length(original_groups) > 0) {
    .data <- .data |>
      dplyr::group_by(dplyr::across(tidyr::all_of(original_groups)))
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
    dplyr::mutate(!!output_col := rowSums(dplyr::pick(tidyr::all_of(dot_cols))))
}


#' Create a binomial outcome from linear predictors
#'
#' Generates a binomial outcome by summing effects, computing probabilities via the logistic
#' function, and drawing a count out of a specified number of trials. This generalizes
#' [add_binary_outcome()] (which is a `size = 1` special case of this function) to support a
#' denominator greater than 1 — for example, a "Reach" outcome where `size` is the number of
#' people eligible at a site-period and the resulting count is the number who received an
#' intervention.
#'
#' @param data A data frame containing effect columns prefixed with `"."`.
#' @param size The number of trials for the binomial draw (the denominator). Either a single
#'   fixed value (e.g. `size = 10`) or the bare name of a column in `data` holding a per-row
#'   value (e.g. `size = n_eligible`).
#' @param linear_col Name of the column to store the summed linear predictor (default `"y_linear"`).
#' @param prob_col Name of the column to store probabilities (default `"y_prob"`).
#' @param binom_col Name of the column to store binomial counts (default `"y_binom"`).
#' @param include_error Logical; whether to include a `.error` column (if present) in the summed
#'   linear predictor (default `FALSE`). A binomial outcome's variance is already implied by `p`
#'   and `size`, so an additional individual-level residual error term is usually not part of the
#'   intended generative model. Set to `TRUE` to instead sum every `.`-prefixed column, including
#'   `.error`.
#'
#' @return A `tibble` with added linear predictor, probability, and binomial count columns.
#' @examples
#' df <- tibble::tibble(.beta = 0.5, .u = rnorm(5))
#'
#' # Fixed number of trials
#' add_binomial_outcome(df, size = 10)
#'
#' # Per-row number of trials drawn from a column (e.g. an eligible count)
#' df2 <- tibble::tibble(.beta = 0.5, .u = rnorm(5), n_eligible = c(8, 12, 9, 15, 10))
#' add_binomial_outcome(df2, size = n_eligible)
#' @export
add_binomial_outcome <- function(data,
                                 size,
                                 linear_col = "y_linear",
                                 prob_col = "y_prob",
                                 binom_col = "y_binom",
                                 include_error = FALSE) {

  dot_cols <- names(data)[startsWith(names(data), ".")]

  if (!include_error) {
    dot_cols <- dot_cols[dot_cols != ".error"]
  }

  if (length(dot_cols) == 0) {
    stop("No effect columns found (no columns starting with '.')")
  }

  data |>
    dplyr::mutate(
      !!linear_col := rowSums(dplyr::pick(tidyr::all_of(dot_cols))),
      !!prob_col   := stats::plogis(.data[[linear_col]]),
      !!binom_col  := stats::rbinom(dplyr::n(), size = {{ size }}, prob = .data[[prob_col]])
    )
}


#' Create a binary outcome from linear predictors
#'
#' Generates a binary outcome by summing effects, computing probabilities via the logistic
#' function, and drawing binary outcomes. This is a thin wrapper around [add_binomial_outcome()]
#' with `size = 1`.
#'
#' @param data A data frame containing effect columns prefixed with `"."`.
#' @param linear_col Name of the column to store the summed linear predictor (default `"y_linear"`).
#' @param prob_col Name of the column to store probabilities (default `"y_prob"`).
#' @param binary_col Name of the column to store binary outcomes (default `"y_binary"`).
#' @param include_error Logical; whether to include a `.error` column (if present) in the summed
#'   linear predictor (default `FALSE`). A Bernoulli outcome's variance is already implied by `p`,
#'   so an additional individual-level residual error term is usually not part of the intended
#'   generative model. Set to `TRUE` to instead sum every `.`-prefixed column, including `.error`,
#'   as earlier versions of this function always did.
#'
#' @return A `tibble` with added linear predictor, probability, and binary outcome columns.
#' @examples
#' df <- tibble::tibble(.beta = 0.5, .u = rnorm(5), .error = rnorm(5))
#'
#' # By default, .error is excluded from the linear predictor
#' add_binary_outcome(df)
#'
#' # Include .error in the sum if that's really what you want
#' add_binary_outcome(df, include_error = TRUE)
#' @export
add_binary_outcome <- function(data,
                               linear_col = "y_linear",
                               prob_col = "y_prob",
                               binary_col = "y_binary",
                               include_error = FALSE) {

  add_binomial_outcome(
    data,
    size = 1,
    linear_col = linear_col,
    prob_col = prob_col,
    binom_col = binary_col,
    include_error = include_error
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
      !!linear_col := rowSums(dplyr::pick(tidyr::all_of(dot_cols))),
      !!rate_col := exp(.data[[linear_col]]),
      !!count_col := stats::rpois(dplyr::n(), lambda = .data[[rate_col]])
    )
}

#' Create a two-step Poisson-binomial outcome
#'
#' Generates the total number of sampled units from a Poisson distribution,
#' then generates the number of positive outcomes from a binomial distribution.
#' The binomial probability is computed from the summed effect columns using
#' the logistic function.
#'
#' @param data A data frame containing effect columns prefixed with `"."`.
#' @param mean_n Mean of the Poisson distribution used to generate the total
#'   number of sampled units.
#' @param linear_col Name of the column to store the summed linear predictor
#'   (default `"y_linear"`).
#' @param prob_col Name of the column to store probabilities
#'   (default `"y_prob"`).
#' @param total_col Name of the column to store the Poisson-sampled totals
#'   (default `"y_n_total"`).
#' @param positive_col Name of the column to store the binomial positive counts
#'   (default `"y_n_positive"`).
#' @param include_error Logical indicating whether the `.error` column should
#'   be included in the linear predictor. Defaults to `FALSE`.
#'
#' @return A `tibble` with added linear predictor, probability, total-count,
#'   and positive-count columns.
#' @examples
#' df <- tibble::tibble(
#'   .beta = 0.5,
#'   .u = stats::rnorm(5),
#'   .error = stats::rnorm(5)
#' )
#'
#' add_poisson_binomial_outcome(df, mean_n = 20)
#' @export
add_poisson_binomial_outcome <- function(
    data,
    mean_n,
    linear_col = "y_linear",
    prob_col = "y_prob",
    total_col = "y_n_total",
    positive_col = "y_n_positive",
    include_error = FALSE) {

  if (!is.numeric(mean_n) ||
      length(mean_n) != 1L ||
      is.na(mean_n) ||
      mean_n < 0) {
    stop(
      "`mean_n` must be a single non-negative numeric value.",
      call. = FALSE
    )
  }

  if (!is.logical(include_error) ||
      length(include_error) != 1L ||
      is.na(include_error)) {
    stop(
      "`include_error` must be either TRUE or FALSE.",
      call. = FALSE
    )
  }

  dot_cols <- names(data)[startsWith(names(data), ".")]

  if (!include_error) {
    dot_cols <- setdiff(dot_cols, ".error")
  }

  if (length(dot_cols) == 0L) {
    stop(
      "No effect columns found after applying the `include_error` setting.",
      call. = FALSE
    )
  }

  data |>
    dplyr::mutate(
      !!linear_col := rowSums(
        dplyr::pick(tidyr::all_of(dot_cols))
      ),
      !!prob_col := stats::plogis(.data[[linear_col]]),
      !!total_col := stats::rpois(
        dplyr::n(),
        lambda = mean_n
      ),
      !!positive_col := stats::rbinom(
        dplyr::n(),
        size = .data[[total_col]],
        prob = .data[[prob_col]]
      )
    )
}



