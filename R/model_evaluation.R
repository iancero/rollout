#' Extract and tidy model results from a column of models
#'
#' Applies a tidying function (default `broom.mixed::tidy`) to a column of models,
#' returning a tidy data frame with one row per term per model, suitable for downstream
#' summarisation and evaluation in simulation studies.
#'
#' @param models A data frame containing a column of fitted model objects.
#' @param model_col Unquoted column name containing the models. Default is `model`.
#' @param tidy_fun A tidying function to apply to each model. Default is `broom.mixed::tidy`.
#'                 The function must return a data frame with a `term` column.
#' @param .term Optional string specifying a term to filter after tidying (e.g., `"(Intercept)"`).
#'              If `NULL` (default), all terms are retained.
#'
#' @return A tidy data frame with the original columns of `models` joined to the
#'         tidied model results, typically including columns such as `term`, `estimate`,
#'         `std.error`, `statistic`, and `p.value`.
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(broom.mixed)
#'
#' # Simulate and fit models
#' sim_models <- tibble(
#'   id = 1:5,
#'   model = map(1:5, ~ lm(mpg ~ wt, data = mtcars))
#' )
#'
#' # Extract all terms
#' extract_model_results(sim_models)
#'
#' # Extract only the slope term
#' extract_model_results(sim_models, .term = "wt")
#'
#' @export

extract_model_results <- function(
    models,
    model_col = model,
    tidy_fun = broom.mixed::tidy,
    .term = NULL
) {
  model_col <- rlang::enquo(model_col)

  results <- models |>
    dplyr::mutate(.results = purrr::map(rlang::eval_tidy(model_col, models), tidy_fun)) |>
    tidyr::unnest(.results)

  if (!is.null(.term)) {
    results <- results |>
      dplyr::filter(term == .term)
  }

  results
}

#' @export
evaluate_model_results <- function(
    results,
    alpha = 0.05,
    ...,
    .summarise_standard_broom = FALSE,
    broom_cols = c("estimate", "std.error", "statistic", "df", "p.value")
) {
  summary_exprs <- rlang::enquos(...)

  results |>
    dplyr::summarise(
      n_models = dplyr::n(),
      power = dplyr::if_else(
        condition = all(is.na(p.value)),
        true = NA_real_,
        false = mean(p.value < alpha, na.rm = TRUE)),
      !!!summary_exprs,
      !!!{
        if (.summarise_standard_broom) {
          rlang::exprs(
            dplyr::across(
              dplyr::all_of(intersect(broom_cols, names(results))),
              list(mean = mean, sd = sd),
              .names = "{fn}_{col}"
            )
          )
        } else {
          rlang::exprs()
        }
      }
    )
}



#' Summarise simulation results from extracted model estimates
#'
#' Computes summary statistics (e.g., power, custom summaries) across a set of
#' extracted model results, typically from `extract_model_results()`, to facilitate
#' simulation evaluation and reporting.
#'
#' @param results A data frame of extracted model results, typically including columns
#' like `term`, `estimate`, `std.error`, `statistic`, and `p.value`.
#' @param alpha Significance level used to compute power. Defaults to `0.05`.
#' @param ... Additional summary expressions to compute within `dplyr::summarise()`.
#'            These may include calls to helper functions like `eval_bias()`, `eval_quantile()`,
#'            or direct summaries such as `mean(estimate, na.rm = TRUE)`.
#' @param .summarise_standard_broom Logical; if `TRUE`, computes mean and standard deviation
#' for standard `broom` columns present in the data (columns in `broom_cols`).
#' Defaults to `FALSE`.
#' @param broom_cols Character vector of standard `broom` columns to summarise if
#' `.summarise_standard_broom = TRUE`. Defaults to
#' `c("estimate", "std.error", "statistic", "df", "p.value")`.
#'
#' @return A summarised data frame containing:
#' \itemize{
#'   \item `n_models`: the number of models summarised.
#'   \item `power`: the proportion of p-values less than `alpha` (NA if all p-values are NA).
#'   \item Additional columns corresponding to custom summaries provided in `...`.
#'   \item Mean and SD summaries of `broom` columns if `.summarise_standard_broom = TRUE`.
#' }
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(broom.mixed)
#'
#' # Simulate and fit models
#' sim_models <- tibble(
#'   id = 1:50,
#'   model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
#' ) |>
#'   extract_model_results()
#'
#' # Evaluate power and mean estimate for the slope
#' sim_models |>
#'   filter(term == "wt") |>
#'   evaluate_model_results(
#'     alpha = 0.05,
#'     mean_estimate = mean(estimate, na.rm = TRUE),
#'     sd_estimate = sd(estimate, na.rm = TRUE)
#'   )
#'
#' # Evaluate with .summarise_standard_broom = TRUE
#' sim_models |>
#'   filter(term == "wt") |>
#'   evaluate_model_results(
#'     .summarise_standard_broom = TRUE
#'   )
#'
#' # Evaluate with eval_bias to compute bias relative to the true value
#' # Suppose the true slope of wt is -5 (hypothetical)
#' sim_models |>
#'   filter(term == "wt") |>
#'   evaluate_model_results(
#'     bias = eval_bias(
#'       estimate,
#'       term = c("wt" = -5)
#'     )
#'   )
#'
#' @export
evaluate_model_results <- function(
    results,
    alpha = 0.05,
    ...,
    .summarise_standard_broom = FALSE,
    broom_cols = c("estimate", "std.error", "statistic", "df", "p.value")
) {
  summary_exprs <- rlang::enquos(...)

  results |>
    dplyr::summarise(
      n_models = dplyr::n(),
      power = dplyr::if_else(
        condition = all(is.na(p.value)),
        true = NA_real_,
        false = mean(p.value < alpha, na.rm = TRUE)),
      !!!summary_exprs,
      !!!{
        if (.summarise_standard_broom) {
          rlang::exprs(
            dplyr::across(
              dplyr::all_of(intersect(broom_cols, names(results))),
              list(mean = mean, sd = sd),
              .names = "{fn}_{col}"
            )
          )
        } else {
          rlang::exprs()
        }
      }
    )
}


#' Compute bias relative to term-specific true values within grouped simulation results
#'
#' Computes the mean bias (difference between estimated values and true values)
#' within each group, typically inside `evaluate_model_results()` for simulation evaluation pipelines.
#'
#' @param x A numeric vector of estimates (e.g., from a model term).
#' @param term A named numeric vector providing the true value for each term.
#' For example, `c("(Intercept)" = 0, x = 2)` to specify the true values for each term.
#' If `NULL` (default), bias is computed relative to zero.
#' @param na.rm Logical; whether to remove missing values when computing the mean bias.
#' Defaults to `FALSE`.
#'
#' @return A numeric scalar representing the mean bias within the current group.
#'
#' @details
#' This function is designed to be used inside `dplyr::summarise()` within a grouped
#' tidyverse pipeline, typically after grouping by `term`. It computes the mean of
#' `x` minus the true value for the corresponding term.
#'
#' If `term` is provided, the current grouping must include a `term` variable matching
#' the names in `term`. If a term in the group is not found in the provided `term` mapping,
#' the function will return `NA` with a warning.
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(broom.mixed)
#'
#' # Simulate and fit models
#' sim_models <- tibble(
#'   id = 1:50,
#'   model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
#' ) |>
#'   extract_model_results()
#'
#' # Compute bias relative to true value (hypothetical slope = -5)
#' sim_models |>
#'   filter(term == "wt") |>
#'   evaluate_model_results(
#'     bias = eval_bias(
#'       estimate,
#'       term = c("wt" = -5)
#'     )
#'   )
#'
#' # Compute bias relative to zero for all terms
#' sim_models |>
#'   group_by(term) |>
#'   evaluate_model_results(
#'     bias = eval_bias(estimate)
#'   )
#'
#' @export
eval_bias <- function(x, term = NULL, na.rm = FALSE) {
  if (!is.numeric(x)) {
    rlang::abort("`x` must be numeric.")
  }

  if (is.null(term)) {
    return(mean(x - 0, na.rm = na.rm))
  }

  if (!rlang::is_named(term)) {
    abort("`term` must be a named vector (e.g., c(x = 1)).")
  }

  group_vars <- dplyr::cur_group()
  if (is.null(group_vars) || length(group_vars) == 0) {
    abort("`eval_bias()` must be used inside a grouped `dplyr` context when `term` is provided.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_bias()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    rlang::warn(glue::glue("Term '{current_term}' not found in `term` mapping. Returning NA."))
    return(NA_real_)
  }

  bias <- mean(x - term[[current_term]], na.rm = na.rm)

  bias
}


#' Compute the proportion of values above term-specific thresholds within grouped simulation results
#'
#' Computes the proportion of `x` values exceeding term-specific thresholds within each group,
#' typically inside `evaluate_model_results()` for simulation evaluation pipelines.
#'
#' @param x A numeric vector of estimates or statistics.
#' @param term A named numeric vector providing the threshold for each term.
#' For example, `c("(Intercept)" = 0, x = 2)`. If `NULL` (default), threshold is assumed to be zero.
#' @param na.rm Logical; whether to remove missing values when computing the proportion.
#' Defaults to `FALSE`.
#'
#' @return A numeric scalar representing the proportion of `x` exceeding the term-specific threshold within the current group.
#'
#' @details
#' This function is designed to be used inside `dplyr::summarise()` within a grouped
#' tidyverse pipeline, typically after grouping by `term`.
#'
#' If `term` is provided, the current grouping must include a `term` variable matching
#' the names in `term`. If a term in the group is not found in the provided `term` mapping,
#' the function will return `NA` with a warning.
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(broom.mixed)
#'
#' sim_models <- tibble(
#'   id = 1:50,
#'   model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
#' ) |>
#'   extract_model_results()
#'
#' sim_models |>
#'   filter(term == "wt") |>
#'   evaluate_model_results(
#'     prop_above_0 = eval_greater_than(
#'       estimate,
#'       term = c("wt" = 0)
#'     )
#'   )
#'
#' @export
eval_greater_than <- function(x, term = NULL, na.rm = FALSE) {
  if (!is.numeric(x)) {
    abort("`x` must be numeric.")
  }

  if (is.null(term)) {
    return(mean(x > 0, na.rm = na.rm))
  }

  if (!rlang::is_named(term)) {
    abort("`term` must be a named vector (e.g., c(x = 1)).")
  }

  group_vars <- dplyr::cur_group()
  if (is.null(group_vars) || length(group_vars) == 0) {
    abort("`eval_greater()` must be used inside a grouped `dplyr` context when `term` is provided.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_greater()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    rlang::warn(glue::glue("Term '{current_term}' not found in `term` mapping. Returning NA."))
    return(NA_real_)
  }

  threshold <- term[[current_term]]
  prop <- mean(x > threshold, na.rm = na.rm)
  return(prop)
}


#' Compute the proportion of values below term-specific thresholds within grouped simulation results
#'
#' Computes the proportion of `x` values falling below term-specific thresholds within each group,
#' typically inside `evaluate_model_results()` for simulation evaluation pipelines.
#'
#' @param x A numeric vector of estimates or statistics.
#' @param term A named numeric vector providing the threshold for each term.
#' For example, `c("(Intercept)" = 0, x = 2)`. If `NULL` (default), threshold is assumed to be zero.
#' @param na.rm Logical; whether to remove missing values when computing the proportion.
#' Defaults to `FALSE`.
#'
#' @return A numeric scalar representing the proportion of `x` below the term-specific threshold within the current group.
#'
#' @details
#' This function is designed to be used inside `dplyr::summarise()` within a grouped
#' tidyverse pipeline, typically after grouping by `term`.
#'
#' If `term` is provided, the current grouping must include a `term` variable matching
#' the names in `term`. If a term in the group is not found in the provided `term` mapping,
#' the function will return `NA` with a warning.
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(broom.mixed)
#'
#' sim_models <- tibble(
#'   id = 1:50,
#'   model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
#' ) |>
#'   extract_model_results()
#'
#' sim_models |>
#'   filter(term == "wt") |>
#'   evaluate_model_results(
#'     prop_below_0 = eval_less_than(
#'       estimate,
#'       term = c("wt" = 0)
#'     )
#'   )
#'
#' @export
eval_less_than <- function(x, term = NULL, na.rm = FALSE) {
  if (!is.numeric(x)) {
    abort("`x` must be numeric.")
  }

  if (is.null(term)) {
    return(mean(x > 0, na.rm = na.rm))
  }

  if (!rlang::is_named(term)) {
    abort("`term` must be a named vector (e.g., c(x = 1)).")
  }

  group_vars <- dplyr::cur_group()
  if (is.null(group_vars) || length(group_vars) == 0) {
    abort("`eval_greater()` must be used inside a grouped `dplyr` context when `term` is provided.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_greater()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    rlang::warn(glue::glue("Term '{current_term}' not found in `term` mapping. Returning NA."))
    return(NA_real_)
  }

  threshold <- term[[current_term]]
  prop <- mean(x < threshold, na.rm = na.rm)
  return(prop)
}


#' Compute the proportion of values within term-specific intervals within grouped simulation results
#'
#' Computes the proportion of `x` values falling within term-specific intervals within each group,
#' typically inside `evaluate_model_results()` for simulation evaluation pipelines.
#'
#' @param x A numeric vector of estimates or statistics.
#' @param term A named list of numeric vectors of length 2, giving the lower and upper bounds for each term.
#' For example, `list("(Intercept)" = c(-1, 1), x = c(1, 3))`.
#' If `NULL` (default), the interval is assumed to be `[0, 1]`.
#' @param na.rm Logical; whether to remove missing values when computing the proportion.
#' Defaults to `FALSE`.
#'
#' @return A numeric scalar representing the proportion of `x` within the term-specific interval within the current group.
#'
#' @details
#' This function is designed to be used inside `dplyr::summarise()` within a grouped
#' tidyverse pipeline, typically after grouping by `term`.
#'
#' If `term` is provided, the current grouping must include a `term` variable matching
#' the names in `term`. If a term in the group is not found in the provided `term` mapping,
#' the function will return `NA` with a warning.
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(broom.mixed)
#'
#' sim_models <- tibble(
#'   id = 1:50,
#'   model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
#' ) |>
#'   extract_model_results()
#'
#' sim_models |>
#'   filter(term == "wt") |>
#'   evaluate_model_results(
#'     prop_between = eval_between(
#'       estimate,
#'       term = list("wt" = c(-1, 0))
#'     )
#'   )
#'
#' @export
eval_between <- function(x, term = NULL, na.rm = FALSE) {
  if (!is.numeric(x)) {
    abort("`x` must be numeric.")
  }

  if (is.null(term)) {
    return(mean(x >= 0 & x <= 1, na.rm = na.rm))
  }

  if (!is.list(term) || !rlang::is_named(term)) {
    abort("`term` must be a named list of numeric vectors of length 2.")
  }

  # Check that all elements of the list are numeric of length 2
  valid_intervals <- purrr::map_lgl(term, ~ is.numeric(.x) && length(.x) == 2)
  if (!all(valid_intervals)) {
    abort("Each element of `term` must be a numeric vector of length 2 (lower and upper bounds).")
  }

  group_vars <- dplyr::cur_group()
  if (is.null(group_vars) || length(group_vars) == 0) {
    abort("`eval_between()` must be used inside a grouped `dplyr` context when `term` is provided.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_between()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    rlang::warn(glue::glue("Term '{current_term}' not found in `term` mapping. Returning NA."))
    return(NA_real_)
  }

  bounds <- term[[current_term]]
  lower <- bounds[1]
  upper <- bounds[2]

  prop <- mean(x >= lower & x <= upper, na.rm = na.rm)
  return(prop)
}

#' Compute the observed quantile value for each term within grouped simulation results
#'
#' Computes the specified quantile of `x` within each group, typically inside
#' `evaluate_model_results()` for simulation evaluation pipelines.
#'
#' @param x A numeric vector of estimates or statistics.
#' @param term A named numeric vector with quantile probabilities for each term.
#' For example, `c("(Intercept)" = 0.05, x = 0.95)`. If `NULL` (default), computes the median (0.5).
#' @param na.rm Logical; whether to remove missing values when computing the quantile.
#' Defaults to `FALSE`.
#'
#' @return A numeric scalar representing the observed quantile of `x` within the current group.
#'
#' @details
#' This function is designed to be used inside `dplyr::summarise()` within a grouped
#' tidyverse pipeline, typically after grouping by `term`.
#'
#' If `term` is provided, the current grouping must include a `term` variable matching
#' the names in `term`. If a term in the group is not found in the provided `term` mapping,
#' the function will return `NA` with a warning.
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' library(broom.mixed)
#'
#' sim_models <- tibble(
#'   id = 1:50,
#'   model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
#' ) |>
#'   extract_model_results()
#'
#' sim_models |>
#'   filter(term == "wt") |>
#'   evaluate_model_results(
#'     lower_quantile = eval_quantile(
#'       estimate,
#'       term = c("wt" = 0.05)
#'     ),
#'     upper_quantile = eval_quantile(
#'       estimate,
#'       term = c("wt" = 0.95)
#'     )
#'   )
#'
#' @export
eval_quantile <- function(x, term = NULL, na.rm = FALSE) {
  if (!is.numeric(x)) {
    abort("`x` must be numeric.")
  }

  if (is.null(term)) {
    return(stats::quantile(x, probs = 0.5, na.rm = na.rm, names = FALSE, type = 7))
  }

  if (!rlang::is_named(term) || !is.numeric(term)) {
    abort("`term` must be a named numeric vector of quantile probabilities (e.g., c(x = 0.05)).")
  }

  # Check that each element is a single numeric value within [0, 1]
  valid_probs <- purrr::map_lgl(term, ~ is.numeric(.x) && length(.x) == 1 && .x >= 0 && .x <= 1)
  if (!all(valid_probs)) {
    abort("Each element of `term` must be a single numeric quantile value between 0 and 1.")
  }

  group_vars <- dplyr::cur_group()
  if (is.null(group_vars) || length(group_vars) == 0) {
    abort("`eval_quantile()` must be used inside a grouped `dplyr` context when `term` is provided.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_quantile()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    rlang::warn(glue::glue("Term '{current_term}' not found in `term` mapping. Returning NA."))
    return(NA_real_)
  }

  prob <- term[[current_term]]
  q <- stats::quantile(x, probs = prob, na.rm = na.rm, names = FALSE, type = 7)
  return(q)
}
