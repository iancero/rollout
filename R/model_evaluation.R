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
    model_col = "model",
    tidy_fun = broom.mixed::tidy,
    .term = NULL
) {
  model_col <- rlang::ensym(model_col)

  results <- models |>
    dplyr::mutate(
      .results = purrr::map(
        .x = rlang::eval_tidy(model_col, data = dplyr::pick(dplyr::everything())),
        .f = tidy_fun
      )
    ) |>
    tidyr::unnest(cols = ".results")

  if (!is.null(.term)) {
    # Avoid `.data` / NSE entirely here to prevent R CMD check masking issues
    results <- results[results[["term"]] == .term, , drop = FALSE]
  }

  results
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
#'            The `term` argument of the `eval_*()` helpers may reference grouping variables
#'            (e.g., `eval_bias(estimate, term = c(conditionimpl = beta))` when grouped by
#'            `beta`), so that true values can vary across simulated parameter conditions.
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
#'   group_by(term) |>
#'   evaluate_model_results(
#'     alpha = 0.05,
#'     mean_estimate = mean(estimate, na.rm = TRUE),
#'     sd_estimate = sd(estimate, na.rm = TRUE)
#'   )
#'
#' # Evaluate with .summarise_standard_broom = TRUE
#' sim_models |>
#'   filter(term == "wt") |>
#'   group_by(term) |>
#'   evaluate_model_results(
#'     .summarise_standard_broom = TRUE
#'   )
#'
#' # Evaluate with eval_bias to compute bias relative to the true value
#' # Suppose the true slope of wt is -5 (hypothetical)
#' sim_models |>
#'   filter(term == "wt") |>
#'   group_by(term) |>
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
      mean_estimate = dplyr::if_else(
        all(is.na(p.value)),
        NA_real_,
        mean(estimate, na.rm = TRUE)
      ),
      mean_std.error = dplyr::if_else(
        all(is.na(p.value)),
        NA_real_,
        mean(std.error, na.rm = TRUE)
      ),
      power = dplyr::if_else(
        all(is.na(p.value)),
        NA_real_,
        mean(p.value < alpha, na.rm = TRUE)
      ),
      !!!summary_exprs,
      !!!{
        if (.summarise_standard_broom) {
          rlang::exprs(
            dplyr::across(
              dplyr::all_of(intersect(broom_cols, names(results))),
              list(mean = base::mean, sd = stats::sd),
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
#' Values may be numeric literals or expressions that reference grouping variables
#' (e.g., `c(conditionimpl = beta)` when the results are grouped by `beta`), allowing
#' the true value to vary across simulated parameter conditions. Each element must
#' resolve to a single value within the current group.
#' @param warnings Should warnings be returned?
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
#'   group_by(term) |>
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
#' # True values may reference grouping variables, allowing them to vary
#' # across simulated parameter conditions (here, a different true effect
#' # for each value of `beta`):
#' sim_grid <- tidyr::expand_grid(
#'   beta = c(0.35, 0.65),
#'   term = "conditionimpl",
#'   rep = 1:20
#' ) |>
#'   mutate(estimate = beta + rnorm(40, sd = 0.05))
#'
#' sim_grid |>
#'   group_by(beta, term) |>
#'   summarise(
#'     bias = eval_bias(estimate, term = c(conditionimpl = beta)),
#'     .groups = "drop"
#'   )
#'
#' @export
eval_bias <- function(x, term = NULL, na.rm = FALSE, warnings = TRUE) {
  if (!is.numeric(x)) {
    rlang::abort("`x` must be numeric.")
  }

  term <- resolve_term_arg(rlang::enquo(term), fn_name = "eval_bias")

  if (is.null(term)) {
    return(mean(x - 0, na.rm = na.rm))
  }

  group_vars <- tryCatch(dplyr::cur_group(), error = function(e) NULL)
  if (is.null(group_vars) || length(group_vars) == 0) {
    abort("`eval_bias()` must be used inside a grouped `dplyr` context when `term` is provided.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_bias()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    # if (warnings) rlang::warn(glue::glue("Term '{current_term}' not found in `term` mapping. Returning NA."))
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
#' Thresholds may be numeric literals or expressions that reference grouping variables
#' (e.g., `c(conditionimpl = beta)` when the results are grouped by `beta`). Each element
#' must resolve to a single value within the current group.
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
#'   group_by(term) |>
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

  term <- resolve_term_arg(rlang::enquo(term), fn_name = "eval_greater_than")

  if (is.null(term)) {
    return(mean(x > 0, na.rm = na.rm))
  }

  group_vars <- tryCatch(dplyr::cur_group(), error = function(e) NULL)
  if (is.null(group_vars) || length(group_vars) == 0) {
    abort("`eval_greater_than()` must be used inside a grouped `dplyr` context when `term` is provided.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_greater_than()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    # rlang::warn(glue::glue("Term '{current_term}' not found in `term` mapping. Returning NA."))
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
#' Thresholds may be numeric literals or expressions that reference grouping variables
#' (e.g., `c(conditionimpl = beta)` when the results are grouped by `beta`). Each element
#' must resolve to a single value within the current group.
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
#'   group_by(term) |>
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

  term <- resolve_term_arg(rlang::enquo(term), fn_name = "eval_less_than")

  if (is.null(term)) {
    return(mean(x < 0, na.rm = na.rm))
  }

  group_vars <- tryCatch(dplyr::cur_group(), error = function(e) NULL)
  if (is.null(group_vars) || length(group_vars) == 0) {
    abort("`eval_less_than()` must be used inside a grouped `dplyr` context when `term` is provided.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_less_than()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    # rlang::warn(glue::glue("Term '{current_term}' not found in `term` mapping. Returning NA."))
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
#' Bounds may be numeric literals or expressions that reference grouping variables
#' (e.g., `list(conditionimpl = c(beta - 0.1, beta + 0.1))` when the results are grouped
#' by `beta`). Each element must resolve to a length-2 vector within the current group.
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
#'   group_by(term) |>
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

  term <- resolve_term_arg(rlang::enquo(term), fn_name = "eval_between", validate = FALSE)

  if (is.null(term)) {
    return(mean(x >= 0 & x <= 1, na.rm = na.rm))
  }

  # Allow either:
  # 1) a named list of length-2 numeric vectors (per-term bounds)
  # 2) a single numeric vector of length 2 (global bounds)
  if (is.numeric(term) && length(term) == 2 && !rlang::is_named(term)) {
    lower <- term[1]
    upper <- term[2]
    return(mean(x >= lower & x <= upper, na.rm = na.rm))
  }

  if (!is.list(term) || !rlang::is_named(term)) {
    abort("`term` must be a named list of numeric vectors of length 2, or a numeric vector length 2.")
  }

  valid_intervals <- purrr::map_lgl(term, ~ is.numeric(.x) && length(.x) == 2)
  if (!all(valid_intervals)) {
    abort(paste(
      "Each element of `term` must resolve to a numeric vector of length 2 (lower and upper bounds).",
      "If a bound references a data column, add that column to `group_by()` so it resolves to a",
      "single value per group."
    ))
  }

  group_vars <- tryCatch(dplyr::cur_group(), error = function(e) NULL)

  # If ungrouped, allow only the single-term mapping case
  if (is.null(group_vars) || length(group_vars) == 0) {
    if (length(term) == 1) {
      bounds <- term[[1]]
      lower <- bounds[1]
      upper <- bounds[2]
      return(mean(x >= lower & x <= upper, na.rm = na.rm))
    }
    abort("`eval_between()` must be used inside a grouped `dplyr` context when `term` has multiple entries.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_between()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    return(NA_real_)
  }

  bounds <- term[[current_term]]
  lower <- bounds[1]
  upper <- bounds[2]

  mean(x >= lower & x <= upper, na.rm = na.rm)
}


#' Compute the observed quantile value for each term within grouped simulation results
#'
#' Computes the specified quantile of `x` within each group, typically inside
#' `evaluate_model_results()` for simulation evaluation pipelines.
#'
#' @param x A numeric vector of estimates or statistics.
#' @param term A named numeric vector with quantile probabilities for each term.
#' For example, `c("(Intercept)" = 0.05, x = 0.95)`. If `NULL` (default), computes the median (0.5).
#' Probabilities may be numeric literals or expressions that reference grouping variables
#' (e.g., `c(conditionimpl = p)` when the results are grouped by `p`). Each element must
#' resolve to a single value within the current group.
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
#'   group_by(term) |>
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

  term <- resolve_term_arg(rlang::enquo(term), fn_name = "eval_quantile")

  if (is.null(term)) {
    return(stats::quantile(x, probs = 0.5, na.rm = na.rm, names = FALSE, type = 7))
  }

  if (!is.numeric(term)) {
    abort("`term` must be a named numeric vector of quantile probabilities (e.g., c(x = 0.05)).")
  }

  # Each resolved probability must lie within [0, 1]
  valid_probs <- purrr::map_lgl(term, ~ .x >= 0 && .x <= 1)
  if (!all(valid_probs)) {
    abort("Each element of `term` must be a single numeric quantile value between 0 and 1.")
  }

  group_vars <- tryCatch(dplyr::cur_group(), error = function(e) NULL)
  if (is.null(group_vars) || length(group_vars) == 0) {
    abort("`eval_quantile()` must be used inside a grouped `dplyr` context when `term` is provided.")
  }

  if (!"term" %in% names(group_vars)) {
    abort("Grouping variable `term` not found. Are you grouping by `term` before calling `eval_quantile()`?")
  }

  current_term <- as.character(group_vars$term)
  if (!current_term %in% names(term)) {
    # rlang::warn(glue::glue("Term '{current_term}' not found in `term` mapping. Returning NA."))
    return(NA_real_)
  }

  prob <- term[[current_term]]
  q <- stats::quantile(x, probs = prob, na.rm = na.rm, names = FALSE, type = 7)
  return(q)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Resolve the `term` argument of an `eval_*()` helper
#'
#' Lazily evaluates the captured `term` expression against the current group
#' keys (`dplyr::cur_group()`), so that elements of `term` may reference
#' grouping variables (e.g., `c(conditionimpl = beta)` when the results are
#' grouped by `beta`). Because true values must be constant within each
#' summarised group for evaluation metrics to be meaningful, any element that
#' does not resolve to the expected length aborts with guidance to add the
#' referenced column to `group_by()`.
#'
#' When `term` is written as a `c(...)` call with all arguments named, each
#' argument is evaluated separately before being combined. This is important:
#' evaluating the whole `c(...)` at once would let `c()` silently flatten a
#' length-n element (e.g., a non-grouping data column) into n length-1
#' elements with mangled names, and the term lookup would then silently
#' return `NA` instead of failing informatively.
#'
#' @param term_quo A quosure capturing the `term` argument.
#' @param fn_name Name of the calling function, used in error messages.
#' @param element_length Required length of each resolved element.
#' @param validate Logical; if `FALSE`, only resolves `term` without checking
#'   names or element lengths. Used by `eval_between()`, which accepts
#'   multiple input shapes and performs its own validation.
#'
#' @return The resolved `term` value (possibly `NULL`).
#' @noRd
resolve_term_arg <- function(term_quo, fn_name, element_length = 1L,
                             validate = TRUE) {
  group_keys <- tryCatch(dplyr::cur_group(), error = function(e) NULL)

  eval_with_hint <- function(x, env = NULL) {
    tryCatch(
      {
        if (is.null(env)) {
          rlang::eval_tidy(x, data = group_keys)
        } else {
          rlang::eval_tidy(x, data = group_keys, env = env)
        }
      },
      error = function(e) {
        abort(glue::glue(
          "Failed to evaluate `term` in `{fn_name}()`: {conditionMessage(e)} ",
          "If `term` references a column, that column must be a grouping ",
          "variable (e.g., `group_by(beta, term)`) so it is constant within ",
          "each group."
        ))
      }
    )
  }

  expr <- rlang::quo_get_expr(term_quo)

  # If `term` is written as c(name = value, ...), evaluate each element
  # separately so a value resolving to length > 1 is caught before c()
  # silently flattens it and mangles the names.
  if (validate && rlang::is_call(expr, "c")) {
    args <- rlang::call_args(expr)
    arg_names <- names(args)

    if (!is.null(arg_names) && all(arg_names != "")) {
      env <- rlang::quo_get_env(term_quo)
      values <- lapply(args, function(a) eval_with_hint(a, env = env))

      lens <- lengths(values)
      if (any(lens != element_length)) {
        abort_nonscalar_term(
          fn_name,
          bad_names = arg_names[lens != element_length],
          bad_lens = lens[lens != element_length],
          element_length = element_length
        )
      }

      return(stats::setNames(unlist(values, use.names = FALSE), arg_names))
    }
  }

  term <- eval_with_hint(term_quo)

  if (is.null(term) || !validate) {
    return(term)
  }

  if (!rlang::is_named(term)) {
    abort(glue::glue(
      "`term` in `{fn_name}()` must be a named vector (e.g., `c(x = 1)`)."
    ))
  }

  lens <- lengths(term)
  if (any(lens != element_length)) {
    abort_nonscalar_term(
      fn_name,
      bad_names = names(term)[lens != element_length],
      bad_lens = lens[lens != element_length],
      element_length = element_length
    )
  }

  term
}

#' Abort with guidance when a `term` element is not constant within the group
#' @noRd
abort_nonscalar_term <- function(fn_name, bad_names, bad_lens, element_length) {
  bad_desc <- paste0("`", bad_names, "` (length ", bad_lens, ")", collapse = ", ")
  abort(glue::glue(
    "In `{fn_name}()`, each element of `term` must resolve to length ",
    "{element_length}, but {bad_desc} did not. True values must be constant ",
    "within each summarised group. If an element references a data column ",
    "(e.g., `conditionimpl = beta`), add that column to `group_by()` ",
    "(e.g., `group_by(beta, term)`) so it resolves to one value per group."
  ))
}
