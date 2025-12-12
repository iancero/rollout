# Compute bias relative to term-specific true values within grouped simulation results

Computes the mean bias (difference between estimated values and true
values) within each group, typically inside
[`evaluate_model_results()`](https://iancero.github.io/rollout/reference/evaluate_model_results.md)
for simulation evaluation pipelines.

## Usage

``` r
eval_bias(x, term = NULL, na.rm = FALSE, warnings = TRUE)
```

## Arguments

- x:

  A numeric vector of estimates (e.g., from a model term).

- term:

  A named numeric vector providing the true value for each term. For
  example, `c("(Intercept)" = 0, x = 2)` to specify the true values for
  each term. If `NULL` (default), bias is computed relative to zero.

- na.rm:

  Logical; whether to remove missing values when computing the mean
  bias. Defaults to `FALSE`.

## Value

A numeric scalar representing the mean bias within the current group.

## Details

This function is designed to be used inside
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
within a grouped tidyverse pipeline, typically after grouping by `term`.
It computes the mean of `x` minus the true value for the corresponding
term.

If `term` is provided, the current grouping must include a `term`
variable matching the names in `term`. If a term in the group is not
found in the provided `term` mapping, the function will return `NA` with
a warning.

## Examples

``` r
library(dplyr)
library(purrr)
library(broom.mixed)

# Simulate and fit models
sim_models <- tibble(
  id = 1:50,
  model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
) |>
  extract_model_results()

# Compute bias relative to true value (hypothetical slope = -5)
sim_models |>
  filter(term == "wt") |>
  evaluate_model_results(
    bias = eval_bias(
      estimate,
      term = c("wt" = -5)
    )
  )
#> Error in dplyr::summarise(results, n_models = dplyr::n(), mean_estimate = dplyr::if_else(condition = all(is.na(rlang::.data$p.value)),     true = NA_real_, false = mean(rlang::.data$estimate, na.rm = TRUE)),     mean_std.error = dplyr::if_else(condition = all(is.na(rlang::.data$p.value)),         true = NA_real_, false = mean(rlang::.data$std.error,             na.rm = TRUE)), power = dplyr::if_else(condition = all(is.na(rlang::.data$p.value)),         true = NA_real_, false = mean(rlang::.data$p.value <             alpha, na.rm = TRUE)), !!!summary_exprs, !!!{        if (.summarise_standard_broom) {            rlang::exprs(dplyr::across(dplyr::all_of(intersect(broom_cols,                 names(results))), list(mean = base::mean, sd = stats::sd),                 .names = "{fn}_{col}"))        }        else {            rlang::exprs()        }    }): ℹ In argument: `mean_estimate = dplyr::if_else(...)`.
#> Caused by error in `evaluate_model_results()`:
#> ! Can't subset `.data` outside of a data mask context.

# Compute bias relative to zero for all terms
sim_models |>
  group_by(term) |>
  evaluate_model_results(
    bias = eval_bias(estimate)
  )
#> Error in dplyr::summarise(results, n_models = dplyr::n(), mean_estimate = dplyr::if_else(condition = all(is.na(rlang::.data$p.value)),     true = NA_real_, false = mean(rlang::.data$estimate, na.rm = TRUE)),     mean_std.error = dplyr::if_else(condition = all(is.na(rlang::.data$p.value)),         true = NA_real_, false = mean(rlang::.data$std.error,             na.rm = TRUE)), power = dplyr::if_else(condition = all(is.na(rlang::.data$p.value)),         true = NA_real_, false = mean(rlang::.data$p.value <             alpha, na.rm = TRUE)), !!!summary_exprs, !!!{        if (.summarise_standard_broom) {            rlang::exprs(dplyr::across(dplyr::all_of(intersect(broom_cols,                 names(results))), list(mean = base::mean, sd = stats::sd),                 .names = "{fn}_{col}"))        }        else {            rlang::exprs()        }    }): ℹ In argument: `mean_estimate = dplyr::if_else(...)`.
#> ℹ In group 1: `term = "(Intercept)"`.
#> Caused by error in `evaluate_model_results()`:
#> ! Can't subset `.data` outside of a data mask context.
```
