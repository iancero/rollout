# Compute the proportion of values above term-specific thresholds within grouped simulation results

Computes the proportion of `x` values exceeding term-specific thresholds
within each group, typically inside
[`evaluate_model_results()`](https://iancero.github.io/rollout/reference/evaluate_model_results.md)
for simulation evaluation pipelines.

## Usage

``` r
eval_greater_than(x, term = NULL, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector of estimates or statistics.

- term:

  A named numeric vector providing the threshold for each term. For
  example, `c("(Intercept)" = 0, x = 2)`. If `NULL` (default), threshold
  is assumed to be zero.

- na.rm:

  Logical; whether to remove missing values when computing the
  proportion. Defaults to `FALSE`.

## Value

A numeric scalar representing the proportion of `x` exceeding the
term-specific threshold within the current group.

## Details

This function is designed to be used inside
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
within a grouped tidyverse pipeline, typically after grouping by `term`.

If `term` is provided, the current grouping must include a `term`
variable matching the names in `term`. If a term in the group is not
found in the provided `term` mapping, the function will return `NA` with
a warning.

## Examples

``` r
library(dplyr)
library(purrr)
library(broom.mixed)

sim_models <- tibble(
  id = 1:50,
  model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
) |>
  extract_model_results()

sim_models |>
  filter(term == "wt") |>
  evaluate_model_results(
    prop_above_0 = eval_greater_than(
      estimate,
      term = c("wt" = 0)
    )
  )
#> Error in dplyr::summarise(results, n_models = dplyr::n(), mean_estimate = dplyr::if_else(condition = all(is.na(p.value)),     true = NA_real_, false = mean(estimate, na.rm = TRUE)), mean_std.error = dplyr::if_else(condition = all(is.na(p.value)),     true = NA_real_, false = mean(std.error, na.rm = TRUE)),     power = dplyr::if_else(condition = all(is.na(p.value)), true = NA_real_,         false = mean(p.value < alpha, na.rm = TRUE)), !!!summary_exprs,     !!!{        if (.summarise_standard_broom) {            rlang::exprs(dplyr::across(dplyr::all_of(intersect(broom_cols,                 names(results))), list(mean = mean, sd = sd),                 .names = "{fn}_{col}"))        }        else {            rlang::exprs()        }    }): ℹ In argument: `prop_above_0 = eval_greater_than(estimate, term = c(wt =
#>   0))`.
#> Caused by error in `eval_greater_than()`:
#> ! `eval_greater()` must be used inside a grouped `dplyr` context when `term` is provided.
```
