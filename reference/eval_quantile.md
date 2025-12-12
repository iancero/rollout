# Compute the observed quantile value for each term within grouped simulation results

Computes the specified quantile of `x` within each group, typically
inside
[`evaluate_model_results()`](https://iancero.github.io/rollout/reference/evaluate_model_results.md)
for simulation evaluation pipelines.

## Usage

``` r
eval_quantile(x, term = NULL, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector of estimates or statistics.

- term:

  A named numeric vector with quantile probabilities for each term. For
  example, `c("(Intercept)" = 0.05, x = 0.95)`. If `NULL` (default),
  computes the median (0.5).

- na.rm:

  Logical; whether to remove missing values when computing the quantile.
  Defaults to `FALSE`.

## Value

A numeric scalar representing the observed quantile of `x` within the
current group.

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
    lower_quantile = eval_quantile(
      estimate,
      term = c("wt" = 0.05)
    ),
    upper_quantile = eval_quantile(
      estimate,
      term = c("wt" = 0.95)
    )
  )
#> Error in dplyr::summarise(results, n_models = dplyr::n(), mean_estimate = dplyr::if_else(condition = all(is.na(rlang::.data$p.value)),     true = NA_real_, false = mean(rlang::.data$estimate, na.rm = TRUE)),     mean_std.error = dplyr::if_else(condition = all(is.na(rlang::.data$p.value)),         true = NA_real_, false = mean(rlang::.data$std.error,             na.rm = TRUE)), power = dplyr::if_else(condition = all(is.na(rlang::.data$p.value)),         true = NA_real_, false = mean(rlang::.data$p.value <             alpha, na.rm = TRUE)), !!!summary_exprs, !!!{        if (.summarise_standard_broom) {            rlang::exprs(dplyr::across(dplyr::all_of(intersect(broom_cols,                 names(results))), list(mean = base::mean, sd = stats::sd),                 .names = "{fn}_{col}"))        }        else {            rlang::exprs()        }    }): ℹ In argument: `mean_estimate = dplyr::if_else(...)`.
#> Caused by error in `evaluate_model_results()`:
#> ! Can't subset `.data` outside of a data mask context.
```
