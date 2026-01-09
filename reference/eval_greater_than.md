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
  group_by(term) |>
  evaluate_model_results(
    prop_above_0 = eval_greater_than(
      estimate,
      term = c("wt" = 0)
    )
  )
#> # A tibble: 1 × 6
#>   term  n_models mean_estimate mean_std.error power prop_above_0
#>   <chr>    <int>         <dbl>          <dbl> <dbl>        <dbl>
#> 1 wt          50         -5.34          0.559     1            0
```
