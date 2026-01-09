# Compute the proportion of values within term-specific intervals within grouped simulation results

Computes the proportion of `x` values falling within term-specific
intervals within each group, typically inside
[`evaluate_model_results()`](https://iancero.github.io/rollout/reference/evaluate_model_results.md)
for simulation evaluation pipelines.

## Usage

``` r
eval_between(x, term = NULL, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector of estimates or statistics.

- term:

  A named list of numeric vectors of length 2, giving the lower and
  upper bounds for each term. For example,
  `list("(Intercept)" = c(-1, 1), x = c(1, 3))`. If `NULL` (default),
  the interval is assumed to be `[0, 1]`.

- na.rm:

  Logical; whether to remove missing values when computing the
  proportion. Defaults to `FALSE`.

## Value

A numeric scalar representing the proportion of `x` within the
term-specific interval within the current group.

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
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
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
    prop_between = eval_between(
      estimate,
      term = list("wt" = c(-1, 0))
    )
  )
#> # A tibble: 1 × 6
#>   term  n_models mean_estimate mean_std.error power prop_between
#>   <chr>    <int>         <dbl>          <dbl> <dbl>        <dbl>
#> 1 wt          50         -5.34          0.559     1            0
```
