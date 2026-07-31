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
  Values may be numeric literals or expressions that reference grouping
  variables (e.g., `c(conditionimpl = beta)` when the results are
  grouped by `beta`), allowing the true value to vary across simulated
  parameter conditions. Each element must resolve to a single value
  within the current group.

- na.rm:

  Logical; whether to remove missing values when computing the mean
  bias. Defaults to `FALSE`.

- warnings:

  Should warnings be returned?

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
  group_by(term) |>
  evaluate_model_results(
    bias = eval_bias(
      estimate,
      term = c("wt" = -5)
    )
  )
#> # A tibble: 1 × 6
#>   term  n_models mean_estimate mean_std.error power   bias
#>   <chr>    <int>         <dbl>          <dbl> <dbl>  <dbl>
#> 1 wt          50         -5.34          0.559     1 -0.344

# Compute bias relative to zero for all terms
sim_models |>
  group_by(term) |>
  evaluate_model_results(
    bias = eval_bias(estimate)
  )
#> # A tibble: 2 × 6
#>   term        n_models mean_estimate mean_std.error power  bias
#>   <chr>          <int>         <dbl>          <dbl> <dbl> <dbl>
#> 1 (Intercept)       50         37.3           1.88      1 37.3 
#> 2 wt                50         -5.34          0.559     1 -5.34

# True values may reference grouping variables, allowing them to vary
# across simulated parameter conditions (here, a different true effect
# for each value of `beta`):
sim_grid <- tidyr::expand_grid(
  beta = c(0.35, 0.65),
  term = "conditionimpl",
  rep = 1:20
) |>
  mutate(estimate = beta + rnorm(40, sd = 0.05))

sim_grid |>
  group_by(beta, term) |>
  summarise(
    bias = eval_bias(estimate, term = c(conditionimpl = beta)),
    .groups = "drop"
  )
#> # A tibble: 2 × 3
#>    beta term             bias
#>   <dbl> <chr>           <dbl>
#> 1  0.35 conditionimpl 0.0129 
#> 2  0.65 conditionimpl 0.00369
```
