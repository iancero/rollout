# Extract and tidy model results from a column of models

Applies a tidying function (default
[`broom.mixed::tidy`](https://generics.r-lib.org/reference/tidy.html))
to a column of models, returning a tidy data frame with one row per term
per model, suitable for downstream summarisation and evaluation in
simulation studies.

## Usage

``` r
extract_model_results(
  models,
  model_col = model,
  tidy_fun = broom.mixed::tidy,
  .term = NULL
)
```

## Arguments

- models:

  A data frame containing a column of fitted model objects.

- model_col:

  Unquoted column name containing the models. Default is `model`.

- tidy_fun:

  A tidying function to apply to each model. Default is
  [`broom.mixed::tidy`](https://generics.r-lib.org/reference/tidy.html).
  The function must return a data frame with a `term` column.

- .term:

  Optional string specifying a term to filter after tidying (e.g.,
  `"(Intercept)"`). If `NULL` (default), all terms are retained.

## Value

A tidy data frame with the original columns of `models` joined to the
tidied model results, typically including columns such as `term`,
`estimate`, `std.error`, `statistic`, and `p.value`.

## Examples

``` r
library(dplyr)
library(purrr)
library(broom.mixed)

# Simulate and fit models
sim_models <- tibble(
  id = 1:5,
  model = map(1:5, ~ lm(mpg ~ wt, data = mtcars))
)

# Extract all terms
extract_model_results(sim_models)
#> # A tibble: 10 × 7
#>       id model  term        estimate std.error statistic  p.value
#>    <int> <list> <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#>  1     1 <lm>   (Intercept)    37.3      1.88      19.9  8.24e-19
#>  2     1 <lm>   wt             -5.34     0.559     -9.56 1.29e-10
#>  3     2 <lm>   (Intercept)    37.3      1.88      19.9  8.24e-19
#>  4     2 <lm>   wt             -5.34     0.559     -9.56 1.29e-10
#>  5     3 <lm>   (Intercept)    37.3      1.88      19.9  8.24e-19
#>  6     3 <lm>   wt             -5.34     0.559     -9.56 1.29e-10
#>  7     4 <lm>   (Intercept)    37.3      1.88      19.9  8.24e-19
#>  8     4 <lm>   wt             -5.34     0.559     -9.56 1.29e-10
#>  9     5 <lm>   (Intercept)    37.3      1.88      19.9  8.24e-19
#> 10     5 <lm>   wt             -5.34     0.559     -9.56 1.29e-10

# Extract only the slope term
extract_model_results(sim_models, .term = "wt")
#> # A tibble: 5 × 7
#>      id model  term  estimate std.error statistic  p.value
#>   <int> <list> <chr>    <dbl>     <dbl>     <dbl>    <dbl>
#> 1     1 <lm>   wt       -5.34     0.559     -9.56 1.29e-10
#> 2     2 <lm>   wt       -5.34     0.559     -9.56 1.29e-10
#> 3     3 <lm>   wt       -5.34     0.559     -9.56 1.29e-10
#> 4     4 <lm>   wt       -5.34     0.559     -9.56 1.29e-10
#> 5     5 <lm>   wt       -5.34     0.559     -9.56 1.29e-10
```
