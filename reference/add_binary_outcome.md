# Create a binary outcome from linear predictors

Generates a binary outcome by summing effects, computing probabilities
via the logistic function, and drawing binary outcomes. This is a thin
wrapper around
[`add_binomial_outcome()`](https://iancero.github.io/rollout/reference/add_binomial_outcome.md)
with `size = 1`.

## Usage

``` r
add_binary_outcome(
  data,
  linear_col = "y_linear",
  prob_col = "y_prob",
  binary_col = "y_binary",
  include_error = FALSE
)
```

## Arguments

- data:

  A data frame containing effect columns prefixed with `"."`.

- linear_col:

  Name of the column to store the summed linear predictor (default
  `"y_linear"`).

- prob_col:

  Name of the column to store probabilities (default `"y_prob"`).

- binary_col:

  Name of the column to store binary outcomes (default `"y_binary"`).

- include_error:

  Logical; whether to include a `.error` column (if present) in the
  summed linear predictor (default `FALSE`). A Bernoulli outcome's
  variance is already implied by `p`, so an additional individual-level
  residual error term is usually not part of the intended generative
  model. Set to `TRUE` to instead sum every `.`-prefixed column,
  including `.error`, as earlier versions of this function always did.

## Value

A `tibble` with added linear predictor, probability, and binary outcome
columns.

## Examples

``` r
df <- tibble::tibble(.beta = 0.5, .u = rnorm(5), .error = rnorm(5))

# By default, .error is excluded from the linear predictor
add_binary_outcome(df)
#> # A tibble: 5 × 6
#>   .beta       .u .error y_linear y_prob y_binary
#>   <dbl>    <dbl>  <dbl>    <dbl>  <dbl>    <int>
#> 1   0.5 -1.40     1.15    -0.900  0.289        0
#> 2   0.5  0.255   -1.82     0.755  0.680        1
#> 3   0.5 -2.44    -0.247   -1.94   0.126        0
#> 4   0.5 -0.00557 -0.244    0.494  0.621        1
#> 5   0.5  0.622   -0.283    1.12   0.754        0

# Include .error in the sum if that's really what you want
add_binary_outcome(df, include_error = TRUE)
#> # A tibble: 5 × 6
#>   .beta       .u .error y_linear y_prob y_binary
#>   <dbl>    <dbl>  <dbl>    <dbl>  <dbl>    <int>
#> 1   0.5 -1.40     1.15     0.248  0.562        0
#> 2   0.5  0.255   -1.82    -1.07   0.256        0
#> 3   0.5 -2.44    -0.247   -2.18   0.101        0
#> 4   0.5 -0.00557 -0.244    0.250  0.562        0
#> 5   0.5  0.622   -0.283    0.839  0.698        1
```
