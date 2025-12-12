# Create a binary outcome from linear predictors

Generates a binary outcome by summing effects, computing probabilities
via the logistic function, and drawing binary outcomes.

## Usage

``` r
add_binary_outcome(
  data,
  linear_col = "y_linear",
  prob_col = "y_prob",
  binary_col = "y_bin"
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

  Name of the column to store binary outcomes (default `"y_bin"`).

## Value

A `tibble` with added linear predictor, probability, and binary outcome
columns.

## Examples

``` r
df <- tibble::tibble(.beta = 0.5, .u = rnorm(5), .error = rnorm(5))
add_binary_outcome(df)
#> # A tibble: 5 × 6
#>   .beta       .u .error y_linear y_prob y_bin
#>   <dbl>    <dbl>  <dbl>    <dbl>  <dbl> <int>
#> 1   0.5  0.255   -1.82    -1.07   0.256     0
#> 2   0.5 -2.44    -0.247   -2.18   0.101     0
#> 3   0.5 -0.00557 -0.244    0.250  0.562     0
#> 4   0.5  0.622   -0.283    0.839  0.698     0
#> 5   0.5  1.15    -0.554    1.09   0.749     1
```
