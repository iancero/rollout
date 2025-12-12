# Create a Poisson outcome from linear predictors

Generates a Poisson-distributed count outcome by summing effects,
exponentiating to obtain rates, and drawing counts.

## Usage

``` r
add_poisson_outcome(
  data,
  linear_col = "y_linear",
  rate_col = "y_rate",
  count_col = "y_count"
)
```

## Arguments

- data:

  A data frame containing effect columns prefixed with `"."`.

- linear_col:

  Name of the column to store the summed linear predictor (default
  `"y_linear"`).

- rate_col:

  Name of the column to store Poisson rates (default `"y_rate"`).

- count_col:

  Name of the column to store Poisson counts (default `"y_count"`).

## Value

A `tibble` with added linear predictor, rate, and count columns.

## Examples

``` r
df <- tibble::tibble(.beta = 0.5, .u = rnorm(5), .error = rnorm(5))
add_poisson_outcome(df)
#> # A tibble: 5 × 6
#>   .beta     .u .error y_linear y_rate y_count
#>   <dbl>  <dbl>  <dbl>    <dbl>  <dbl>   <int>
#> 1   0.5  0.151 -0.795   -0.145  0.865       2
#> 2   0.5 -0.293 -1.57    -1.36   0.257       0
#> 3   0.5  0.255 -1.04    -0.286  0.752       0
#> 4   0.5 -0.553  1.02     0.967  2.63        4
#> 5   0.5  1.41  -0.702    1.20   3.33        5
```
