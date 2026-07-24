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
#>   .beta      .u .error y_linear y_rate y_count
#>   <dbl>   <dbl>  <dbl>    <dbl>  <dbl>   <int>
#> 1   0.5  0.862   0.550    1.91   6.77       10
#> 2   0.5 -0.243  -2.27    -2.02   0.133       0
#> 3   0.5 -0.206   2.68     2.98  19.6        16
#> 4   0.5  0.0192 -0.361    0.158  1.17        0
#> 5   0.5  0.0296  0.213    0.743  2.10        2
```
