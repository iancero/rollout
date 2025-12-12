# Create a linear outcome by summing effects

Generates a linear outcome variable by summing all columns that start
with `"."` (representing fixed, random, and error effects).

## Usage

``` r
add_linear_outcome(data, output_col = "y_linear")
```

## Arguments

- data:

  A data frame containing effect columns prefixed with `"."`.

- output_col:

  Name of the column to store the linear outcome (default `"y_linear"`).

## Value

A `tibble` with the added linear outcome column.

## Examples

``` r
df <- tibble::tibble(.beta = 0.5, .u = rnorm(5), .error = rnorm(5))
add_linear_outcome(df)
#> # A tibble: 5 × 4
#>   .beta      .u .error y_linear
#>   <dbl>   <dbl>  <dbl>    <dbl>
#> 1   0.5  0.0792 -0.424    0.155
#> 2   0.5  0.770  -0.872    0.398
#> 3   0.5  0.563   0.107    1.17 
#> 4   0.5 -0.374  -0.587   -0.461
#> 5   0.5 -0.601  -0.328   -0.429
```
