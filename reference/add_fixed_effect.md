# Add a fixed effect column for simulation

Adds a fixed effect column (prefixed with `"."`) to the design data
frame for simulation purposes.

## Usage

``` r
add_fixed_effect(design_df, ...)
```

## Arguments

- design_df:

  A data frame containing the rollout design and any parameters.

- ...:

  A single named expression specifying the fixed effect to add (e.g.,
  `beta = 0.5 * x`).

## Value

A `tibble` with the added fixed effect column.

## Examples

``` r
df <- tibble::tibble(x = rnorm(5))
add_fixed_effect(df, beta = 0.5 * x)
#> # A tibble: 5 × 2
#>        x   .beta
#>    <dbl>   <dbl>
#> 1 -0.134 -0.0670
#> 2 -1.91  -0.955 
#> 3 -0.279 -0.140 
#> 4 -0.313 -0.157 
#> 5  1.07   0.534 
```
