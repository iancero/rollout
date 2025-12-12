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
#>          x    .beta
#>      <dbl>    <dbl>
#> 1 -0.00289 -0.00145
#> 2  0.413    0.207  
#> 3  0.724    0.362  
#> 4  2.35     1.18   
#> 5 -0.281   -0.141  
```
