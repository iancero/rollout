# Add a random effect column for simulation

Adds a random effect column (prefixed with `"."`) to the design data
frame, with optional grouping for nested random effects.

## Usage

``` r
add_random_effect(design_df, ..., .nesting = NULL)
```

## Arguments

- design_df:

  A data frame containing the rollout design and any parameters.

- ...:

  A single named expression specifying the random effect to add (e.g.,
  `u = rnorm(1, 0, 1)`).

- .nesting:

  Optional character vector specifying grouping columns for nested
  random effects (default `NULL`).

## Value

A `tibble` with the added random effect column.

## Examples

``` r
df <- tibble::tibble(site = rep(1:2, each = 3))
add_random_effect(df, u = rnorm(1, 0, 1), .nesting = "site")
#> # A tibble: 6 × 2
#>    site      .u
#>   <int>   <dbl>
#> 1     1 -0.206 
#> 2     1 -0.206 
#> 3     1 -0.206 
#> 4     2  0.0192
#> 5     2  0.0192
#> 6     2  0.0192
```
