# Expand a data frame with parameter combinations for simulation

Adds combinations of specified parameter values to a data frame for
simulation by expanding over all combinations.

## Usage

``` r
add_parameter(df, ...)
```

## Arguments

- df:

  A data frame to expand.

- ...:

  Named vectors specifying parameter values to expand, provided as
  `param_name = values`.

## Value

A `tibble` with added parameter columns for each combination of values.

## Examples

``` r
df <- tibble::tibble(site = "A", condition = "control")
add_parameter(df, beta = c(0, 0.5), sigma = c(1, 2))
#> # A tibble: 4 × 4
#>   site  condition  beta sigma
#>   <chr> <chr>     <dbl> <dbl>
#> 1 A     control     0       1
#> 2 A     control     0       2
#> 3 A     control     0.5     1
#> 4 A     control     0.5     2
```
