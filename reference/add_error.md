# Add an error term for simulation

Adds a residual error term (column `.error`) to the data frame, drawn
from a normal distribution with specified variance.

## Usage

``` r
add_error(.data, variance = 1)
```

## Arguments

- .data:

  A data frame to which the error term will be added.

- variance:

  Numeric; variance of the residual error (default `1`).

## Value

A `tibble` with an added `.error` column.

## Examples

``` r
df <- tibble::tibble(x = 1:5)
add_error(df, variance = 2)
#> # A tibble: 5 × 2
#>       x .error
#>   <int>  <dbl>
#> 1     1  0.695
#> 2     2 -1.07 
#> 3     3  0.494
#> 4     4 -0.242
#> 5     5  2.31 
```
