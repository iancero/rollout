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
#> Error in data %>% dplyr::mutate(`:=`(!!output_col, rowSums(dplyr::pick(all_of(dot_cols))))): could not find function "%>%"
```
