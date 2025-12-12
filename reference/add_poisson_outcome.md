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
#> Error in data %>% dplyr::mutate(`:=`(!!linear_col, rowSums(dplyr::pick(all_of(dot_cols)))),     `:=`(!!rate_col, exp(.data[[linear_col]])), `:=`(!!count_col,         rpois(dplyr::n(), lambda = .data[[rate_col]]))): could not find function "%>%"
```
