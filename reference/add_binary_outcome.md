# Create a binary outcome from linear predictors

Generates a binary outcome by summing effects, computing probabilities
via the logistic function, and drawing binary outcomes.

## Usage

``` r
add_binary_outcome(
  data,
  linear_col = "y_linear",
  prob_col = "y_prob",
  binary_col = "y_bin"
)
```

## Arguments

- data:

  A data frame containing effect columns prefixed with `"."`.

- linear_col:

  Name of the column to store the summed linear predictor (default
  `"y_linear"`).

- prob_col:

  Name of the column to store probabilities (default `"y_prob"`).

- binary_col:

  Name of the column to store binary outcomes (default `"y_bin"`).

## Value

A `tibble` with added linear predictor, probability, and binary outcome
columns.

## Examples

``` r
df <- tibble::tibble(.beta = 0.5, .u = rnorm(5), .error = rnorm(5))
add_binary_outcome(df)
#> Error in data %>% dplyr::mutate(`:=`(!!linear_col, rowSums(dplyr::pick(all_of(dot_cols)))),     `:=`(!!prob_col, plogis(.data[[linear_col]])), `:=`(!!binary_col,         rbinom(dplyr::n(), size = 1, prob = .data[[prob_col]]))): could not find function "%>%"
```
