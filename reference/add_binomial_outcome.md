# Create a binomial outcome from linear predictors

Generates a binomial outcome by summing effects, computing probabilities
via the logistic function, and drawing a count out of a specified number
of trials. This generalizes
[`add_binary_outcome()`](https://iancero.github.io/rollout/reference/add_binary_outcome.md)
(which is a `size = 1` special case of this function) to support a
denominator greater than 1 — for example, a "Reach" outcome where `size`
is the number of people eligible at a site-period and the resulting
count is the number who received an intervention.

## Usage

``` r
add_binomial_outcome(
  data,
  size,
  linear_col = "y_linear",
  prob_col = "y_prob",
  binom_col = "y_binom",
  include_error = FALSE
)
```

## Arguments

- data:

  A data frame containing effect columns prefixed with `"."`.

- size:

  The number of trials for the binomial draw (the denominator). Either a
  single fixed value (e.g. `size = 10`) or the bare name of a column in
  `data` holding a per-row value (e.g. `size = n_eligible`).

- linear_col:

  Name of the column to store the summed linear predictor (default
  `"y_linear"`).

- prob_col:

  Name of the column to store probabilities (default `"y_prob"`).

- binom_col:

  Name of the column to store binomial counts (default `"y_binom"`).

- include_error:

  Logical; whether to include a `.error` column (if present) in the
  summed linear predictor (default `FALSE`). A binomial outcome's
  variance is already implied by `p` and `size`, so an additional
  individual-level residual error term is usually not part of the
  intended generative model. Set to `TRUE` to instead sum every
  `.`-prefixed column, including `.error`.

## Value

A `tibble` with added linear predictor, probability, and binomial count
columns.

## Examples

``` r
df <- tibble::tibble(.beta = 0.5, .u = rnorm(5))

# Fixed number of trials
add_binomial_outcome(df, size = 10)
#> # A tibble: 5 × 5
#>   .beta      .u y_linear y_prob y_binom
#>   <dbl>   <dbl>    <dbl>  <dbl>   <int>
#> 1   0.5 -1.86    -1.36    0.204       3
#> 2   0.5 -0.522   -0.0220  0.494       5
#> 3   0.5 -0.0526   0.447   0.610       6
#> 4   0.5  0.543    1.04    0.739       7
#> 5   0.5 -0.914   -0.414   0.398       2

# Per-row number of trials drawn from a column (e.g. an eligible count)
df2 <- tibble::tibble(.beta = 0.5, .u = rnorm(5), n_eligible = c(8, 12, 9, 15, 10))
add_binomial_outcome(df2, size = n_eligible)
#> # A tibble: 5 × 6
#>   .beta      .u n_eligible y_linear y_prob y_binom
#>   <dbl>   <dbl>      <dbl>    <dbl>  <dbl>   <int>
#> 1   0.5  0.724           8   1.22    0.773       5
#> 2   0.5  2.35           12   2.85    0.946      12
#> 3   0.5 -0.281           9   0.219   0.554       4
#> 4   0.5 -0.481          15   0.0190  0.505      10
#> 5   0.5  0.0792         10   0.579   0.641       7
```
