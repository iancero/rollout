# Summarise simulation results from extracted model estimates

Computes summary statistics (e.g., power, custom summaries) across a set
of extracted model results, typically from
[`extract_model_results()`](https://iancero.github.io/rollout/reference/extract_model_results.md),
to facilitate simulation evaluation and reporting.

## Usage

``` r
evaluate_model_results(
  results,
  alpha = 0.05,
  ...,
  .summarise_standard_broom = FALSE,
  broom_cols = c("estimate", "std.error", "statistic", "df", "p.value")
)
```

## Arguments

- results:

  A data frame of extracted model results, typically including columns
  like `term`, `estimate`, `std.error`, `statistic`, and `p.value`.

- alpha:

  Significance level used to compute power. Defaults to `0.05`.

- ...:

  Additional summary expressions to compute within
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
  These may include calls to helper functions like
  [`eval_bias()`](https://iancero.github.io/rollout/reference/eval_bias.md),
  [`eval_quantile()`](https://iancero.github.io/rollout/reference/eval_quantile.md),
  or direct summaries such as `mean(estimate, na.rm = TRUE)`.

- .summarise_standard_broom:

  Logical; if `TRUE`, computes mean and standard deviation for standard
  `broom` columns present in the data (columns in `broom_cols`).
  Defaults to `FALSE`.

- broom_cols:

  Character vector of standard `broom` columns to summarise if
  `.summarise_standard_broom = TRUE`. Defaults to
  `c("estimate", "std.error", "statistic", "df", "p.value")`.

## Value

A summarised data frame containing:

- `n_models`: the number of models summarised.

- `power`: the proportion of p-values less than `alpha` (NA if all
  p-values are NA).

- Additional columns corresponding to custom summaries provided in
  `...`.

- Mean and SD summaries of `broom` columns if
  `.summarise_standard_broom = TRUE`.

## Examples

``` r
library(dplyr)
library(purrr)
library(broom.mixed)

# Simulate and fit models
sim_models <- tibble(
  id = 1:50,
  model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
) |>
  extract_model_results()

# Evaluate power and mean estimate for the slope
sim_models |>
  filter(term == "wt") |>
  evaluate_model_results(
    alpha = 0.05,
    mean_estimate = mean(estimate, na.rm = TRUE),
    sd_estimate = sd(estimate, na.rm = TRUE)
  )
#> # A tibble: 1 × 5
#>   n_models mean_estimate mean_std.error power sd_estimate
#>      <int>         <dbl>          <dbl> <dbl>       <dbl>
#> 1       50         -5.34          0.559     1           0

# Evaluate with .summarise_standard_broom = TRUE
sim_models |>
  filter(term == "wt") |>
  evaluate_model_results(
    .summarise_standard_broom = TRUE
  )
#> # A tibble: 1 × 10
#>   n_models mean_estimate mean_std.error power sd_estimate sd_std.error
#>      <int>         <dbl>          <dbl> <dbl>       <dbl>        <dbl>
#> 1       50         -5.34          0.559     1           0            0
#> # ℹ 4 more variables: mean_statistic <dbl>, sd_statistic <dbl>,
#> #   mean_p.value <dbl>, sd_p.value <dbl>

# Evaluate with eval_bias to compute bias relative to the true value
# Suppose the true slope of wt is -5 (hypothetical)
sim_models |>
  filter(term == "wt") |>
  evaluate_model_results(
    bias = eval_bias(
      estimate,
      term = c("wt" = -5)
    )
  )
#> Error in dplyr::summarise(results, n_models = dplyr::n(), mean_estimate = dplyr::if_else(condition = all(is.na(p.value)),     true = NA_real_, false = mean(estimate, na.rm = TRUE)), mean_std.error = dplyr::if_else(condition = all(is.na(p.value)),     true = NA_real_, false = mean(std.error, na.rm = TRUE)),     power = dplyr::if_else(condition = all(is.na(p.value)), true = NA_real_,         false = mean(p.value < alpha, na.rm = TRUE)), !!!summary_exprs,     !!!{        if (.summarise_standard_broom) {            rlang::exprs(dplyr::across(dplyr::all_of(intersect(broom_cols,                 names(results))), list(mean = mean, sd = sd),                 .names = "{fn}_{col}"))        }        else {            rlang::exprs()        }    }): ℹ In argument: `bias = eval_bias(estimate, term = c(wt = -5))`.
#> Caused by error in `eval_bias()`:
#> ! `eval_bias()` must be used inside a grouped `dplyr` context when `term` is provided.
```
