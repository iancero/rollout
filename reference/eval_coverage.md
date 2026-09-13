# Compute confidence interval coverage of term-specific true values within grouped simulation results

Computes the proportion of replicates whose interval `[lower, upper]`
contains the true value for the current term, typically inside
[`evaluate_model_results()`](https://iancero.github.io/rollout/reference/evaluate_model_results.md)
for simulation evaluation pipelines.

## Usage

``` r
eval_coverage(term = NULL, lower = conf.low, upper = conf.high, na.rm = FALSE)
```

## Arguments

- term:

  A named numeric vector providing the true value for each term. For
  example, `c("(Intercept)" = 0, x = 2)` to specify the true values for
  each term. If `NULL` (default), the true value is zero for all terms.
  When the true value really is zero, coverage is then one minus the
  empirical Type I error rate. Values may be numeric literals or
  expressions that reference grouping variables (e.g.,
  `c(conditionimpl = beta)` when the results are grouped by `beta`),
  allowing the true value to vary across simulated parameter conditions.
  Each element must resolve to a single value within the current group.

- lower:

  Unquoted column name or expression giving the lower bound of each
  replicate's interval, evaluated within the current group. Defaults to
  `conf.low`, the column created by
  `broom.mixed::tidy(m, conf.int = TRUE)`.

- upper:

  Unquoted column name or expression giving the upper bound of each
  replicate's interval, evaluated within the current group. Defaults to
  `conf.high`, the column created by
  `broom.mixed::tidy(m, conf.int = TRUE)`.

- na.rm:

  A logical value indicating whether to remove missing values when
  computing the proportion. Defaults to `FALSE`.

## Value

A numeric scalar representing the proportion of intervals containing the
term-specific true value within the current group.

## Details

This function is designed to be used inside
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
within a grouped tidyverse pipeline, typically after grouping by `term`.
It computes the mean of `truth >= lower & truth <= upper`, where `truth`
is the true value for the corresponding term. Interval bounds are
inclusive.

There is no `level` argument. The confidence level is whatever was
chosen when the models were tidied (e.g., `conf.level` in
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html), which
defaults to 0.95).

The Monte Carlo standard error of the coverage estimate is
`sqrt(coverage * (1 - coverage) / n_models)`, where `n_models` is
already returned by
[`evaluate_model_results()`](https://iancero.github.io/rollout/reference/evaluate_model_results.md).

If `term` is provided, the current grouping must include a `term`
variable matching the names in `term`. If a term in the group is not
found in the provided `term` mapping, the function will return `NA`.

## Examples

``` r
library(dplyr)
library(purrr)
library(broom.mixed)

# Simulate and fit models, keeping confidence intervals when tidying
sim_models <- tibble(
  id = 1:50,
  model = map(1:50, ~ lm(mpg ~ wt, data = mtcars))
) |>
  extract_model_results(tidy_fun = \(m) broom::tidy(m, conf.int = TRUE))

# Compute coverage of the true value (hypothetical slope = -5)
sim_models |>
  filter(term == "wt") |>
  group_by(term) |>
  evaluate_model_results(
    coverage = eval_coverage(
      term = c("wt" = -5)
    )
  )
#> # A tibble: 1 × 6
#>   term  n_models mean_estimate mean_std.error power coverage
#>   <chr>    <int>         <dbl>          <dbl> <dbl>    <dbl>
#> 1 wt          50         -5.34          0.559     1        1

# Compute coverage of zero for all terms
sim_models |>
  group_by(term) |>
  evaluate_model_results(
    coverage = eval_coverage()
  )
#> # A tibble: 2 × 6
#>   term        n_models mean_estimate mean_std.error power coverage
#>   <chr>          <int>         <dbl>          <dbl> <dbl>    <dbl>
#> 1 (Intercept)       50         37.3           1.88      1        0
#> 2 wt                50         -5.34          0.559     1        0

# True values may reference grouping variables, allowing them to vary
# across simulated parameter conditions (here, a different true effect
# for each value of `beta`):
sim_grid <- tidyr::expand_grid(
  beta = c(0.35, 0.65),
  term = "conditionimpl",
  rep = 1:20
) |>
  mutate(
    estimate = beta + rnorm(40, sd = 0.05),
    conf.low = estimate - 1.96 * 0.05,
    conf.high = estimate + 1.96 * 0.05
  )

sim_grid |>
  group_by(beta, term) |>
  summarise(
    coverage = eval_coverage(term = c(conditionimpl = beta)),
    .groups = "drop"
  )
#> # A tibble: 2 × 3
#>    beta term          coverage
#>   <dbl> <chr>            <dbl>
#> 1  0.35 conditionimpl     0.85
#> 2  0.65 conditionimpl     1   
```
