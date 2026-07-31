# Changelog

## rollout 0.2.0

- New function
  [`add_binomial_outcome()`](https://iancero.github.io/rollout/reference/add_binomial_outcome.md)
  generalizes
  [`add_binary_outcome()`](https://iancero.github.io/rollout/reference/add_binary_outcome.md)
  to support a binomial outcome with more than one trial. `size` (the
  number of trials, i.e. the denominator) can be a fixed value or the
  name of an existing column, so a per-row size that varies — e.g. a
  Poisson-distributed eligible count generated upstream with
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  — can be piped straight in:
  `add_binomial_outcome(size = n_eligible, ...)`.
  [`add_binary_outcome()`](https://iancero.github.io/rollout/reference/add_binary_outcome.md)
  is now a `size = 1` special case of this function.
- [`add_binary_outcome()`](https://iancero.github.io/rollout/reference/add_binary_outcome.md)
  and
  [`add_binomial_outcome()`](https://iancero.github.io/rollout/reference/add_binomial_outcome.md)
  both gain an `include_error` argument (default `FALSE`). By default, a
  `.error` column (if present) is no longer included in the summed
  linear predictor for these two outcomes, since a Bernoulli/binomial
  outcome’s variance is already implied by `p` (and `size`). Set
  `include_error = TRUE` to restore the previous behavior of summing
  every `.`-prefixed column.
- **Breaking change:**
  [`add_binary_outcome()`](https://iancero.github.io/rollout/reference/add_binary_outcome.md)’s
  default output column is now named `"y_binary"` (previously
  `"y_bin"`).
- The `term` argument of the `eval_*()` helpers
  ([`eval_bias()`](https://iancero.github.io/rollout/reference/eval_bias.md),
  [`eval_greater_than()`](https://iancero.github.io/rollout/reference/eval_greater_than.md),
  [`eval_less_than()`](https://iancero.github.io/rollout/reference/eval_less_than.md),
  [`eval_between()`](https://iancero.github.io/rollout/reference/eval_between.md),
  and
  [`eval_quantile()`](https://iancero.github.io/rollout/reference/eval_quantile.md))
  can now reference grouping variables, so true values, thresholds,
  bounds, and probabilities may vary across simulated parameter
  conditions — e.g.,
  `eval_bias(estimate, term = c(conditionimpl = beta))` when results are
  grouped by `beta`. Elements of `term` that do not resolve to a single
  value per group (for example, ones referencing a column that was not
  included in
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html))
  now abort with guidance instead of silently returning `NA`.
- [`eval_less_than()`](https://iancero.github.io/rollout/reference/eval_less_than.md)
  now correctly computes the proportion of values *below* zero when
  `term = NULL`; previously it computed the proportion above zero. Error
  messages in
  [`eval_greater_than()`](https://iancero.github.io/rollout/reference/eval_greater_than.md)
  and
  [`eval_less_than()`](https://iancero.github.io/rollout/reference/eval_less_than.md)
  also now reference the correct function names.

## rollout 0.1.0

CRAN release: 2026-01-13

- Initial CRAN submission.
