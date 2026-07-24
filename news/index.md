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

## rollout 0.1.0

CRAN release: 2026-01-13

- Initial CRAN submission.
