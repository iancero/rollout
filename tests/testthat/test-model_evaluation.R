# Defaults (term = NULL) ------------------------------------------------------

test_that("eval_bias() with term = NULL computes mean deviation from zero", {
  expect_equal(eval_bias(c(1, 2, 3)), 2)
  expect_equal(eval_bias(c(-1, 1)), 0)
})

test_that("eval_greater_than() and eval_less_than() defaults point in opposite directions", {
  x <- c(-2, -1, -1, 1)
  expect_equal(eval_greater_than(x), 0.25)
  expect_equal(eval_less_than(x), 0.75)
})

test_that("eval_quantile() with term = NULL returns the median", {
  expect_equal(eval_quantile(1:9), 5)
})

test_that("eval_between() with term = NULL uses the [0, 1] interval", {
  expect_equal(eval_between(c(-0.5, 0.25, 0.75, 1.5)), 0.5)
})

# Literal named `term` values -------------------------------------------------

test_that("eval_bias() applies literal term-specific true values", {
  df <- tibble::tibble(
    term = rep(c("a", "b"), each = 2),
    estimate = c(1, 3, 10, 12)
  )

  out <- df |>
    dplyr::group_by(term) |>
    dplyr::summarise(bias = eval_bias(estimate, term = c(a = 0, b = 10)))

  expect_equal(out$bias[out$term == "a"], 2)
  expect_equal(out$bias[out$term == "b"], 1)
})

test_that("eval_bias() returns NA for terms missing from the mapping", {
  df <- tibble::tibble(term = c("a", "a"), estimate = c(1, 2))

  out <- df |>
    dplyr::group_by(term) |>
    dplyr::summarise(bias = eval_bias(estimate, term = c(other = 0)))

  expect_true(is.na(out$bias))
})

test_that("eval_bias() with a pre-built term vector still works", {
  truth <- c(a = 0, b = 10)
  df <- tibble::tibble(
    term = rep(c("a", "b"), each = 2),
    estimate = c(1, 3, 10, 12)
  )

  out <- df |>
    dplyr::group_by(term) |>
    dplyr::summarise(bias = eval_bias(estimate, term = truth))

  expect_equal(out$bias[out$term == "a"], 2)
  expect_equal(out$bias[out$term == "b"], 1)
})

# Grouping-variable references in `term` --------------------------------------

test_that("eval_bias() resolves term values from grouping variables", {
  df <- tidyr::expand_grid(
    beta = c(0.35, 0.65),
    term = "conditionimpl",
    rep = 1:5
  ) |>
    dplyr::mutate(estimate = beta + 0.1)

  out <- df |>
    dplyr::group_by(beta, term) |>
    dplyr::summarise(
      bias = eval_bias(estimate, term = c(conditionimpl = beta)),
      .groups = "drop"
    )

  expect_equal(out$bias, c(0.1, 0.1))
})

test_that("eval_bias() mixes literal and grouping-variable term values", {
  df <- tidyr::expand_grid(
    beta = c(0.35, 0.65),
    term = c("(Intercept)", "conditionimpl"),
    rep = 1:5
  ) |>
    dplyr::mutate(
      estimate = dplyr::if_else(term == "conditionimpl", beta + 0.1, 0.2)
    )

  out <- df |>
    dplyr::group_by(beta, term) |>
    dplyr::summarise(
      bias = eval_bias(
        estimate,
        term = c("(Intercept)" = 0, conditionimpl = beta)
      ),
      .groups = "drop"
    )

  expect_equal(out$bias[out$term == "conditionimpl"], c(0.1, 0.1))
  expect_equal(out$bias[out$term == "(Intercept)"], c(0.2, 0.2))
})

test_that("eval_greater_than() resolves thresholds from grouping variables", {
  df <- tidyr::expand_grid(
    cutoff = c(2, 4),
    term = "a",
    estimate = 1:5
  )

  out <- df |>
    dplyr::group_by(cutoff, term) |>
    dplyr::summarise(
      prop = eval_greater_than(estimate, term = c(a = cutoff)),
      .groups = "drop"
    )

  expect_equal(out$prop, c(3 / 5, 1 / 5))
})

test_that("eval_less_than() resolves thresholds from grouping variables", {
  df <- tidyr::expand_grid(
    cutoff = c(2, 4),
    term = "a",
    estimate = 1:5
  )

  out <- df |>
    dplyr::group_by(cutoff, term) |>
    dplyr::summarise(
      prop = eval_less_than(estimate, term = c(a = cutoff)),
      .groups = "drop"
    )

  expect_equal(out$prop, c(1 / 5, 3 / 5))
})

test_that("eval_between() resolves bounds from grouping variables", {
  df <- tidyr::expand_grid(
    center = c(2, 4),
    term = "a",
    estimate = 1:5
  )

  out <- df |>
    dplyr::group_by(center, term) |>
    dplyr::summarise(
      prop = eval_between(estimate, term = list(a = c(center - 1, center + 1))),
      .groups = "drop"
    )

  expect_equal(out$prop, c(3 / 5, 3 / 5))
})

test_that("eval_quantile() resolves probabilities from grouping variables", {
  df <- tidyr::expand_grid(
    p = c(0.25, 0.75),
    term = "a",
    estimate = 1:5
  )

  out <- df |>
    dplyr::group_by(p, term) |>
    dplyr::summarise(
      q = eval_quantile(estimate, term = c(a = p)),
      .groups = "drop"
    )

  expect_equal(out$q, c(2, 4))
})

test_that("eval_bias() works when spliced through evaluate_model_results()", {
  df <- tidyr::expand_grid(
    beta = c(0.35, 0.65),
    term = "conditionimpl",
    rep = 1:5
  ) |>
    dplyr::mutate(
      estimate = beta + 0.1,
      std.error = 0.1,
      p.value = 0.01
    )

  out <- df |>
    dplyr::group_by(beta, term) |>
    evaluate_model_results(
      bias = eval_bias(estimate, term = c(conditionimpl = beta))
    ) |>
    dplyr::ungroup()

  expect_equal(out$bias, c(0.1, 0.1))
  expect_equal(out$power, c(1, 1))
})

# Guardrails -------------------------------------------------------------------

test_that("eval_bias() aborts helpfully when `term` references a non-grouping column", {
  df <- tibble::tibble(
    term = "a",
    estimate = c(1, 2, 3, 4),
    truth = c(1, 1, 2, 2)
  )

  expect_error(
    df |>
      dplyr::group_by(term) |>
      dplyr::summarise(bias = eval_bias(estimate, term = c(a = truth))),
    regexp = "group_by"
  )
})

test_that("eval_bias() with `term` requires a grouped context", {
  df <- tibble::tibble(estimate = 1:4)

  expect_error(
    df |> dplyr::summarise(bias = eval_bias(estimate, term = c(a = 1))),
    regexp = "grouped"
  )
})

test_that("eval_between() aborts helpfully when a bound is not constant in the group", {
  df <- tibble::tibble(
    term = "a",
    estimate = c(1, 2, 3, 4),
    lo = c(0, 0, 1, 1)
  )

  expect_error(
    df |>
      dplyr::group_by(term) |>
      dplyr::summarise(prop = eval_between(estimate, term = list(a = c(lo, 5)))),
    regexp = "group_by"
  )
})
