#' @export
extract_model_results <- function(
    models,
    model_col = model,
    tidy_fun = broom.mixed::tidy,
    .term = NULL
) {
  model_col <- rlang::enquo(model_col)

  results <- models |>
    dplyr::mutate(.results = purrr::map(rlang::eval_tidy(model_col, models), tidy_fun)) |>
    tidyr::unnest(.results)

  if (!is.null(.term)) {
    results <- results |>
      dplyr::filter(term == .term)
  }

  results
}

#' @export
evaluate_model_results <- function(
    results,
    alpha = 0.05,
    ...,
    .summarise_standard_broom = FALSE,
    broom_cols = c("estimate", "std.error", "statistic", "df", "p.value")
) {
  summary_exprs <- rlang::enquos(...)

  results |>
    dplyr::summarise(
      n_models = dplyr::n(),
      power = mean(p.value < alpha, na.rm = TRUE),
      !!!summary_exprs,
      !!!{
        if (.summarise_standard_broom) {
          rlang::exprs(
            dplyr::across(
              dplyr::all_of(intersect(broom_cols, names(results))),
              list(mean = mean, sd = sd),
              .names = "{fn}_{col}"
            )
          )
        } else {
          rlang::exprs()
        }
      }
    )
}

