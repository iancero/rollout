#' @export
evaluate_models <- function(
    models,
    .term = NULL,
    ...,
    model_col = model,
    tidy_fun = broom.mixed::tidy,
    alpha = .05
  ) {

  # Capture the user expressions and the model column
  model_col <- rlang::enquo(model_col)
  summary_exprs <- rlang::enquos(...)

  models <- models %>%
    dplyr::mutate(.results = purrr::map(rlang::eval_tidy(model_col, .), tidy_fun)) %>%
    tidyr::unnest(.results)

  if (!is.null(.term)){
    models <- models |>
      filter(term == .term)
  }

  models |>
    dplyr::summarise(
      n_models = dplyr::n(),
      power = mean(p.value < alpha, na.rm = TRUE),
      !!!summary_exprs
    )

}
