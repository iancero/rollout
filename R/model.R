#' @export
fit_model <- function(.data, model){
  .data |>
    tidyr::nest(.by = ".sample") |>
    dplyr::mutate(
      .fit = purrr::map(data, ~ lmerTest::lmer(model, data = .x))
    )
}

