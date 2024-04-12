#' @export
eval_design <- function(.data, feature = "power"){
  .data |>
    dplyr::mutate(.results = purrr::map(.fit, broom.mixed::tidy)) |>
    tidyr::unnest(.results) |>
    dplyr::filter(effect == "fixed") |>
    dplyr::group_by(term) |>
    dplyr::summarize(power = mean(p.value < 0.05)) |>
    dplyr::ungroup()
}
