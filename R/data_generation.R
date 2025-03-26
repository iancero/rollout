#' Title
#'
#' @param a the first number
#' @param b the second number
#'
#' @returns the product
#' @export
#'
#' @examples
#' multiply(2, 2)
multiply = function (a, b) {
  a * b
}


gen_design_df = function (cohorts, sites, units) {
  assertthat::assert_that(length(cohorts) > 0)
  assertthat::assert_that(all(sites > 0))
  assertthat::assert_that(all(units > 0))

  assertthat::assert_that(length(cohorts) == length(sites))
  assertthat::assert_that(sum(sites) == length(units))

  dplyr::tibble(cohort = cohorts, n_sites = sites) |>
    tidyr::uncount(n_sites, .id = "site") |>
    dplyr::mutate(n_units = units) |>
    tidyr::uncount(n_units, .id = "unit")
}
