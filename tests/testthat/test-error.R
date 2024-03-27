test_that("sim_normal_error works", {
  expect_equal(
    mtcars |>
      sim_normal_error() |>
      dplyr::pull(.error) |>
      length(),
    nrow(mtcars)
  )
})

test_that("sim_normal_error works with custom sd", {
  expect_equal(
    mtcars |>
      sim_normal_error(sd = 2) |>
      dplyr::pull(.error) |>
      length(),
    nrow(mtcars)
  )
})
