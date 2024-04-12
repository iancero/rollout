test_that("sim_normal_error works", {
  df <- data.frame(x = 1:10)

  expect_equal(
    df |>
      sim_normal_error() |>
      nrow(),
    nrow(df)
  )
})

test_that("sim_normal_error works with custom sd", {
  df <- data.frame(x = 1:10)

  expect_equal(
    df |>
      sim_normal_error(sd = 2) |>
      nrow(),
    nrow(df)
  )
})
