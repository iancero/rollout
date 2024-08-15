fit_models_progress <- function(.data, formula){

  .data <- .data |>
    dplyr::group_split()

  progress_bar <- progressr::progressor(along = .data)

  future.apply::future_lapply(
    X = .data,
    FUN = function(x){
      lmerTest::lmer(formula = formula, data = x)
      progress_bar()
    }
  )

}


fit_models <- function(.data, fitting_func, ...){

  by_indices <- group_indices(.data)

  future.apply::future_by(
    data = .data,
    INDICES = by_indices,
    FUN = fitting_func,
    ...
  )
}

tidy_models <- function(.fitted_models, as_dataframe = TRUE){
  tidied_models <- purrr::map(.fitted_models, broom.mixed::tidy)

  if(as_dataframe){
    tidied_models <- bind_rows(tidied_models, .id = "id")
  }

  tidied_models
}
