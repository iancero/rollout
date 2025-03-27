add_fixed_effect <- function(design_df, ...) {
  dots <- rlang::enquos(...)

  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the fixed effect to add.")
  }

  var_name <- paste0(".", names(dots)[1])
  var_expr <- dots[[1]]

  design_df %>%
    dplyr::mutate(!!var_name := !!var_expr)
}

add_random_effect <- function(design_df, ..., .nesting = NULL) {
  dots <- rlang::enquos(...)

  if (length(dots) != 1) {
    stop("Please provide exactly one named argument for the random effect to add.")
  }

  var_name <- paste0(".", names(dots)[1])
  var_expr <- dots[[1]]

  # Group if .nesting is provided
  if (!is.null(.nesting)) {
    design_df <- design_df %>%
      dplyr::group_by(dplyr::across(all_of(.nesting)))
  }

  design_df %>%
    dplyr::mutate(!!var_name := !!var_expr) %>%
    dplyr::ungroup()
}
