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

  # Save original grouping
  original_groups <- dplyr::group_vars(design_df)

  # Apply nesting groups temporarily (if any)
  if (!is.null(.nesting)) {
    design_df <- design_df %>%
      dplyr::group_by(dplyr::across(all_of(.nesting)))
  }

  # Add random effect and restore original grouping
  design_df <- design_df %>%
    dplyr::mutate(!!var_name := !!var_expr)

  # Restore original grouping
  if (length(original_groups) > 0) {
    design_df <- design_df %>%
      dplyr::group_by(dplyr::across(all_of(original_groups)))
  } else {
    design_df <- design_df %>% dplyr::ungroup()
  }

  design_df
}

add_error <- function(.data, variance = 1) {
  .data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.error = rnorm(n(), sd = sqrt(variance)))
}
