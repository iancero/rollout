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
  # Save original grouping
  original_groups <- dplyr::group_vars(.data)

  .data <- .data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.error = rnorm(n(), sd = sqrt(variance)))

  # Restore original grouping
  if (length(original_groups) > 0) {
    .data <- .data %>%
      dplyr::group_by(dplyr::across(all_of(original_groups)))
  }

  .data
}

add_linear_outcome <- function(data, output_col = ".y_linear") {
  dot_cols <- names(data)[startsWith(names(data), ".")]

  if (length(dot_cols) == 0) {
    stop("No effect columns found (no columns starting with '.')")
  }

  data %>%
    dplyr::mutate(!!output_col := rowSums(dplyr::pick(all_of(dot_cols))))
}

add_binary_outcome <- function(data,
                                 linear_col = ".y_linear",
                                 prob_col = ".y_prob",
                                 binary_col = ".y_bin") {

  dot_cols <- names(data)[startsWith(names(data), ".")]

  if (length(dot_cols) == 0) {
    stop("No effect columns found (no columns starting with '.')")
  }

  data %>%
    dplyr::mutate(
      !!linear_col := rowSums(dplyr::pick(all_of(dot_cols))),
      !!prob_col   := plogis(.data[[linear_col]]),
      !!binary_col := rbinom(dplyr::n(), size = 1, prob = .data[[prob_col]])
    )
}

add_poisson_outcome <- function(data,
                                linear_col = ".y_linear",
                                rate_col = ".y_rate",
                                count_col = ".y_count") {

  dot_cols <- names(data)[startsWith(names(data), ".")]
  if (length(dot_cols) == 0) stop("No effect columns found.")

  data %>%
    dplyr::mutate(
      !!linear_col := rowSums(dplyr::pick(all_of(dot_cols))),
      !!rate_col := exp(.data[[linear_col]]),
      !!count_col := rpois(dplyr::n(), lambda = .data[[rate_col]])
    )
}



