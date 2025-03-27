add_fixed_effect <- function(design_df, ...) {
  dots <- rlang::enquos(...)

  if (any(names(dots) == "")) {
    stop("All fixed effects must be named.")
  }

  # Prefix all new variables with '.'
  new_vars <- purrr::imap(dots, ~ rlang::expr(!!rlang::sym(paste0(".", .y)) := !!.x))

  design_df %>%
    dplyr::mutate(!!!new_vars)
}
