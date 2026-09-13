#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

# Columns referenced through data masking in `evaluate_model_results()`, plus
# the `conf.low` and `conf.high` defaults of `eval_coverage()`, which are
# resolved from the current group rather than evaluated as symbols
utils::globalVariables(c("conf.high", "conf.low", "estimate", "p.value", "std.error"))
