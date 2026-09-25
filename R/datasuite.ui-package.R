#' @keywords internal
#' @import ggplot2
#' @importFrom rlang is_scalar_character arg_match
"_PACKAGE"

# The package's small helpers.

`%||%` <- function(x, y) if (is.null(x)) y else x

# An error for a caller of the package (cli formatting, the caller's call shown)
.ds_abort <- function(message, ..., class = NULL, .envir = parent.frame(), call = rlang::caller_env()) {
  cli::cli_abort(message = message, ..., class = c(class, "datasuite_error"), .envir = .envir, call = call)
}

# A warning for a caller of the package
.ds_warn <- function(message, ..., .envir = parent.frame(), call = rlang::caller_env()) {
  cli::cli_warn(message = message, ..., .envir = .envir, call = call)
}
