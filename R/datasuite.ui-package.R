#' @keywords internal
#' @import ggplot2
#' @import shiny
#' @importFrom rlang is_scalar_character arg_match as_function eval_tidy enquo
#' @importFrom htmltools css
#' @importFrom stringr str_glue_data
#' @importFrom purrr map walk
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @rawNamespace exportPattern("^[[:alpha:]]")
"_PACKAGE"

# The kit's stylesheet, fonts, logo and React bundle are served at cd-ui/ (inst/www)
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("cd-ui", system.file("www", package = pkgname))
}

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

# An error's message for the screen: without cli's colours and bullets
.ds_clean_error <- function(error) {
  if (!inherits(error, "error")) return("")
  message <- cli::ansi_strip(conditionMessage(error))
  message <- gsub("[\u2716\u2714\u2139\u26a0!]", "", message)
  message <- gsub("In index: \\d+\\.?", "", message)
  message <- gsub("Caused by error in `.*?\\(\\)`:?", "", message)
  trimws(message)
}
