#' @details
#' An app built on datasuite.ui gives it its pages, its data and its report content through small interfaces; the
#' package itself knows nothing about any app's data and never calls an app's own package. The Countdown to 2030 apps
#' are the working example: cd2030.core adds the Countdown pages and report content on top of it, and each app package
#' (cd2030.rmncah, cd2030.vaxx, cd2030.pooled) has a `run_app()` built from both.
#'
#' @section The app frame and components:
#' [app_frame()] builds a whole app: the page, the sidebar and header, and the server. An app gives it a page registry
#' ([cd_page_def()], [cd_use_pages()]), a nav tree ([cd_nav_section()], [cd_nav_item()]) and a `data_server` returning
#' its dataset. A page is a module whose UI is [cd_page_ui()], made of cards ([cd_card()], [cd_chart_card()]), charts
#' with their download and Customize tools ([cd_plot_ui()], [cd_plot_server()]), inputs, dialogs and feedback. Every
#' kit function is listed in `?"interface-kit"`; the package README (the dataset contract, a minimal app) and
#' `docs/COMPONENTS.md` in the repository describe each one.
#'
#' @section Chart options:
#' [cd_chart_options()] describes how a ggplot2 chart should look (texts, fonts, colours, axes, legend, grid, facets,
#' marks, and showing or hiding each element) and [apply_chart_options()] applies it to a finished chart.
#' [merge_chart_options()], [resolve_chart_options()], [chart_option_fields()] and [cd_chart_type()] support them.
#'
#' @section Reports:
#' A report is blocks on pages with a theme and a cover, edited in the browser (the Reports page, [reports_ui()] /
#' [reports_server()]) and exported with [export_report()] (Word, PDF) or [export_deck()] (PowerPoint, PDF). An app
#' registers what its reports offer once with [report_register()] (kinds of chart and table, standard reports, themes,
#' fields) and gives the engine one dataset's data as a [report_context()], or through an [as_report_context()] method
#' for its own dataset class. Themes: [report_themes()], [report_theme_from_file()]; fields: [report_field_catalog()].
#'
#' @section Translations:
#' Text is a translation key. [cd_translations()] merges the kit's keys, the files packages registered with
#' [cd_register_translations()] and the app's own file for `shiny.i18n::init_i18n()`; [cd_use_i18n()] makes the
#' translator the one the components use.
#'
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
