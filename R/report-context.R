# What the report builder needs from an app, in two parts:
#
#   the registry (report_register()): what the app offers, the same for every dataset - its themes and default theme,
#     the cover's defaults, the kinds of chart and table, the standard reports, indicator names and extra fields.
#     An app registers them once, when its package loads.
#   the context (report_context()): one dataset's data - drawing a block, the pictures and templates kept with it,
#     the chart options saved in it, the values of its fields, its years and regions, a flag for the cover.
#
# Every report function takes a context as its first argument, or anything as_report_context() turns into one (an app
# adds a method for its own dataset class, as cd2030.core does for CacheConnection).

.ds_report <- new.env(parent = emptyenv())

#' Register what an app's reports offer
#'
#' Called once by an app package (usually in its `.onLoad()`); later calls replace only what they give.
#'
#' @param themes A named list of designs (see [report_default_design()]), each with `name` (a string, or a list of
#'   `en`, `fr`, `pt`): offered beside the built-in themes, first.
#' @param default_theme The id of the theme new reports start from.
#' @param cover A list of cover fields that new reports start from (see [report_default_cover()]), e.g. a `kicker`.
#' @param kinds A function returning the kinds of chart and table, a named list of [report_kind()].
#' @param presets A function of `lang` returning the standard reports (see the app's documentation of its own).
#' @param indicator_name A function of `indicator` and `i18n` returning an indicator's name in the report's language,
#'   for `{chart_indicator}`.
#' @param fields A list of extra fields for [report_field_catalog()], each `list(key, group, label)`; their values come
#'   from the report context's `fields()`.
#' @param chart_id A function of a chart's data and its ggplot returning the chart's id: the key its options are kept
#'   under, for the screen and for reports (the same chart gets the same id in both). By default, [cd_chart_type()].
#' @return Invisibly, `NULL`.
#' @export
report_register <- function(themes = NULL, default_theme = NULL, cover = NULL, kinds = NULL, presets = NULL,
                            indicator_name = NULL, fields = NULL, chart_id = NULL) {
  given <- list(themes = themes, default_theme = default_theme, cover = cover, kinds = kinds, presets = presets,
                indicator_name = indicator_name, fields = fields, chart_id = chart_id)
  for (k in names(given)) if (!is.null(given[[k]])) assign(k, given[[k]], envir = .ds_report)
  invisible(NULL)
}

# One registered part, or `default`
.ds_registered <- function(key, default = NULL) get0(key, envir = .ds_report, inherits = FALSE) %||% default

# The registered kinds of chart and table (a named list; empty when the app gave none)
.ds_report_kinds <- function() {
  kinds <- .ds_registered("kinds")
  if (is.function(kinds)) tryCatch(kinds(), error = function(e) list()) else list()
}

# The registered standard reports in `lang` (an empty list when the app gave none)
.ds_report_presets <- function(lang = "en") {
  presets <- .ds_registered("presets")
  if (is.function(presets)) tryCatch(presets(lang), error = function(e) list()) else list()
}

# A chart's id (the app's registered function; else its type of graph)
.ds_chart_id <- function(data, plot) {
  f <- .ds_registered("chart_id")
  id <- if (is.function(f)) tryCatch(f(data, plot), error = function(e) NULL)
  id %||% cd_chart_type(plot)
}

#' One kind of chart or table
#'
#' @param type `"chart"` or `"table"`.
#' @param group The group it is shown under in the builder.
#' @param label Its name.
#' @param indicators The indicators it can be drawn for (a character vector, an app-defined keyword, or `NULL`).
#' @param levels,variants The levels (e.g. national, regional) and variants it offers.
#' @param year Whether a year is chosen.
#' @param regional Whether it is drawn for one region.
#' @param groups The app's groups of indicators it is for.
#' @param tall Whether it is drawn taller than wide (maps, charts with a panel per district), see [report_block_size()].
#' @return A list.
#' @export
report_kind <- function(type, group, label, indicators = NULL, levels = NULL, variants = NULL, year = FALSE, regional = FALSE,
                        groups = NULL, tall = FALSE) {
  list(type = type, group = group, label = label, indicators = indicators, levels = levels, variants = variants, year = year,
       regional = regional, groups = groups, tall = tall)
}

#' A report context: one dataset's side of the report builder
#'
#' Each argument is a function; those left out do nothing (no pictures kept, no saved chart options, no fields).
#'
#' @param draw `function(block, i18n)` returning a ggplot or a flextable for a chart or table block (an error when it
#'   cannot be drawn).
#' @param asset_get `function(id)` returning a kept picture or template, `list(type, data = <raw>)`, or `NULL`.
#' @param asset_set `function(id, value)` keeping one.
#' @param chart_options `function()` returning the chart options saved with the data: a named list, `"default"` for
#'   every chart and one per chart id or type of graph ([cd_chart_type()]).
#' @param fields `function(project, lang)` returning a named list of field values (e.g. `country`).
#' @param years `function()` returning the years the data covers.
#' @param regions `function()` returning the regions a report can be about.
#' @param flag `function()` returning the path of a flag picture for the cover, or `NULL`.
#' @return An object of class `report_context`.
#' @export
report_context <- function(draw = NULL, asset_get = NULL, asset_set = NULL, chart_options = NULL, fields = NULL,
                           years = NULL, regions = NULL, flag = NULL) {
  none <- function(...) NULL
  structure(list(
    draw = draw %||% function(block, i18n) .ds_abort(c("x" = "This report has no data to draw {.val {block$kind %||% block$type}} from.")),
    asset_get = asset_get %||% none,
    asset_set = asset_set %||% function(id, value) .ds_abort(c("x" = "This report cannot keep pictures.")),
    chart_options = chart_options %||% function() list(),
    fields = fields %||% function(project, lang) list(),
    years = years %||% function() integer(),
    regions = regions %||% function() character(),
    flag = flag %||% none
  ), class = "report_context")
}

#' Make a report context
#'
#' What every report function calls on its first argument. A `report_context` is returned as it is and `NULL` gives an
#' empty one; an app adds a method for its own dataset class.
#' @param x A report context, `NULL`, or an object an app has a method for.
#' @param ... Passed to methods.
#' @return A `report_context`.
#' @export
as_report_context <- function(x, ...) UseMethod("as_report_context")

#' @export
as_report_context.report_context <- function(x, ...) x

#' @export
as_report_context.NULL <- function(x, ...) report_context()

#' @export
as_report_context.default <- function(x, ...) {
  .ds_abort(c("x" = "A report needs a report context, not {.cls {class(x)[1]}}.",
              "i" = "See {.fn datasuite.ui::report_context}, or load the package that knows this kind of data."))
}

# An indicator's name in the report's language (the app's registered function; else the indicator as given)
.ds_indicator_name <- function(i18n, indicator) {
  f <- .ds_registered("indicator_name")
  name <- if (is.function(f)) tryCatch(f(indicator, i18n), error = function(e) NULL)
  if (is.character(name) && length(name) == 1 && nzchar(name)) name else indicator
}
