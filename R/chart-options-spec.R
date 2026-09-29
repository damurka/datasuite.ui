# Chart options: one object that describes everything about a chart's look that a user may change, and the one place that
# applies it. Every plot method takes `options = NULL` (and passes its `...` to resolve_chart_options()), so every chart is
# editable the same way, and CacheConnection can store what the user chose (see CacheConnection$set_chart_options()).
#
# `.chart_option_spec()` describes the fields: their type, and (for choices) the allowed values. cd_chart_options()
# validates against it and apply_chart_options() reads the validated result.

# The options' types and choices come from Quire's contract (quire::quire_chart_options()), which the report builder and
# every other host share: an option is added there, and here to cd_chart_options()' arguments and to the code that
# applies it. Read once, when first needed (not when the package is built, so a newer quire is used as soon as it is
# installed).
.chart_spec_cache <- new.env(parent = emptyenv())

.chart_option_spec <- function() {
  if (is.null(.chart_spec_cache$spec)) {
    .chart_spec_cache$spec <- lapply(quire::quire_chart_options()$options, function(o) {
      list(type = o$type, choices = if (!is.null(o$choices)) unlist(o$choices))
    })
  }
  .chart_spec_cache$spec
}

.chart_option_fields <- function() names(.chart_option_spec())

#' The names of the chart options
#'
#' Every field [cd_chart_options()] accepts, for code that keeps only the chart options out of a list of settings.
#' @return A character vector.
#' @export
chart_option_fields <- function() .chart_option_fields()

# Names some plot methods have always used for the same thing -> the option they mean
.chart_option_aliases <- c(
  x_axis = "x_title", x_label = "x_title",
  y_axis = "y_title", y_label = "y_title",
  legend = "legend_title", fill_label = "legend_title",
  colours = "colors"
)

# The named-vector options merge name by name; all others are replaced
.chart_option_named <- c("legend_labels", "category_labels", "colors")
