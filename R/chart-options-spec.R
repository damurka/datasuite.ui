# Chart options: one object that describes everything about a chart's look that a user may change, and the one place that
# applies it. Every plot method takes `options = NULL` (and passes its `...` to resolve_chart_options()), so every chart is
# editable the same way, and CacheConnection can store what the user chose (see CacheConnection$set_chart_options()).
#
# `.chart_option_spec` is the single description of the fields: their type, and (for choices) the allowed values.
# cd_chart_options() validates against it and apply_chart_options() reads the validated result.

.chart_choices <- list(
  legend_position = c("right", "bottom", "top", "left", "none"),
  legend_direction = c("horizontal", "vertical"),
  legend_justification = c("center", "top", "bottom", "left", "right"),
  face = c("plain", "bold", "italic", "bold.italic"),
  align = c("left", "center", "right"),
  position = c("panel", "plot"),
  grid = c("both", "horizontal", "vertical", "none"),
  linetype = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
  format = c("number", "comma", "percent", "percent_points", "scientific", "compact"),
  theme_preset = c("grey", "minimal", "classic", "bw", "light", "linedraw", "dark", "void"),
  facet_scales = c("fixed", "free", "free_x", "free_y"),
  strip_position = c("top", "bottom", "left", "right")
)

.chart_show_fields <- c("show_title", "show_subtitle", "show_caption", "show_x_title", "show_y_title", "show_x_text",
                        "show_y_text", "show_legend", "show_legend_title", "show_labels", "show_strips")

#' The names of the chart options
#'
#' Every field [cd_chart_options()] accepts, for code that keeps only the chart options out of a list of settings.
#' @return A character vector.
#' @export
chart_option_fields <- function() .chart_option_fields

.chart_option_spec <- local({
  spec <- list()
  add <- function(names, type, choices = NULL) for (n in names) spec[[n]] <<- list(type = type, choices = choices)

  add(c("title", "subtitle", "caption", "tag", "x_title", "y_title", "legend_title", "font_family"), "text")
  add(c("title_size", "subtitle_size", "caption_size", "axis_title_size", "axis_text_size", "x_title_size", "y_title_size",
        "x_text_size", "y_text_size", "legend_title_size", "legend_text_size", "strip_text_size", "label_size",
        "text_scale", "line_height", "legend_key_size", "grid_linewidth", "line_scale", "point_scale"), "positive")
  add(c("title_face", "subtitle_face", "caption_face", "axis_title_face", "axis_text_face", "legend_title_face",
        "legend_text_face", "strip_text_face"), "choice", .chart_choices$face)
  add(c("text_color", "title_color", "subtitle_color", "caption_color", "axis_title_color", "axis_text_color",
        "legend_title_color", "legend_text_color", "strip_text_color", "label_color", "axis_line_color", "grid_color",
        "background_color", "panel_color", "panel_border_color", "strip_background", "legend_background"), "color")
  add(c("title_align", "subtitle_align", "caption_align"), "choice", .chart_choices$align)
  add(c("title_position", "caption_position"), "choice", .chart_choices$position)
  add(c("x_text_angle", "y_text_angle", "label_angle"), "number")
  add("plot_margin", "margin")
  add("legend_position", "choice", .chart_choices$legend_position)
  add("legend_direction", "choice", .chart_choices$legend_direction)
  add("legend_justification", "choice", .chart_choices$legend_justification)
  add(c("legend_ncol", "legend_nrow", "legend_label_wrap", "category_label_wrap", "title_wrap"), "count")
  add(c("x_limits", "y_limits"), "limits")
  add(c("x_labels", "y_labels"), "choice", .chart_choices$format)
  add("grid", "choice", .chart_choices$grid)
  add("grid_linetype", "choice", .chart_choices$linetype)
  add("theme_preset", "choice", .chart_choices$theme_preset)
  add(c("alpha", "bar_width"), "fraction")
  add(c("legend_reverse", "axis_line", "axis_ticks", "grid_minor", "panel_border", "flip"), "logical")
  # show / hide one element of the chart: TRUE shown, FALSE hidden (no space left for it), NULL as the chart draws it
  add(.chart_show_fields, "logical")
  add(c("legend_labels", "category_labels"), "named_text")
  add("colors", "named_color")
  add(c("facet_ncol", "facet_nrow"), "panel_count")
  add("facet_scales", "choice", .chart_choices$facet_scales)
  add("strip_position", "choice", .chart_choices$strip_position)
  spec
})

.chart_option_fields <- names(.chart_option_spec)

# Names some plot methods have always used for the same thing -> the option they mean
.chart_option_aliases <- c(
  x_axis = "x_title", x_label = "x_title",
  y_axis = "y_title", y_label = "y_title",
  legend = "legend_title", fill_label = "legend_title",
  colours = "colors"
)

# The named-vector options merge name by name; all others are replaced
.chart_option_named <- c("legend_labels", "category_labels", "colors")
