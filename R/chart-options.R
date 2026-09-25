# Chart options: the object, its validation, and how several are combined. The field list is in chart-options-spec.R;
# applying them to a plot is chart-options-apply.R (+ -theme, -marks, -scales).

#' Chart options
#'
#' Describes what a user can change about a chart: its texts, fonts, sizes, colours, angles, legend, axes, grid,
#' backgrounds and how its lines, points and labels are drawn. Every plot method in the package accepts one through its
#' `options` argument (and any of these fields as a named argument in `...`), and an app can keep them per chart, so
#' the choices survive reloads.
#'
#' A field left `NULL` means "keep what the plot draws by default"; only what is set is changed. Text fields accept `""`,
#' which hides that text and the space it took (as the `show_*` fields do). Colours are anything R understands (`"red"`, `"#1b7837"`, `"transparent"`). Sizes are in points
#' unless said otherwise. `x_*` and `y_*` always mean the data's x and y aesthetics, so they follow the data when the chart
#' is flipped.
#' @param title,subtitle,caption,tag Chart texts.
#' @param x_title,y_title Axis titles.
#' @param legend_title Legend title.
#' @param title_wrap Wrap the title, subtitle and caption after this many characters.
#'
#' @param font_family A font family available on the machine drawing the chart (e.g. `"Arial"`, `"serif"`).
#' @param text_scale A multiplier for every text size in the chart (`1.25` = 25 percent larger). Applied before the
#'   explicit sizes below.
#' @param line_height Line spacing of text (a multiple of the font size, default about `0.9`).
#' @param title_size,subtitle_size,caption_size,axis_title_size,axis_text_size,legend_title_size,legend_text_size,strip_text_size
#'   Text sizes in points. `axis_*` sets both axes; use the `x_`/`y_` ones below to set one.
#' @param x_title_size,y_title_size,x_text_size,y_text_size Size of one axis' title or tick labels.
#' @param title_face,subtitle_face,caption_face,axis_title_face,axis_text_face,legend_title_face,legend_text_face,strip_text_face
#'   One of `"plain"`, `"bold"`, `"italic"`, `"bold.italic"`.
#' @param text_color Colour of all text; the `*_color` fields below override it for one kind of text.
#' @param title_color,subtitle_color,caption_color,axis_title_color,axis_text_color,legend_title_color,legend_text_color,strip_text_color
#'   Colour of one kind of text.
#'
#' @param title_align,subtitle_align,caption_align One of `"left"`, `"center"`, `"right"`.
#' @param title_position,caption_position `"panel"` aligns the text with the plotting area, `"plot"` with the whole chart.
#' @param x_text_angle,y_text_angle Angle of the tick labels in degrees (e.g. `45`, `90`); the alignment is adjusted so the
#'   labels stay attached to the axis.
#' @param plot_margin Space around the chart in points: one number, or four (top, right, bottom, left).
#'
#' @param legend_position One of `"right"`, `"bottom"`, `"top"`, `"left"`, `"none"`.
#' @param legend_direction `"horizontal"` or `"vertical"`.
#' @param legend_justification Where the legend sits along its side: `"center"`, `"top"`, `"bottom"`, `"left"`, `"right"`.
#' @param legend_ncol,legend_nrow Number of columns or rows the legend entries are laid out in.
#' @param legend_reverse `TRUE` to reverse the order of the legend entries.
#' @param legend_key_size Size of the legend keys in millimetres.
#' @param legend_background Fill colour of the legend box.
#' @param legend_labels A named character vector (or list): names are the legend entries the plot draws (or the text
#'   currently shown for them), values the text to show instead.
#' @param legend_label_wrap Wrap the legend entries after this many characters.
#'
#' @param category_labels The same as `legend_labels` for the categories on a discrete axis.
#' @param category_label_wrap Wrap the category labels after this many characters.
#' @param x_limits,y_limits Two numbers (`NA` leaves an end open): the range to show. Zooms; data is not dropped.
#' @param x_labels,y_labels How numbers on a continuous axis are written: `"number"`, `"comma"`, `"percent"` (0.5 becomes
#'   50%), `"percent_points"` (50 becomes 50%), `"scientific"`, `"compact"` (1.2K, 3M).
#' @param axis_line `TRUE`/`FALSE` to draw or hide the axis lines.
#' @param axis_line_color Colour of the axis lines.
#' @param axis_ticks `TRUE`/`FALSE` to draw or hide the tick marks.
#' @param flip `TRUE` to swap the axes, `FALSE` to keep the chart as drawn, `NULL` for automatic.
#'
#' @param grid Which grid lines to show: `"both"`, `"horizontal"`, `"vertical"` or `"none"`.
#' @param grid_minor `TRUE`/`FALSE` to show or hide the minor grid lines.
#' @param grid_color,grid_linewidth,grid_linetype Style of the grid lines (`linetype` is one of `"solid"`, `"dashed"`,
#'   `"dotted"`, `"dotdash"`, `"longdash"`, `"twodash"`).
#' @param panel_border `TRUE`/`FALSE` to draw or hide a border around the plotting area.
#' @param panel_border_color Colour of that border.
#' @param panel_color Fill colour of the plotting area.
#' @param background_color Fill colour of the whole chart.
#' @param strip_background Fill colour of facet strips.
#' @param theme_preset Start from a different look before the other options are applied: `"grey"`, `"minimal"`,
#'   `"classic"`, `"bw"`, `"light"`, `"linedraw"`, `"dark"`, `"void"`. It replaces the plot's own theme.
#'
#' @param colors A named character vector of colours for a colour or fill legend (names are entries as in
#'   `legend_labels`); entries not named keep their colour.
#' @param line_scale,point_scale Multiply the width of lines and the size of points (`1.5` = 50 percent bigger).
#' @param alpha Transparency of every mark, from `0` (invisible) to `1`.
#' @param bar_width Width of the bars (and boxes) as a share of the space each has, above `0` and up to `1`
#'   (`0.9` is ggplot2's default).
#' @param label_size Size in points of text drawn on the chart (data labels).
#' @param label_angle,label_color Angle and colour of that text.
#'
#' @param facet_ncol,facet_nrow For a chart split into panels with `facet_wrap()`: the number of columns or rows of panels
#'   (a whole number from 1 to 20). Setting only one lets ggplot2 work out the other. Charts split with `facet_grid()` keep
#'   their layout (one row or column per value).
#' @param facet_scales Whether panels share their axes: `"fixed"` (all the same), `"free"` (each its own), `"free_x"`
#'   or `"free_y"` (only that axis varies). Ignored on maps and other charts whose coordinates cannot vary by panel.
#' @param strip_position Where the panel labels (strips) go: `"top"`, `"bottom"`, `"left"` or `"right"`. With
#'   `facet_grid()`, `"top"`/`"bottom"` place the column labels and `"left"`/`"right"` the row labels.
#'   Use [chart_facet_info()] to see whether a chart has panels at all.
#'
#' @param show_title,show_subtitle,show_caption,show_x_title,show_y_title,show_x_text,show_y_text,show_legend,show_legend_title,show_labels,show_strips
#'   Show or hide one element of the chart: the title, subtitle, caption, an axis title, an axis' tick labels
#'   (`show_x_text`, `show_y_text`), the whole legend, the legend's title, the data labels drawn on the chart
#'   (`geom_text()`, `geom_label()` and their ggrepel versions) or the panel names of a chart split into panels
#'   (`show_strips`). `FALSE` hides it and leaves no space for it; `TRUE` shows it again where the chart itself hid it
#'   (a legend the chart put at `"none"` goes to the right, a text the chart's theme blanked is drawn), which only works
#'   when the chart has that text at all; `NULL` keeps what the chart draws. An empty text (`title = ""`, `x_title = ""`,
#'   `legend_title = ""`...) hides that element the same way.
#'
#' @param ... The names older plot methods used: `x_axis`, `x_label` (= `x_title`), `y_axis`, `y_label` (= `y_title`),
#'   `legend`, `fill_label` (= `legend_title`), `colours` (= `colors`).
#'
#' @return An object of class `cd_chart_options` (a list of the fields that are set).
#'
#' @examples
#' cd_chart_options(title = "Coverage", legend_position = "bottom", text_scale = 1.2)
#' cd_chart_options(x_axis = "Year", legend_labels = c(dhis2 = "Routine data"))
#' cd_chart_options(x_text_angle = 45, grid = "horizontal", grid_color = "grey90", y_labels = "percent_points")
#'
#' @seealso [apply_chart_options()], [merge_chart_options()]
#' @export
# `...` comes first so that every field below must be named in full: with `...` after them, R would match `legend =` (an
# older name) to whichever of legend_title / legend_position / legend_labels it happens to prefix.
cd_chart_options <- function(..., title = NULL, subtitle = NULL, caption = NULL, tag = NULL,
                             x_title = NULL, y_title = NULL, legend_title = NULL, title_wrap = NULL,
                             font_family = NULL, text_scale = NULL, line_height = NULL,
                             title_size = NULL, subtitle_size = NULL, caption_size = NULL,
                             axis_title_size = NULL, axis_text_size = NULL,
                             x_title_size = NULL, y_title_size = NULL, x_text_size = NULL, y_text_size = NULL,
                             legend_title_size = NULL, legend_text_size = NULL, strip_text_size = NULL,
                             title_face = NULL, subtitle_face = NULL, caption_face = NULL,
                             axis_title_face = NULL, axis_text_face = NULL,
                             legend_title_face = NULL, legend_text_face = NULL, strip_text_face = NULL,
                             text_color = NULL, title_color = NULL, subtitle_color = NULL, caption_color = NULL,
                             axis_title_color = NULL, axis_text_color = NULL,
                             legend_title_color = NULL, legend_text_color = NULL, strip_text_color = NULL,
                             title_align = NULL, subtitle_align = NULL, caption_align = NULL,
                             title_position = NULL, caption_position = NULL,
                             x_text_angle = NULL, y_text_angle = NULL, plot_margin = NULL,
                             legend_position = NULL, legend_direction = NULL, legend_justification = NULL,
                             legend_ncol = NULL, legend_nrow = NULL, legend_reverse = NULL,
                             legend_key_size = NULL, legend_background = NULL,
                             legend_labels = NULL, legend_label_wrap = NULL,
                             category_labels = NULL, category_label_wrap = NULL,
                             x_limits = NULL, y_limits = NULL, x_labels = NULL, y_labels = NULL,
                             axis_line = NULL, axis_line_color = NULL, axis_ticks = NULL, flip = NULL,
                             grid = NULL, grid_minor = NULL, grid_color = NULL, grid_linewidth = NULL,
                             grid_linetype = NULL, panel_border = NULL, panel_border_color = NULL,
                             panel_color = NULL, background_color = NULL, strip_background = NULL,
                             theme_preset = NULL,
                             colors = NULL, line_scale = NULL, point_scale = NULL, alpha = NULL, bar_width = NULL,
                             label_size = NULL, label_angle = NULL, label_color = NULL,
                             facet_ncol = NULL, facet_nrow = NULL, facet_scales = NULL, strip_position = NULL,
                             show_title = NULL, show_subtitle = NULL, show_caption = NULL,
                             show_x_title = NULL, show_y_title = NULL, show_x_text = NULL, show_y_text = NULL,
                             show_legend = NULL, show_legend_title = NULL, show_labels = NULL, show_strips = NULL) {
  opts <- mget(setdiff(names(formals()), "..."))

  extra <- list(...)
  if (length(extra)) {
    extra_names <- names(extra) %||% rep("", length(extra))
    bad <- extra_names[!extra_names %in% names(.chart_option_aliases)]
    if (length(bad)) {
      .ds_abort(c("x" = "Unknown chart option: {.field {bad}}.",
                 "i" = "See {.code ?cd_chart_options} for the options that exist."))
    }
    for (name in names(extra)) {
      target <- .chart_option_aliases[[name]]
      if (is.null(opts[[target]])) opts[[target]] <- extra[[name]]
    }
  }

  opts <- Filter(Negate(is.null), opts)
  structure(.validate_chart_options(opts), class = "cd_chart_options")
}

.is_color <- function(x) {
  is_scalar_character(x) && !is.na(x) && !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}

.validate_chart_options <- function(opts) {
  bad <- setdiff(names(opts), .chart_option_fields)
  if (length(bad)) .ds_abort(c("x" = "Unknown chart option: {.field {bad}}."))

  for (name in names(opts)) {
    spec <- .chart_option_spec[[name]]
    value <- opts[[name]]
    ok <- switch(
      spec$type,
      text = is_scalar_character(value) && !is.na(value),
      positive = is.numeric(value) && length(value) == 1 && !is.na(value) && value > 0,
      number = is.numeric(value) && length(value) == 1 && !is.na(value),
      fraction = is.numeric(value) && length(value) == 1 && !is.na(value) && value > 0 && value <= 1,
      count = is.numeric(value) && length(value) == 1 && !is.na(value) && value >= 1 && value == round(value),
      panel_count = is.numeric(value) && length(value) == 1 && !is.na(value) && value >= 1 && value <= 20 &&
        value == round(value),
      logical = is.logical(value) && length(value) == 1 && !is.na(value),
      color = .is_color(value),
      limits = is.numeric(value) && length(value) == 2,
      margin = is.numeric(value) && length(value) %in% c(1, 4) && !anyNA(value) && all(value >= 0),
      choice = {
        if (identical(value, "centre")) value <- opts[[name]] <- "center"
        if (identical(value, "gray")) value <- opts[[name]] <- "grey"
        is_scalar_character(value) && value %in% spec$choices
      },
      named_text = {
        value <- unlist(value)
        is.character(value) && !is.null(names(value)) && all(nzchar(names(value)))
      },
      named_color = {
        value <- unlist(value)
        is.character(value) && !is.null(names(value)) && all(nzchar(names(value))) && all(vapply(value, .is_color, logical(1)))
      },
      FALSE
    )
    if (!isTRUE(ok)) {
      expects <- .chart_option_expects_text(spec)
      .ds_abort(c("x" = "{.arg {name}} {expects}"))
    }
  }
  invisible(opts)
}

# Message fragment: what a field must be
.chart_option_expects_text <- function(spec) {
  switch(
    spec$type,
    text = "must be a single string or NULL.",
    positive = "must be a single positive number or NULL.",
    number = "must be a single number or NULL.",
    fraction = "must be a number above 0 and up to 1, or NULL.",
    count = "must be a single whole number of 1 or more, or NULL.",
    panel_count = "must be a single whole number from 1 to 20, or NULL.",
    logical = "must be TRUE, FALSE or NULL.",
    color = "must be a single colour (a name such as \"red\", or \"#rrggbb\") or NULL.",
    limits = "must be two numbers (NA leaves an end open) or NULL.",
    margin = "must be one number, or four (top, right, bottom, left), of 0 or more, or NULL.",
    choice = paste0("must be one of ", paste0("\"", spec$choices, "\"", collapse = ", "), " or NULL."),
    named_text = "must be a named character vector (names = the entries to change).",
    named_color = "must be a named vector of colours (names = the entries to change).",
    "is not valid."
  )
}

#' Chart options from what a caller hands in
#'
#' Normalises `NULL`, a list of fields, or a [cd_chart_options()] object to a `cd_chart_options` object.
#' @param x `NULL`, a list, or a `cd_chart_options` object.
#' @return A `cd_chart_options` object.
#' @keywords internal
#' @export
as_chart_options <- function(x) {
  if (is.null(x)) return(structure(list(), class = "cd_chart_options"))
  if (inherits(x, "cd_chart_options")) return(x)
  if (is.list(x)) return(do.call(cd_chart_options, x))
  .ds_abort(c("x" = "{.arg options} must be NULL, a list, or a {.cls cd_chart_options} object."))
}

#' Combine chart options
#'
#' Later arguments win, field by field; `legend_labels`, `category_labels` and `colors` combine name by name. Use it to
#' layer a dataset's defaults, a chart's saved options and what the caller passes now.
#'
#' @param ... `cd_chart_options` objects, lists of fields, or `NULL`s.
#'
#' @return A `cd_chart_options` object.
#'
#' @examples
#' merge_chart_options(cd_chart_options(font_family = "serif", title = "A"), cd_chart_options(title = "B"))
#'
#' @export
merge_chart_options <- function(...) {
  merged <- list()
  for (item in list(...)) {
    item <- as_chart_options(item)
    for (name in names(item)) {
      merged[[name]] <- if (name %in% .chart_option_named) {
        old <- unlist(merged[[name]])
        new <- unlist(item[[name]])
        c(old[setdiff(names(old), names(new))], new)
      } else {
        item[[name]]
      }
    }
  }
  structure(merged, class = "cd_chart_options")
}

#' Resolve the chart options for a plot
#'
#' What every plot method calls with its `options` argument and its `...`: `options` (an object, a list or `NULL`) plus any
#' chart-option field named in `...` (unrelated arguments in `...` are ignored). Named arguments in `...` win.
#'
#' @param options A `cd_chart_options` object, a list of fields, or `NULL`.
#' @param ... Chart-option fields (or their older aliases).
#'
#' @return A `cd_chart_options` object.
#' @export
resolve_chart_options <- function(options = NULL, ...) {
  dots <- list(...)
  known <- c(.chart_option_fields, names(.chart_option_aliases))
  dots <- dots[names(dots) %in% known]
  merge_chart_options(options, if (length(dots)) do.call(cd_chart_options, dots))
}

#' The type of graph a plot is
#'
#' A key made from the geoms a ggplot is built from (lines and points, bars, tiles...), used for a chart that has no
#' id of its own.
#'
#' @param p A ggplot.
#'
#' @return A string such as `"geom:Errorbar+Line+Point"`, or `NULL` when `p` is not a ggplot with layers.
#' @export
cd_chart_type <- function(p) {
  if (!inherits(p, "ggplot") || !length(p$layers)) return(NULL)
  geoms <- vapply(p$layers, function(layer) sub("^Geom", "", class(layer$geom)[[1]]), character(1))
  paste0("geom:", paste(sort(unique(geoms)), collapse = "+"))
}

#' @export
print.cd_chart_options <- function(x, ...) {
  if (length(x) == 0) {
    cat("<cd_chart_options> (nothing set: every chart keeps its own look)\n")
  } else {
    cat("<cd_chart_options>\n")
    for (name in names(x)) {
      value <- x[[name]]
      shown <- if (length(value) > 1 || !is.null(names(value))) paste0(names(value), " = ", unlist(value), collapse = "; ") else as.character(value)
      cat(sprintf("  %-20s %s\n", name, shown))
    }
  }
  invisible(x)
}

# `$` on a plain list matches partially (`o$title` would find `title_size`), so options use exact matching
#' @export
`$.cd_chart_options` <- function(x, name) .subset2(x, name)

