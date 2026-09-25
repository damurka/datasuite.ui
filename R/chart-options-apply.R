# Applying chart options to a ggplot: apply_chart_options() is the one entry point and runs the steps in a fixed order
# (preset, texts, labels and colours, orientation and range, panels, legend layout, theme, marks). Each step lives in its own
# file: chart-options-theme.R, chart-options-scales.R, chart-options-facet.R, chart-options-marks.R.

#' Apply chart options to a ggplot
#'
#' Changes only what `options` sets (see [cd_chart_options()] for the list). The plot passed in is not modified. A plot that
#' is not a ggplot (base graphics) is returned unchanged.
#'
#' @param p A ggplot.
#' @param options A `cd_chart_options` object (or a list of its fields, or `NULL`).
#'
#' @return The ggplot.
#' @export
apply_chart_options <- function(p, options = NULL) {
  o <- as_chart_options(options)
  if (!inherits(p, "ggplot") || length(o) == 0) return(p)
  p <- .clone_plot(p)

  legend_aes <- c("fill", "colour", "color", "shape", "linetype", "size", "alpha")

  # the built plot tells which scales are discrete/continuous; needed by several steps, built at most once
  built <- NULL
  get_built <- function() {
    if (is.null(built)) built <<- tryCatch(ggplot2::ggplot_build(p), error = function(e) FALSE)
    if (isFALSE(built)) NULL else built
  }

  if (!is.null(o$theme_preset)) p <- p + .theme_preset(o$theme_preset)

  # ---- texts
  texts <- list()
  for (field in c("title", "subtitle", "caption", "tag")) texts[[field]] <- o[[field]]
  texts$x <- o$x_title
  texts$y <- o$y_title
  if (!is.null(o$legend_title)) {
    present <- intersect(legend_aes, names(ggplot2::get_labs(p)))
    for (key in if (length(present)) present else c("fill", "colour")) texts[[key]] <- o$legend_title
    # a scale given its own name (name = NULL hides the legend title) beats labs(), so name those too
    for (i in seq_along(p$scales$scales)) {
      if (any(p$scales$scales[[i]]$aesthetics %in% legend_aes)) p$scales$scales[[i]]$name <- o$legend_title
    }
    # and the plot theme may have blanked the legend title ("" hides it: see .apply_visibility())
    if (nzchar(o$legend_title)) p <- p + ggplot2::theme(legend.title = ggplot2::element_text())
  }
  if (!is.null(o$title_wrap)) {
    current <- ggplot2::get_labs(p)
    for (field in c("title", "subtitle", "caption")) {
      texts[[field]] <- .wrap_text(texts[[field]] %||% current[[field]], o$title_wrap)
    }
  }
  texts <- Filter(Negate(is.null), texts)
  if (length(texts)) p <- p + do.call(ggplot2::labs, texts)

  # ---- legend entries, category labels and colours
  if (!is.null(o$legend_labels) || !is.null(o$legend_label_wrap)) {
    p <- .relabel_scales(p, unlist(o$legend_labels), legend_aes, o$legend_label_wrap)
  }
  if (!is.null(o$category_labels) || !is.null(o$category_label_wrap)) {
    p <- .relabel_discrete_axes(p, unlist(o$category_labels), o$category_label_wrap, get_built())
  }
  if (!is.null(o$colors)) p <- .recolor_scales(p, unlist(o$colors), get_built())
  for (axis in c("x", "y")) {
    format <- o[[paste0(axis, "_labels")]]
    if (!is.null(format)) p <- .format_axis(p, axis, format, get_built())
  }

  # ---- orientation and range
  if (!is.null(o$flip)) {
    already <- inherits(p$coordinates, "CoordFlip")
    if (isTRUE(o$flip) && !already) p <- p + ggplot2::coord_flip()
    if (isFALSE(o$flip) && already) p <- p + ggplot2::coord_cartesian()
  }
  flipped <- inherits(p$coordinates, "CoordFlip")
  if (!is.null(o$x_limits) || !is.null(o$y_limits)) p <- .limit_axes(p, o$x_limits, o$y_limits)

  # ---- panels (facet_wrap / facet_grid): layout, shared axes, strip placement
  if (!is.null(o$facet_ncol) || !is.null(o$facet_nrow) || !is.null(o$facet_scales) || !is.null(o$strip_position)) {
    p <- .apply_facet_options(p, o, get_built)
  }

  # ---- legend layout
  if (!is.null(o$legend_ncol) || !is.null(o$legend_nrow) || !is.null(o$legend_reverse)) {
    p <- .legend_layout(p, o, legend_aes, get_built())
  }

  # ---- theme: fonts, sizes, colours, angles, legend, grid, panel, background
  p <- .apply_theme_options(p, o, flipped)

  # ---- what is shown and what is hidden (after the theme, so a hidden element stays hidden)
  p <- .apply_visibility(p, o, flipped)

  # ---- marks
  .apply_layer_options(p, o)
}

# Plot objects share their scales and layers with the plot they were copied from (they are ggproto objects, i.e.
# environments), so editing one in place would change the original. Give the copy its own.
.clone_plot <- function(p) {
  p$scales <- p$scales$clone()
  p$layers <- lapply(p$layers, function(layer) ggplot2::ggproto(NULL, layer))
  p
}

.theme_preset <- function(name) {
  switch(
    name,
    grey = ggplot2::theme_grey(), minimal = ggplot2::theme_minimal(), classic = ggplot2::theme_classic(),
    bw = ggplot2::theme_bw(), light = ggplot2::theme_light(), linedraw = ggplot2::theme_linedraw(),
    dark = ggplot2::theme_dark(), void = ggplot2::theme_void()
  )
}

.wrap_text <- function(x, width) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) return(x)
  paste(vapply(strsplit(x, "\n", fixed = TRUE)[[1]], function(line) paste(strwrap(line, width), collapse = "\n"), character(1)),
        collapse = "\n")
}
