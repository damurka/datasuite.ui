# Marks part of the chart options: line width, point size, transparency and the text drawn by geom_text()/geom_label().

# The geoms that draw data labels (ggplot2's and ggrepel's)
.label_geoms <- c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel")

# show_labels = FALSE: the label layers are removed; a chart that cannot be drawn without them keeps them, invisible
.hide_label_layers <- function(p) {
  is_label <- vapply(p$layers, function(layer) inherits(layer$geom, .label_geoms), logical(1))
  if (!any(is_label)) return(p)
  without <- p
  without$layers <- p$layers[!is_label]
  if (!inherits(tryCatch(ggplot2::ggplot_build(without), error = function(e) e), "error")) return(without)
  for (i in which(is_label)) p$layers[[i]]$aes_params$alpha <- 0
  p
}

.apply_layer_options <- function(p, o) {
  if (isFALSE(o$show_labels)) p <- .hide_label_layers(p)
  if (is.null(o$line_scale) && is.null(o$point_scale) && is.null(o$alpha) && is.null(o$bar_width) &&
      is.null(o$label_size) && is.null(o$label_angle) && is.null(o$label_color)) {
    return(p)
  }

  scaled <- function(layer, aesthetic, factor, fallback) {
    if (!is.null(layer$mapping[[aesthetic]])) return(layer)
    current <- layer$aes_params[[aesthetic]]
    if (is.null(current)) {
      default <- layer$geom$default_aes[[aesthetic]]
      current <- if (is.numeric(default) && length(default) == 1) default else fallback
    }
    layer$aes_params[[aesthetic]] <- current * factor
    layer
  }

  # the lines that draw data; reference lines (hline, vline, abline) are left as the plot drew them
  line_geoms <- c("GeomPath", "GeomSmooth", "GeomSegment", "GeomErrorbar", "GeomLinerange", "GeomCrossbar", "GeomStep")
  for (i in seq_along(p$layers)) {
    layer <- p$layers[[i]]
    geom <- layer$geom
    is_text <- inherits(geom, .label_geoms)
    if (!is.null(o$line_scale) && inherits(geom, line_geoms)) layer <- scaled(layer, "linewidth", o$line_scale, 0.5)
    if (!is.null(o$point_scale) && inherits(geom, c("GeomPoint", "GeomPointrange"))) layer <- scaled(layer, "size", o$point_scale, 1.5)
    if (!is.null(o$alpha) && !is_text) layer$aes_params$alpha <- o$alpha
    # bars and boxes: their width is a parameter of the layer (a width mapped to the data is left alone)
    if (!is.null(o$bar_width) && inherits(geom, c("GeomBar", "GeomCol", "GeomBoxplot", "GeomCrossbar", "GeomErrorbar", "GeomTile")) &&
        is.null(layer$mapping$width)) {
      layer$geom_params$width <- o$bar_width
      if ("width" %in% names(layer$stat_params) || inherits(layer$stat, "StatCount")) layer$stat_params$width <- o$bar_width
    }
    if (is_text) {
      if (!is.null(o$label_size)) layer$aes_params$size <- o$label_size / ggplot2::.pt
      if (!is.null(o$label_angle)) layer$aes_params$angle <- o$label_angle
      if (!is.null(o$label_color)) layer$aes_params$colour <- o$label_color
    }
    p$layers[[i]] <- layer
  }
  p
}
