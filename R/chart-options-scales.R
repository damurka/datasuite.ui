# Scale part of the chart options: legend and category labels, colours, number formats, axis range, legend layout.

# Rewrites the labels of a scale: `map` names are matched against the scale's break values first, then against the text
# the scale would show; `wrap` then wraps every label.
.relabel_function <- function(old, map, wrap = NULL) {
  force(old); force(map); force(wrap)
  function(breaks) {
    shown <- if (inherits(old, "waiver")) as.character(breaks) else if (is.function(old)) as.character(old(breaks)) else as.character(old)
    hit_break <- match(as.character(breaks), names(map))
    hit_shown <- match(shown, names(map))
    hit <- ifelse(is.na(hit_break), hit_shown, hit_break)
    shown <- replace(shown, !is.na(hit), unname(map[hit[!is.na(hit)]]))
    if (!is.null(wrap)) shown <- vapply(shown, .wrap_text, character(1), width = wrap, USE.NAMES = FALSE)
    shown
  }
}

.relabel_scales <- function(p, map, aesthetics, wrap = NULL) {
  map <- map %||% character()
  for (i in seq_along(p$scales$scales)) {
    scale <- p$scales$scales[[i]]
    if (any(scale$aesthetics %in% aesthetics) && isTRUE(scale$is_discrete())) {
      scale$labels <- .relabel_function(scale$labels, map, wrap)
    }
  }
  p
}

.relabel_discrete_axes <- function(p, map, wrap = NULL, built = NULL) {
  map <- map %||% character()
  if (is.null(built)) return(p)

  for (axis in c("x", "y")) {
    panel_scale <- built$layout[[paste0("panel_scales_", axis)]][[1]]
    if (is.null(panel_scale) || !isTRUE(panel_scale$is_discrete())) next

    existing <- Filter(function(s) axis %in% s$aesthetics, p$scales$scales)
    if (length(existing)) {
      existing[[1]]$labels <- .relabel_function(existing[[1]]$labels, map, wrap)
    } else {
      labeller <- .relabel_function(ggplot2::waiver(), map, wrap)
      p <- p + if (axis == "x") ggplot2::scale_x_discrete(labels = labeller) else ggplot2::scale_y_discrete(labels = labeller)
    }
  }
  p
}

# Change some of the colours of a colour/fill legend, keeping the ones not named. Every discrete colour or fill scale (the
# plot's own or the default one it would get) is given the colours it produces now with the named ones replaced.
.recolor_scales <- function(p, map, built) {
  if (is.null(built)) return(p)

  for (built_scale in built$plot$scales$scales) {
    aesthetics <- intersect(built_scale$aesthetics, c("colour", "fill"))
    if (!length(aesthetics) || !isTRUE(built_scale$is_discrete())) next

    limits <- built_scale$get_limits()
    limits <- limits[!is.na(limits)]
    if (!length(limits)) next
    colours <- stats::setNames(as.character(built_scale$map(limits)), limits)
    shown <- as.character(built_scale$get_labels(limits))

    hit <- ifelse(is.na(match(limits, names(map))), match(shown, names(map)), match(limits, names(map)))
    colours[!is.na(hit)] <- unname(map[hit[!is.na(hit)]])

    aes <- aesthetics[[1]]
    existing <- Filter(function(s) aes %in% s$aesthetics, p$scales$scales)
    if (length(existing)) {
      existing[[1]]$palette <- local({ fixed <- colours; function(n) fixed })
      existing[[1]]$palette.cache <- NULL
      existing[[1]]$n.breaks.cache <- NULL
    } else {
      p <- p + if (aes == "fill") ggplot2::scale_fill_manual(values = colours) else ggplot2::scale_colour_manual(values = colours)
    }
  }
  p
}

# How the numbers on a continuous axis are written
.format_axis <- function(p, axis, format, built) {
  if (is.null(built)) return(p)
  panel_scale <- built$layout[[paste0("panel_scales_", axis)]][[1]]
  if (is.null(panel_scale) || !inherits(panel_scale, "ScaleContinuous") ||
      inherits(panel_scale, c("ScaleContinuousDate", "ScaleContinuousDatetime", "ScaleBinned"))) {
    return(p)
  }

  labeller <- switch(
    format,
    number = scales::label_number(),
    comma = scales::label_comma(),
    percent = scales::label_percent(),
    percent_points = scales::label_number(suffix = "%"),
    scientific = scales::label_scientific(),
    compact = scales::label_number(scale_cut = scales::cut_short_scale())
  )

  existing <- Filter(function(s) axis %in% s$aesthetics, p$scales$scales)
  if (length(existing)) {
    existing[[1]]$labels <- labeller
  } else {
    p <- p + if (axis == "x") ggplot2::scale_x_continuous(labels = labeller) else ggplot2::scale_y_continuous(labels = labeller)
  }
  p
}

# Zoom the axes (nothing is dropped from the data). Only Cartesian charts (also flipped) can be zoomed this way.
.limit_axes <- function(p, x_limits, y_limits) {
  coord <- p$coordinates
  old <- coord$limits %||% list()
  x <- x_limits %||% old$x
  y <- y_limits %||% old$y
  if (inherits(coord, "CoordFlip")) {
    p + ggplot2::coord_flip(xlim = x, ylim = y, expand = coord$expand %||% TRUE)
  } else if (inherits(coord, "CoordCartesian") && !inherits(coord, "CoordFixed")) {
    p + ggplot2::coord_cartesian(xlim = x, ylim = y, expand = coord$expand %||% TRUE, clip = coord$clip %||% "on")
  } else {
    p
  }
}

# Legend columns/rows/order for every discrete legend
.legend_layout <- function(p, o, legend_aes, built) {
  if (is.null(built)) return(p)
  args <- Filter(Negate(is.null), list(ncol = o$legend_ncol, nrow = o$legend_nrow, reverse = o$legend_reverse))
  done <- character()
  for (scale in built$plot$scales$scales) {
    aes <- intersect(scale$aesthetics, c("fill", "colour", "shape", "linetype", "size", "alpha"))
    if (!length(aes) || !isTRUE(scale$is_discrete()) || identical(scale$guide, "none") || aes[[1]] %in% done) next
    done <- c(done, aes[[1]])
    p <- p + do.call(ggplot2::guides, stats::setNames(list(do.call(ggplot2::guide_legend, args)), aes[[1]]))
  }
  p
}
