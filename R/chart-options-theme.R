# Theme part of the chart options: text (font, size, face, colour, angle, alignment), legend box, axes, grid, panel and
# backgrounds. Everything here ends up in one ggplot2::theme() call.

# kind of text -> the theme elements it lives in (an axis' text also lives in axis.text.x / axis.text.y)
.text_elements <- list(
  title = "plot.title", subtitle = "plot.subtitle", caption = "plot.caption",
  axis_title = c("axis.title", "axis.title.x", "axis.title.y"),
  axis_text = c("axis.text", "axis.text.x", "axis.text.y"),
  legend_title = "legend.title", legend_text = "legend.text", strip_text = "strip.text"
)

.apply_theme_options <- function(p, o, flipped) {
  # theme element -> list of its properties, built up by set_text()
  text_props <- list()
  set_text <- function(elements, ...) {
    props <- Filter(Negate(is.null), list(...))
    if (!length(props)) return(invisible())
    for (element in elements) text_props[[element]] <<- utils::modifyList(text_props[[element]] %||% list(), props)
  }
  raw <- list()   # theme arguments that are finished elements or plain values

  all_text <- unique(c(unlist(.text_elements), "plot.tag"))

  # the theme axis an aesthetic ends up on: a flipped chart draws x up the side
  side <- function(aes) if (flipped == (aes == "x")) "y" else "x"

  # text_scale: multiply what the plot has now, then the explicit sizes replace it
  if (!is.null(o$text_scale)) {
    current <- tryCatch(ggplot2::theme_get() + p$theme, error = function(e) NULL)
    for (element in all_text) {
      base <- if (is.null(current)) NULL else tryCatch(ggplot2::calc_element(element, current)$size, error = function(e) NULL)
      if (is.numeric(base) && length(base) == 1) set_text(element, size = base * o$text_scale)
    }
  }

  # font-wide
  if (!is.null(o$font_family)) set_text("text", family = o$font_family)
  if (!is.null(o$line_height)) set_text("text", lineheight = o$line_height)
  if (!is.null(o$text_color)) set_text(all_text, colour = o$text_color)

  # per kind of text: size, face, colour, alignment
  for (key in names(.text_elements)) {
    set_text(.text_elements[[key]],
             size = o[[paste0(key, "_size")]], face = o[[paste0(key, "_face")]], colour = o[[paste0(key, "_color")]])
  }
  for (key in c("title", "subtitle", "caption")) {
    align <- o[[paste0(key, "_align")]]
    if (!is.null(align)) set_text(.text_elements[[key]], hjust = c(left = 0, center = 0.5, right = 1)[[align]])
  }
  if (!is.null(o$title_position)) raw$plot.title.position <- o$title_position
  if (!is.null(o$caption_position)) raw$plot.caption.position <- o$caption_position

  # one axis' size and angle
  for (aes in c("x", "y")) {
    axis <- side(aes)
    set_text(paste0("axis.title.", axis), size = o[[paste0(aes, "_title_size")]])
    set_text(paste0("axis.text.", axis), size = o[[paste0(aes, "_text_size")]])
    angle <- o[[paste0(aes, "_text_angle")]]
    if (!is.null(angle)) do.call(set_text, c(list(paste0("axis.text.", axis)), .axis_text_angle(angle, axis)))
  }

  if (!is.null(o$plot_margin)) {
    m <- if (length(o$plot_margin) == 1) rep(o$plot_margin, 4) else o$plot_margin
    raw$plot.margin <- ggplot2::margin(m[[1]], m[[2]], m[[3]], m[[4]], unit = "pt")
  }

  # legend
  if (!is.null(o$legend_position)) raw$legend.position <- o$legend_position
  if (!is.null(o$legend_direction)) raw$legend.direction <- o$legend_direction
  if (!is.null(o$legend_justification)) raw$legend.justification <- o$legend_justification
  if (!is.null(o$legend_key_size)) raw$legend.key.size <- ggplot2::unit(o$legend_key_size, "mm")
  if (!is.null(o$legend_background)) raw$legend.background <- ggplot2::element_rect(fill = o$legend_background)

  # axes
  if (!is.null(o$axis_line)) {
    raw$axis.line <- if (isTRUE(o$axis_line)) ggplot2::element_line(colour = o$axis_line_color %||% "black") else ggplot2::element_blank()
  } else if (!is.null(o$axis_line_color)) {
    raw$axis.line <- ggplot2::element_line(colour = o$axis_line_color)
  }
  if (!is.null(o$axis_ticks)) raw$axis.ticks <- if (isTRUE(o$axis_ticks)) ggplot2::element_line() else ggplot2::element_blank()

  # grid
  raw <- c(raw, .grid_elements(o))

  # panel and backgrounds
  if (!is.null(o$panel_border)) {
    raw$panel.border <- if (isTRUE(o$panel_border)) {
      ggplot2::element_rect(fill = NA, colour = o$panel_border_color %||% "grey40")
    } else {
      ggplot2::element_blank()
    }
  } else if (!is.null(o$panel_border_color)) {
    raw$panel.border <- ggplot2::element_rect(fill = NA, colour = o$panel_border_color)
  }
  if (!is.null(o$panel_color)) raw$panel.background <- ggplot2::element_rect(fill = o$panel_color)
  if (!is.null(o$background_color)) raw$plot.background <- ggplot2::element_rect(fill = o$background_color)
  if (!is.null(o$strip_background)) raw$strip.background <- ggplot2::element_rect(fill = o$strip_background)

  elements <- lapply(text_props, function(props) do.call(ggplot2::element_text, props))
  args <- c(elements, raw)
  if (length(args)) p <- p + do.call(ggplot2::theme, args)

  # text drawn by geom_text()/geom_label() layers does not follow the theme's family
  if (!is.null(o$font_family)) {
    for (i in seq_along(p$layers)) {
      if (inherits(p$layers[[i]]$geom, c("GeomText", "GeomLabel"))) p$layers[[i]]$aes_params$family <- o$font_family
    }
  }
  p
}

# Angle plus the alignment that keeps rotated labels touching their axis
.axis_text_angle <- function(angle, axis) {
  a <- angle %% 360
  if (axis == "x") {
    just <- if (a == 0) c(0.5, 1) else if (a == 90) c(1, 0.5) else if (a == 270) c(0, 0.5) else if (a < 180) c(1, 1) else c(0, 1)
  } else {
    just <- if (a == 0) c(1, 0.5) else if (a == 90) c(0.5, 0) else if (a == 270) c(0.5, 1) else c(1, 0.5)
  }
  list(angle = angle, hjust = just[[1]], vjust = just[[2]])
}

.grid_elements <- function(o) {
  styled <- !is.null(o$grid_color) || !is.null(o$grid_linewidth) || !is.null(o$grid_linetype)
  if (is.null(o$grid) && is.null(o$grid_minor)) {
    # only restyle whatever grid lines the plot already draws
    if (!styled) return(list())
    return(list(panel.grid = ggplot2::element_line(colour = o$grid_color, linewidth = o$grid_linewidth, linetype = o$grid_linetype)))
  }

  line <- function(scale = 1) {
    ggplot2::element_line(
      colour = o$grid_color %||% "grey85",
      linewidth = if (!is.null(o$grid_linewidth)) o$grid_linewidth * scale,
      linetype = o$grid_linetype
    )
  }
  blank <- ggplot2::element_blank()
  grid <- o$grid %||% "both"
  show_x <- grid %in% c("both", "vertical")     # vertical lines sit on the x breaks
  show_y <- grid %in% c("both", "horizontal")
  out <- list()
  if (!is.null(o$grid)) {
    out$panel.grid.major.x <- if (show_x) line() else blank
    out$panel.grid.major.y <- if (show_y) line() else blank
  }
  minor <- isTRUE(o$grid_minor)
  if (!is.null(o$grid_minor) || identical(o$grid, "none")) {
    out$panel.grid.minor.x <- if (minor && show_x) line(0.5) else blank
    out$panel.grid.minor.y <- if (minor && show_y) line(0.5) else blank
  }
  out
}

# ---- show / hide ---------------------------------------------------------------------------------------------------

# The theme elements a show_* option switches: `parent` is the element several of them share (un-blanked only when the
# chart's theme blanked it), `own` those only this one uses. Axis elements are given for the theme axis ("x" = bottom/top).
.visibility_elements <- function(key, axis = NULL) {
  pos <- if (identical(axis, "x")) c("top", "bottom") else c("left", "right")
  switch(
    key,
    title = list(parent = NULL, own = "plot.title"),
    subtitle = list(parent = NULL, own = "plot.subtitle"),
    caption = list(parent = NULL, own = "plot.caption"),
    axis_title = list(parent = "axis.title", own = c(paste0("axis.title.", axis), paste0("axis.title.", axis, ".", pos))),
    axis_text = list(parent = "axis.text", own = c(paste0("axis.text.", axis), paste0("axis.text.", axis, ".", pos))),
    legend_title = list(parent = NULL, own = "legend.title"),
    strips = list(parent = NULL, own = c("strip.text", "strip.text.x", "strip.text.y", "strip.text.x.top", "strip.text.x.bottom",
                                         "strip.text.y.left", "strip.text.y.right"))
  )
}

# show_* options (and empty texts, which hide their element). Hidden = element_blank, which ggplot2 gives no space;
# shown = an element the chart's theme blanked becomes a text again.
.apply_visibility <- function(p, o, flipped) {
  side <- function(aes) if (flipped == (aes == "x")) "y" else "x"
  empty <- function(field) identical(o[[field]], "")
  # element -> TRUE (show) / FALSE (hide)
  wanted <- list(
    list(el = .visibility_elements("title"), show = if (empty("title")) FALSE else o$show_title),
    list(el = .visibility_elements("subtitle"), show = if (empty("subtitle")) FALSE else o$show_subtitle),
    list(el = .visibility_elements("caption"), show = if (empty("caption")) FALSE else o$show_caption),
    list(el = .visibility_elements("axis_title", side("x")), show = if (empty("x_title")) FALSE else o$show_x_title),
    list(el = .visibility_elements("axis_title", side("y")), show = if (empty("y_title")) FALSE else o$show_y_title),
    list(el = .visibility_elements("axis_text", side("x")), show = o$show_x_text),
    list(el = .visibility_elements("axis_text", side("y")), show = o$show_y_text),
    list(el = .visibility_elements("legend_title"), show = if (empty("legend_title")) FALSE else o$show_legend_title),
    list(el = .visibility_elements("strips"), show = o$show_strips)
  )
  wanted <- Filter(function(w) !is.null(w$show), wanted)
  if (!length(wanted) && is.null(o$show_legend)) return(p)

  current <- tryCatch(ggplot2::theme_get() + p$theme, error = function(e) NULL)
  resolved_blank <- function(name) {
    if (is.null(current)) return(FALSE)
    el <- tryCatch(ggplot2::calc_element(name, current), error = function(e) NULL)
    inherits(el, "element_blank")
  }
  set_blank <- function(name) inherits(p$theme[[name]], "element_blank")

  args <- list()
  for (w in wanted) {
    if (isFALSE(w$show)) {
      for (name in w$el$own) args[[name]] <- ggplot2::element_blank()
      next
    }
    # TRUE: un-blank what the theme blanked, the shared parent included; the other elements under that parent that were
    # hidden stay hidden
    for (name in c(w$el$parent, w$el$own)) if (set_blank(name) || resolved_blank(name)) args[[name]] <- ggplot2::element_text()
    parent <- w$el$parent
    if (!is.null(parent) && !is.null(args[[parent]])) {
      for (axis in c("x", "y")) {
        sibling <- .visibility_elements(sub("^axis\\.(title|text)$", "axis_\\1", parent), axis)$own
        if (any(sibling %in% w$el$own)) next
        if (resolved_blank(sibling[[1]])) for (name in sibling) args[[name]] <- args[[name]] %||% ggplot2::element_blank()
      }
    }
  }

  if (isFALSE(o$show_legend)) {
    args$legend.position <- "none"
  } else if (isTRUE(o$show_legend) && is.null(o$legend_position)) {
    position <- if (is.null(current)) NULL else tryCatch(ggplot2::calc_element("legend.position", current), error = function(e) NULL)
    if (identical(position, "none")) args$legend.position <- "right"
  }

  if (length(args)) p <- p + do.call(ggplot2::theme, args)
  p
}
