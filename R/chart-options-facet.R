# Panels part of the chart options: how a chart split with facet_wrap() / facet_grid() lays out its panels, whether they
# share their axes, and where their labels (strips) sit. The facet is changed through its `params` (the same list
# facet_wrap() / facet_grid() build), so every setting not overridden -- labeller, drop, dir, as.table, space, margins,
# axes -- is kept as the plot method made it. ggplot2 3.4 to 4.x store these params under the same names.

.apply_facet_options <- function(p, o, get_built = function() NULL) {
  facet <- p$facet
  is_wrap <- inherits(facet, "FacetWrap")
  is_grid <- inherits(facet, "FacetGrid")
  if (!is_wrap && !is_grid) return(p)
  params <- facet$params
  if (!is.list(params)) return(p)

  if (!is.null(o$facet_scales) && .coord_allows_free_scales(p)) {
    params$free <- list(x = o$facet_scales %in% c("free", "free_x"), y = o$facet_scales %in% c("free", "free_y"))
  }

  if (is_wrap) {
    ncol <- o$facet_ncol
    nrow <- o$facet_nrow
    if (!is.null(ncol) && !is.null(nrow)) {
      # a grid too small for the panels is an error in ggplot2: keep the columns and let it work out the rows
      n <- .facet_panel_count(get_built())
      if (!is.na(n) && ncol * nrow < n) nrow <- NULL
      params$ncol <- as.integer(ncol)
      params["nrow"] <- list(if (!is.null(nrow)) as.integer(nrow))
    } else if (!is.null(ncol)) {
      # the other dimension follows from the one set (a leftover nrow could make the grid too small)
      params$ncol <- as.integer(ncol)
      params["nrow"] <- list(NULL)
    } else if (!is.null(nrow)) {
      params$nrow <- as.integer(nrow)
      params["ncol"] <- list(NULL)
    }
    if (!is.null(o$strip_position)) params$strip.position <- o$strip_position
  }

  if (is_grid && !is.null(o$strip_position)) {
    params["switch"] <- list(.grid_switch(params$switch, o$strip_position))
  }

  p$facet <- ggplot2::ggproto(NULL, facet, params = params)
  p
}

# Maps and fixed-ratio charts (coord_sf, coord_fixed, coord_polar...) cannot have free scales: ggplot2 refuses to draw them
.coord_allows_free_scales <- function(p) {
  coord <- p$coordinates
  if (is.null(coord) || !is.function(coord$is_free)) return(TRUE)
  isTRUE(tryCatch(coord$is_free(), error = function(e) TRUE))
}

# facet_grid()'s `switch`: "x" moves the column strips to the bottom, "y" the row strips to the left, "both" does both.
# top/bottom set the column strips, left/right the row strips; the other side keeps what it had.
.grid_switch <- function(current, position) {
  current <- if (is.character(current) && length(current) == 1) current else "none"
  x <- current %in% c("x", "both")
  y <- current %in% c("y", "both")
  switch(position, top = x <- FALSE, bottom = x <- TRUE, left = y <- TRUE, right = y <- FALSE)
  if (x && y) "both" else if (x) "x" else if (y) "y" else NULL
}

.facet_panel_count <- function(built) {
  layout <- tryCatch(built$layout$layout, error = function(e) NULL)
  if (is.data.frame(layout)) nrow(layout) else NA_integer_
}

#' Whether a chart is split into panels, and how
#'
#' Tells whether a ggplot is split into panels (`facet_wrap()` or `facet_grid()`) and how they are laid out now, so an
#' editor can offer the panel options of [cd_chart_options()] (`facet_ncol`, `facet_nrow`, `facet_scales`,
#' `strip_position`) only for charts that have panels.
#'
#' @param plot A ggplot (anything else gives `NULL`).
#'
#' @return `NULL` when the chart has no panels. Otherwise a list with
#'   * `type`: `"wrap"` or `"grid"`;
#'   * `ncol`, `nrow`: the number of columns and rows of panels as drawn (integer; `NA` when the plot cannot be built and
#'     the facet does not say);
#'   * `scales`: `"fixed"`, `"free"`, `"free_x"` or `"free_y"`;
#'   * `strip_position`: `"top"`, `"bottom"`, `"left"` or `"right"` (for `facet_grid()`, where the column labels are, or
#'     the row labels when it only has rows);
#'   * `panels`: the number of panels (integer, `NA` when the plot cannot be built).
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~cyl, ncol = 2)
#' chart_facet_info(p)
#' chart_facet_info(ggplot(mtcars, aes(wt, mpg)) + geom_point())
#'
#' @export
chart_facet_info <- function(plot) {
  if (!inherits(plot, "ggplot")) return(NULL)
  facet <- plot$facet
  is_wrap <- inherits(facet, "FacetWrap")
  is_grid <- inherits(facet, "FacetGrid")
  if (!is_wrap && !is_grid) return(NULL)
  params <- facet$params %||% list()

  built <- tryCatch(ggplot2::ggplot_build(plot), error = function(e) NULL)
  layout <- tryCatch(built$layout$layout, error = function(e) NULL)
  drawn <- function(column, param) {
    if (is.data.frame(layout) && column %in% names(layout) && nrow(layout)) return(as.integer(max(layout[[column]])))
    value <- params[[param]]
    if (is.numeric(value) && length(value) == 1 && !is.na(value)) as.integer(value) else NA_integer_
  }

  free <- params$free %||% list()
  free_x <- isTRUE(free$x)
  free_y <- isTRUE(free$y)
  scales <- if (free_x && free_y) "free" else if (free_x) "free_x" else if (free_y) "free_y" else "fixed"

  strip <- if (is_wrap) {
    position <- params$strip.position
    if (is.character(position) && length(position) == 1) position else "top"
  } else {
    switched <- params$switch
    switched <- if (is.character(switched) && length(switched) == 1) switched else "none"
    if (length(params$cols) || !length(params$rows)) {
      if (switched %in% c("x", "both")) "bottom" else "top"
    } else {
      if (switched %in% c("y", "both")) "left" else "right"
    }
  }

  list(
    type = if (is_wrap) "wrap" else "grid",
    ncol = drawn("COL", "ncol"),
    nrow = drawn("ROW", "nrow"),
    scales = scales,
    strip_position = strip,
    panels = .facet_panel_count(built)
  )
}
