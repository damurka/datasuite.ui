# A ggplot built once. Building a ggplot (ggplot2::ggplot_build(): computing its stats, scales and panels) is most of
# the time it takes to draw, and one chart is read several times before it is drawn: its layout (cd_chart_layout()),
# its legend entries (cd_chart_entries()), the scales the chart options change (apply_chart_options()), its panels
# (chart_facet_info()), a report's palette and slide text, then the drawing itself. .ds_built() keeps the last few
# builds, each with the plot it was built from, and gives the kept one for the same plot.
#
# "The same plot" is the same object (identical(), which is immediate for the very same object): changing a ggplot
# (`+`, `$<-`) makes a new object, which is built anew. A plot's scales and layers are ggproto objects (environments),
# shared by its copies, so a plot edited in place would not be seen as changed: the package edits copies only
# (.clone_plot(), chart-options-apply.R).

.ds_build_memo <- new.env(parent = emptyenv())
.ds_build_memo$kept <- list()

# How many builds are kept: enough for one chart drawn after another, and the report blocks being drawn
.DS_BUILDS_KEPT <- 6L

# The built plot: ggplot2::ggplot_build(p), once for the same plot. An error building it is not kept.
.ds_built <- function(p) {
  kept <- .ds_build_memo$kept
  for (i in seq_along(kept)) {
    if (identical(kept[[i]]$plot, p)) {
      if (i > 1L) .ds_build_memo$kept <- c(kept[i], kept[-i])
      return(kept[[i]]$built)
    }
  }
  built <- ggplot2::ggplot_build(p)
  .ds_build_memo$kept <- utils::head(c(list(list(plot = p, built = built)), kept), .DS_BUILDS_KEPT)
  built
}

# Draws a ggplot on the open device from its kept build (what print() draws, without building it again)
.ds_draw_plot <- function(p) {
  if (!inherits(p, "ggplot")) return(print(p))
  gtable <- ggplot2::ggplot_gtable(.ds_built(p))
  grid::grid.newpage()
  grid::grid.draw(gtable)
  invisible(p)
}
