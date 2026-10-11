# What the chart customize panel (js/src/components/ChartCustomize.tsx) edits, as data: its elements, and each chart
# option's element, group, control, choices and labels. They come from Quire's contract (quire::quire_chart_options(),
# chart-options.json), the same as the report builder's, so a chart option is made editable there once, for both.

cd_chart_schema <- function(i18n = NULL) {
  o <- quire::quire_chart_options()
  list(tabs = o$tabs, fields = o$fields)
}

# the chart options the panel edits ("entries" is three of them), and the elements' Show switches
.chart_panel_fields <- function() {
  o <- quire::quire_chart_options()
  keys <- vapply(o$fields, function(f) f$key, character(1))
  shows <- unlist(lapply(o$tabs, function(t) t$show), use.names = FALSE)
  c(setdiff(keys, "entries"), "legend_labels", "colors", "category_labels", shows)
}

# Legend entries and the categories of a discrete axis, to relabel or recolour. Capped so a very large legend cannot make
# the panel unusable.
cd_chart_entries <- function(p, max_entries = 30) {
  if (!inherits(p, "ggplot")) return(list())
  built <- tryCatch(.ds_built(p), error = function(e) NULL)
  if (is.null(built)) return(list())

  legend <- list()
  for (scale in built$plot$scales$scales) {
    if (!.ds_legend_scale(scale)) next
    limits <- scale$get_limits()
    limits <- limits[!is.na(limits)]
    if (!length(limits) || length(limits) > max_entries) next
    shown <- .ds_scale_labels(scale, limits)
    colours <- tryCatch(as.character(scale$map(limits)), error = function(e) rep(NA_character_, length(limits)))
    legend <- lapply(seq_along(limits), function(i) list(
      key = as.character(limits[[i]]), shown = shown[[i]],
      color = tryCatch(grDevices::rgb(t(grDevices::col2rgb(colours[[i]])), maxColorValue = 255), error = function(e) NULL)
    ))
    break
  }

  categories <- list()
  for (axis in c("x", "y")) {
    scale <- built$layout[[paste0("panel_scales_", axis)]][[1]]
    if (is.null(scale) || !isTRUE(scale$is_discrete())) next
    limits <- scale$get_limits()
    limits <- limits[!is.na(limits)]
    if (!length(limits) || length(limits) > max_entries) next
    shown <- .ds_scale_labels(scale, limits)
    categories <- lapply(seq_along(limits), function(i) list(key = as.character(limits[[i]]), shown = shown[[i]]))
    break
  }
  list(legend = legend, categories = categories)
}

# Is this the scale of a colour or fill legend? A discrete colour or fill scale that draws one. A scale without a
# legend (scale_colour_identity(), guide = "none") is not: it colours marks (the numbers on a heat map's tiles), it
# has no entries to rename or recolour, and it gives no labels (listing them failed: "subscript out of bounds").
.ds_legend_scale <- function(scale) {
  any(c("colour", "fill") %in% scale$aesthetics) && isTRUE(scale$is_discrete()) &&
    !identical(scale$guide, "none") && !isFALSE(scale$guide)
}

# What a scale shows for each of `limits`, one for each: the value itself where the scale gives no label.
.ds_scale_labels <- function(scale, limits) {
  shown <- tryCatch(as.character(scale$get_labels(limits)), error = function(e) character())
  if (length(shown) == length(limits)) shown else as.character(limits)
}
