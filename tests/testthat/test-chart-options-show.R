library(ggplot2)

show_plot <- function() {
  df <- data.frame(x = 1:4, y = c(2, 3, 1, 4), g = c("a", "b", "a", "b"), f = c("p", "p", "q", "q"))
  ggplot(df, aes(x, y, colour = g)) +
    geom_point() +
    geom_text(aes(label = y), vjust = -1) +
    facet_wrap(~f) +
    labs(title = "Title", subtitle = "Subtitle", caption = "Caption", x = "X", y = "Y", colour = "Group")
}

# the size in points of the rows / columns of the plot's layout holding a grob called `name`
grob_size <- function(p, name, what = c("height", "width")) {
  what <- match.arg(what)
  g <- ggplotGrob(p)
  rows <- g$layout[grepl(name, g$layout$name), , drop = FALSE]
  if (!nrow(rows)) return(0)
  if (what == "height") {
    sum(vapply(seq_len(nrow(rows)), function(i) grid::convertHeight(g$heights[rows$t[i]], "pt", TRUE), numeric(1)))
  } else {
    sum(vapply(seq_len(nrow(rows)), function(i) grid::convertWidth(g$widths[rows$l[i]], "pt", TRUE), numeric(1)))
  }
}

element <- function(p, name) calc_element(name, theme_get() + p$theme)
hidden <- function(p, name) inherits(element(p, name), "element_blank")

test_that("the show_* options are logical chart options that the report builder keeps", {
  for (field in c("show_title", "show_subtitle", "show_caption", "show_x_title", "show_y_title", "show_x_text", "show_y_text",
                  "show_legend", "show_legend_title", "show_labels", "show_strips")) {
    expect_true(field %in% chart_option_fields())
    expect_identical(do.call(cd_chart_options, setNames(list(FALSE), field))[[field]], FALSE)
  }
  expect_error(cd_chart_options(show_title = "no"), class = "datasuite_error")
})

test_that("hiding the title, subtitle and caption leaves no space for them", {
  p <- show_plot()
  expect_gt(grob_size(p, "^title$"), 0)
  expect_gt(grob_size(p, "^subtitle$"), 0)
  expect_gt(grob_size(p, "^caption$"), 0)
  q <- apply_chart_options(p, cd_chart_options(show_title = FALSE, show_subtitle = FALSE, show_caption = FALSE))
  expect_true(hidden(q, "plot.title"))
  expect_true(hidden(q, "plot.subtitle"))
  expect_true(hidden(q, "plot.caption"))
  expect_equal(grob_size(q, "^title$"), 0)
  expect_equal(grob_size(q, "^subtitle$"), 0)
  expect_equal(grob_size(q, "^caption$"), 0)
  # a hidden title stays hidden when its size is set in the same options
  expect_true(hidden(apply_chart_options(p, cd_chart_options(show_title = FALSE, title_size = 20, text_scale = 1.2)), "plot.title"))
})

test_that("an empty text hides its element without leaving space", {
  q <- apply_chart_options(show_plot(), cd_chart_options(title = "", x_title = "", y_title = "", legend_title = ""))
  expect_equal(grob_size(q, "^title$"), 0)
  expect_equal(grob_size(q, "^xlab-b$"), 0)
  expect_equal(grob_size(q, "^ylab-l$", "width"), 0)
  expect_true(hidden(q, "legend.title"))
})

test_that("axis titles and tick labels hide one axis at a time, following the data when flipped", {
  p <- show_plot()
  q <- apply_chart_options(p, cd_chart_options(show_x_title = FALSE, show_y_text = FALSE))
  expect_equal(grob_size(q, "^xlab-b$"), 0)
  expect_gt(grob_size(q, "^ylab-l$", "width"), 0)
  expect_true(hidden(q, "axis.text.y.left"))
  expect_false(hidden(q, "axis.text.x.bottom"))
  # only the tick marks are left (axis_ticks hides those)
  expect_lt(grob_size(q, "^axis-l-1-1$", "width"), grob_size(p, "^axis-l-1-1$", "width") - 5)
  expect_equal(grob_size(apply_chart_options(p, cd_chart_options(show_y_text = FALSE, axis_ticks = FALSE)), "^axis-l-1-1$", "width"), 0)

  q <- apply_chart_options(p, cd_chart_options(show_y_title = FALSE, show_x_text = FALSE))
  expect_equal(grob_size(q, "^ylab-l$", "width"), 0)
  expect_true(hidden(q, "axis.text.x.bottom"))
  expect_false(hidden(q, "axis.text.y.left"))

  # flipped: the data's x is drawn up the side
  q <- apply_chart_options(p, cd_chart_options(flip = TRUE, show_x_title = FALSE))
  expect_true(hidden(q, "axis.title.y"))
  expect_false(hidden(q, "axis.title.x"))
})

test_that("show_x_text = TRUE brings back tick labels the chart hid, and not the other axis'", {
  p <- show_plot() + theme(axis.text = element_blank())
  q <- apply_chart_options(p, cd_chart_options(show_x_text = TRUE))
  expect_false(hidden(q, "axis.text.x.bottom"))
  expect_true(hidden(q, "axis.text.y.left"))
  expect_gt(grob_size(q, "^axis-b", "height"), 0)
})

test_that("the legend and its title hide and come back", {
  p <- show_plot()
  q <- apply_chart_options(p, cd_chart_options(show_legend = FALSE))
  expect_identical(element(q, "legend.position"), "none")
  expect_equal(grob_size(q, "^guide-box-right$", "width"), 0)

  gone <- p + theme(legend.position = "none")
  back <- apply_chart_options(gone, cd_chart_options(show_legend = TRUE))
  expect_identical(element(back, "legend.position"), "right")
  expect_gt(grob_size(back, "^guide-box-right$", "width"), 0)
  # a position given with it wins
  expect_identical(element(apply_chart_options(gone, cd_chart_options(show_legend = TRUE, legend_position = "bottom")), "legend.position"), "bottom")

  q <- apply_chart_options(p, cd_chart_options(show_legend_title = FALSE))
  expect_true(hidden(q, "legend.title"))
  # hidden even when a legend title text is given at the same time
  expect_true(hidden(apply_chart_options(p, cd_chart_options(show_legend_title = FALSE, legend_title = "New")), "legend.title"))
  back <- apply_chart_options(p + theme(legend.title = element_blank()), cd_chart_options(show_legend_title = TRUE))
  expect_false(hidden(back, "legend.title"))
})

test_that("show_labels = FALSE removes the data labels; TRUE and NULL keep them", {
  p <- show_plot() + geom_label(aes(label = g))
  q <- apply_chart_options(p, cd_chart_options(show_labels = FALSE))
  geoms <- unname(vapply(q$layers, function(l) class(l$geom)[[1]], character(1)))
  expect_identical(geoms, "GeomPoint")
  expect_length(ggplot_build(q)$data, 1)
  expect_length(apply_chart_options(p, cd_chart_options(show_labels = TRUE))$layers, 3)
  # the original is untouched
  expect_length(p$layers, 3)
  # a chart made only of labels keeps its layer, invisible
  only <- ggplot(data.frame(x = 1, y = 1), aes(x, y, label = "a")) + geom_text()
  hidden_only <- apply_chart_options(only, cd_chart_options(show_labels = FALSE))
  expect_true(length(hidden_only$layers) %in% c(0, 1))
  if (length(hidden_only$layers)) expect_identical(hidden_only$layers[[1]]$aes_params$alpha, 0)
})

test_that("show_strips hides the panel names and their space, and brings them back", {
  p <- show_plot()
  expect_gt(grob_size(p, "^strip-t"), 0)
  q <- apply_chart_options(p, cd_chart_options(show_strips = FALSE))
  expect_true(hidden(q, "strip.text.x.top"))
  expect_equal(grob_size(q, "^strip-t"), 0)
  back <- apply_chart_options(p + theme(strip.text = element_blank()), cd_chart_options(show_strips = TRUE))
  expect_false(hidden(back, "strip.text.x.top"))
  expect_gt(grob_size(back, "^strip-t"), 0)
})

test_that("NULL show options change nothing", {
  p <- show_plot()
  q <- apply_chart_options(p, cd_chart_options(title_size = 12))
  for (name in c("plot.title", "axis.title.x", "axis.text.y", "legend.title", "strip.text")) expect_false(hidden(q, name))
  expect_length(q$layers, 2)
})
