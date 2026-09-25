library(ggplot2)

facet_data <- function() {
  data.frame(x = rep(1:3, 6), y = seq_len(18), g = rep(letters[1:6], each = 3), h = rep(c("u", "v"), 9))
}

wrap_plot <- function(...) {
  ggplot(facet_data(), aes(x, y)) + geom_point() + facet_wrap(~g, ncol = 3, labeller = label_both, ...)
}

# draw to a null device so no Rplots.pdf is left behind
draw_plot <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  ggplot2::ggplotGrob(p)
}

grid_plot <- function(...) {
  ggplot(facet_data(), aes(x, y)) + geom_point() + facet_grid(. ~ h, space = "free_x", ...)
}

test_that("the panel options are validated", {
  o <- cd_chart_options(facet_ncol = 2, facet_nrow = 4, facet_scales = "free_y", strip_position = "bottom")
  expect_identical(o$facet_scales, "free_y")
  expect_error(cd_chart_options(facet_ncol = 0), class = "datasuite_error")
  expect_error(cd_chart_options(facet_nrow = 21), class = "datasuite_error")
  expect_error(cd_chart_options(facet_ncol = 2.5), class = "datasuite_error")
  expect_error(cd_chart_options(facet_scales = "wobbly"), class = "datasuite_error")
  expect_error(cd_chart_options(strip_position = "middle"), class = "datasuite_error")
  expect_true(all(c("facet_ncol", "facet_nrow", "facet_scales", "strip_position") %in% chart_option_fields()))
})

test_that("facet_wrap gets the new layout, scales and strips and keeps everything else", {
  p <- wrap_plot(dir = "v")
  before <- p$facet$params
  out <- apply_chart_options(p, cd_chart_options(facet_ncol = 2, facet_scales = "free_y", strip_position = "bottom"))

  expect_s3_class(out$facet, "FacetWrap")
  params <- out$facet$params
  expect_equal(params$ncol, 2)
  expect_null(params$nrow)
  expect_false(params$free$x)
  expect_true(params$free$y)
  expect_identical(params$strip.position, "bottom")
  expect_identical(params$labeller, before$labeller)
  expect_identical(params$dir, before$dir)
  expect_identical(params$drop, before$drop)
  expect_identical(names(params), names(before))
  # the original is not touched
  expect_equal(p$facet$params$ncol, 3)
  expect_identical(p$facet$params$strip.position, "top")

  built <- ggplot2::ggplot_build(out)
  expect_equal(max(built$layout$layout$COL), 2)
  expect_equal(max(built$layout$layout$ROW), 3)
  expect_no_error(draw_plot(out))
})

test_that("facet_nrow alone lets ggplot2 work out the columns, and a too-small grid keeps the columns", {
  out <- apply_chart_options(wrap_plot(), cd_chart_options(facet_nrow = 1))
  expect_null(out$facet$params$ncol)
  expect_equal(max(ggplot2::ggplot_build(out)$layout$layout$COL), 6)

  out <- apply_chart_options(wrap_plot(), cd_chart_options(facet_ncol = 2, facet_nrow = 2))
  expect_equal(out$facet$params$ncol, 2)
  expect_null(out$facet$params$nrow)
  expect_no_error(ggplot2::ggplot_build(out))

  out <- apply_chart_options(wrap_plot(), cd_chart_options(facet_ncol = 2, facet_nrow = 3))
  expect_equal(out$facet$params$nrow, 3)
})

test_that("facet_grid gets the scales and strip side and keeps its space", {
  p <- grid_plot()
  out <- apply_chart_options(p, cd_chart_options(facet_scales = "free_x", strip_position = "bottom", facet_ncol = 5))
  expect_s3_class(out$facet, "FacetGrid")
  expect_true(out$facet$params$free$x)
  expect_false(out$facet$params$free$y)
  expect_identical(out$facet$params$space_free, p$facet$params$space_free)
  expect_identical(out$facet$params$switch, "x")
  expect_no_error(draw_plot(out))

  # row strips to the left keep the column strips at the bottom
  expect_identical(apply_chart_options(out, cd_chart_options(strip_position = "left"))$facet$params$switch, "both")
  expect_null(apply_chart_options(grid_plot(switch = "x"), cd_chart_options(strip_position = "top"))$facet$params$switch)
})

test_that("free scales are not forced on charts whose coordinates cannot have them", {
  p <- wrap_plot() + coord_fixed()
  out <- apply_chart_options(p, cd_chart_options(facet_scales = "free"))
  expect_false(out$facet$params$free$x)
  expect_no_error(ggplot2::ggplot_build(out))
})

test_that("a chart without panels is unchanged", {
  p <- ggplot(facet_data(), aes(x, y)) + geom_point()
  out <- apply_chart_options(p, cd_chart_options(facet_ncol = 2, facet_scales = "free", strip_position = "left"))
  expect_s3_class(out$facet, "FacetNull")
  expect_identical(out$facet, p$facet)
})

test_that("chart_facet_info describes the panels", {
  expect_null(chart_facet_info(ggplot(facet_data(), aes(x, y)) + geom_point()))
  expect_null(chart_facet_info("not a plot"))

  info <- chart_facet_info(wrap_plot())
  expect_identical(info, list(type = "wrap", ncol = 3L, nrow = 2L, scales = "fixed", strip_position = "top", panels = 6L))

  info <- chart_facet_info(apply_chart_options(wrap_plot(), cd_chart_options(facet_ncol = 2, facet_scales = "free_y",
                                                                              strip_position = "bottom")))
  expect_identical(info[c("ncol", "nrow", "scales", "strip_position", "panels")],
                   list(ncol = 2L, nrow = 3L, scales = "free_y", strip_position = "bottom", panels = 6L))

  info <- chart_facet_info(grid_plot(scales = "free_x", switch = "x"))
  expect_identical(info, list(type = "grid", ncol = 2L, nrow = 1L, scales = "free_x", strip_position = "bottom", panels = 2L))

  info <- chart_facet_info(ggplot(facet_data(), aes(x, y)) + geom_point() + facet_grid(h ~ .))
  expect_identical(info$strip_position, "right")
})
