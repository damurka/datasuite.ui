# One ggplot built once (R/kit-chart-build.R), and drawing and saving charts from that build

builds <- function(expr) {
  n <- 0L
  build <- ggplot2::ggplot_build
  local_mocked_bindings(ggplot_build = function(plot, ...) { n <<- n + 1L; build(plot, ...) }, .package = "ggplot2")
  force(expr)
  n
}

facet_chart <- function() {
  d <- expand.grid(region = c("North", "Coast", "Lake"), year = 2022:2024, indicator = c("ANC4", "Penta3"))
  d$value <- seq_len(nrow(d))
  ggplot2::ggplot(d, ggplot2::aes(region, value, fill = indicator)) + ggplot2::geom_col() + ggplot2::facet_wrap(~year)
}

test_that("a plot is built once for its layout, legend entries, panels and drawing; a changed plot anew", {
  p <- facet_chart()
  n <- builds({
    cd_chart_layout(p)
    cd_chart_entries(p)
    chart_facet_info(p)
    f <- tempfile(fileext = ".png")
    grDevices::png(f)
    .ds_draw_plot(p)
    grDevices::dev.off()
  })
  expect_equal(n, 1L)
  expect_equal(builds(.ds_built(p + ggplot2::labs(title = "Changed"))), 1L)
  # the options' scale steps read the plot given, already built
  expect_equal(builds(apply_chart_options(p, list(colors = list(ANC4 = "#000000")))), 0L)
})

test_that("a built plot is the one ggplot2 builds, and drawing it is what print() draws", {
  p <- facet_chart()
  expect_equal(.ds_built(p)$layout$layout, ggplot2::ggplot_build(p)$layout$layout)
  a <- tempfile(fileext = ".png")
  b <- tempfile(fileext = ".png")
  grDevices::png(a, width = 400, height = 300)
  print(p)
  grDevices::dev.off()
  grDevices::png(b, width = 400, height = 300)
  .ds_draw_plot(p)
  grDevices::dev.off()
  expect_equal(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
})

test_that("a chart's image download is what ggsave() writes, background included", {
  p <- facet_chart() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "lightyellow"))
  a <- tempfile(fileext = ".png")
  b <- tempfile(fileext = ".png")
  ggplot2::ggsave(a, plot = p, width = 800, height = 600, units = "px", dpi = 150)
  .ds_save_plot(b, p, width = 800, height = 600, dpi = 150)
  expect_equal(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
})

test_that("a plot drawn with base graphics is recorded, off screen, and saved as itself", {
  before <- grDevices::dev.list()
  rec <- .ds_capture_plot(function() graphics::barplot(c(a = 1, b = 3)))
  expect_s3_class(rec, "recordedplot")
  expect_identical(grDevices::dev.list(), before)
  f <- tempfile(fileext = ".png")
  .ds_save_plot(f, rec, width = 600, height = 400, dpi = 100)
  expect_gt(file.info(f)$size, 1000)
  # a ggplot (or anything else that draws nothing) is given back as it is
  p <- facet_chart()
  expect_identical(.ds_capture_plot(function() p), p)
  expect_identical(.ds_capture_plot(function() 42), 42)
})

test_that("every font a chart's text is drawn in is found: the theme's, an element's own, a label layer's", {
  p <- facet_chart()
  expect_false(any(.rb_plot_families(p) %in% .rb_bitmap_fonts))
  expect_true("Calibri" %in% .rb_plot_families(p + ggplot2::theme(axis.text.x = ggplot2::element_text(family = "Calibri"))))
  expect_true("Cambria" %in% .rb_plot_families(p + ggplot2::theme(legend.title = ggplot2::element_text(family = "Cambria"))))
  expect_true("Corbel" %in% .rb_plot_families(p + ggplot2::theme_minimal(base_family = "Corbel")))
  expect_true("Calibri" %in% .rb_plot_families(p + ggplot2::geom_text(ggplot2::aes(label = value), family = "Calibri")))
})
