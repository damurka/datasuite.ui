
test_that("fields are filled in and escaped in formatted text", {
  fields <- list(country = "Côte <d'Ivoire>", latest_year = "2025")
  expect_identical(.rb_fill("{country}, {latest_year}, {unknown}", fields), "Côte <d'Ivoire>, 2025, {unknown}")
  expect_match(.rb_fill("<b>{country}</b>", fields, escape = TRUE), "&lt;d'Ivoire&gt;", fixed = TRUE)
  project <- list(name = "R", cover = list(editors = list(list(name = "A"), list(name = "B")), date_mode = "custom", date = "Soon"))
  f <- report_fields(NULL, project)
  expect_identical(f$editors, "A, B")
  expect_identical(f$report_date, "Soon")
  keys <- vapply(report_field_catalog(), function(x) x$key, "")
  expect_false(anyDuplicated(keys) > 0)
})

test_that("themes are complete designs and saved designs are filled in", {
  fields <- names(report_default_design())
  for (th in report_themes()) {
    expect_true(all(setdiff(fields, "name") %in% names(th)), info = th$theme)
    expect_true(all(c(th$heading_font, th$body_font) %in% report_fonts(installed = FALSE)), info = th$theme)
  }
  old <- .rb_design(list(accent = "#123456", font = "sans"))
  expect_identical(old$heading_font, "Calibri")
  expect_identical(old$heading_color, "#123456")
  expect_null(old$font)
  cover <- .rb_cover(list(editors = list(list(name = "A"), list(name = "B"))))
  expect_length(cover$editors, 2)
})

test_that("a block that cannot be drawn reports an error instead of failing", {
  r <- render_report_block(NULL, list(type = "chart", kind = "coverage", indicator = "anc4"))
  expect_identical(r$type, "error")
  expect_true(nzchar(r$message))
})

test_that("a picture's shape follows its turn and crop", {
  b <- list(type = "image", ratio = 0.5)
  expect_equal(.rb_image_ratio(b), 0.5)
  expect_equal(.rb_image_ratio(modifyList(b, list(rotate = 90))), 2)
  expect_equal(.rb_image_ratio(modifyList(b, list(crop = list(0, 50, 0, 0)))), 1)
  expect_equal(.rb_image_ratio(modifyList(b, list(shape = "circle", rotate = 90))), 1)
  # at least a tenth is always left
  expect_equal(.rb_image_crop(list(crop = list(80, 0, 80, 0))), c(0.45, 0, 0.45, 0))
  expect_equal(.rb_image_crop(list(crop = "x")), c(0, 0, 0, 0))
  # any width from 5 percent of the column
  small <- report_block_size(list(type = "image", ratio = 1, width = 5))
  full <- report_block_size(list(type = "image", ratio = 1, width = 100))
  expect_equal(small[1] / full[1], 0.05, tolerance = 0.01)
})

test_that("{chart_indicator} and {chart_year} are the chart below the text (or the chart itself)", {
  blocks <- list(
    list(id = "a", type = "heading", level = 2, text = "Coverage of {chart_indicator}, {chart_year}"),
    list(id = "b", type = "paragraph", text = "No field here"),
    list(id = "c", type = "chart", kind = "coverage", indicator = "penta3", year = 2021, title = "{chart_indicator} trend"),
    list(id = "d", type = "paragraph", text = "Then {chart_indicator}"),
    list(id = "e", type = "table", kind = "coverage_table", indicator = "anc4"),
    list(id = "f", type = "paragraph", text = "Nothing after: {chart_indicator}")
  )
  out <- report_chart_fields(blocks)
  expect_equal(out[[1]]$text, "Coverage of penta3, 2021")
  expect_equal(out[[2]]$text, "No field here")
  expect_equal(out[[3]]$title, "penta3 trend")
  expect_equal(out[[4]]$text, "Then anc4")
  expect_equal(out[[6]]$text, "Nothing after: {chart_indicator}")
  keys <- vapply(report_field_catalog(), function(f) f$key, character(1))
  expect_true(all(c("chart_indicator", "chart_year") %in% keys))
})

test_that("the report fonts are print faces, serif then sans serif", {
  fonts <- report_fonts()
  expect_true(length(fonts) > 0)
  expect_equal(fonts, c(intersect(report_fonts("serif"), fonts), intersect(report_fonts("sans"), fonts)))
  expect_false(any(c("Verdana", "Segoe UI", "Tahoma") %in% fonts))
})

test_that("bar width sets the width of bars", {
  p <- ggplot2::ggplot(data.frame(x = c("a", "b"), y = 1:2), ggplot2::aes(x, y)) + ggplot2::geom_col()
  d <- ggplot2::layer_data(apply_chart_options(p, cd_chart_options(bar_width = 0.4)))
  expect_equal(unique(round(d$xmax - d$xmin, 3)), 0.4)
  expect_error(cd_chart_options(bar_width = 2))
})

test_that("a chart's show_* options reach the report, FALSE and TRUE alike", {
  kept <- .rb_block_options(list(show_title = FALSE, show_legend = TRUE, title = ""))
  expect_identical(kept$show_title, FALSE)
  expect_identical(kept$show_legend, TRUE)
})
