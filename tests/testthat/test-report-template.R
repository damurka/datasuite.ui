test_that("a PowerPoint file's theme becomes a report theme", {
  skip_if_not_installed("zip")
  potx <- .rb_test_pptx_template(tempfile("Brand ", fileext = ".potx"))
  th <- report_theme_from_file(potx, name = "Brand")
  # the fields of the ready-made themes, and what the file adds
  expect_setequal(setdiff(names(th), c("template_kind", "background", "slide_designs")), names(report_themes()$formal))
  expect_named(th$slide_designs, c("title", "content"))
  expect_true("background" %in% names(th))
  expect_identical(th$template_kind, "pptx")
  expect_match(th$theme, "^custom_[0-9a-f]+$")
  expect_identical(th$name, list(en = "Brand", fr = "Brand", pt = "Brand"))
  expect_identical(th$accent, "#b5472b")
  expect_identical(th$text_color, "#1b1b1b")
  # the title colour of the master is the text colour: the second dark colour is the heading colour
  expect_identical(th$heading_color, "#243b55")
  expect_identical(th$palette, c("#b5472b", "#2e7d6b", "#e0a526", "#5b4c9a", "#3f88c5", "#7a8691"))
  expect_identical(th$heading_font, "Georgia")
  expect_identical(th$body_font, "Verdana")
  expect_identical(th$slide_size, "16:9")
  expect_identical(th$background, "#f3eee4")
  expect_match(th$note_fill, "^#[0-9a-f]{6}$")
  expect_gt(.rb_luminance(th$note_fill), .rb_luminance(th$note_border))
  expect_gt(.rb_luminance(th$muted_color), .rb_luminance(th$text_color))
  # sizes stay the default design's
  expect_identical(th$h1_size, report_default_design()$h1_size)
  # the design it makes is a complete one
  expect_identical(.rb_design(th)$heading_font, "Georgia")

  pptx <- .rb_test_pptx_template(tempfile(fileext = ".pptx"), size = "4:3", background = NULL)
  th2 <- report_theme_from_file(pptx)
  expect_identical(th2$slide_size, "4:3")
  expect_null(th2$background)
  expect_true("background" %in% names(th2))
  # the file's name (without its extension) is the theme's name
  expect_identical(th2$name$en, tools::file_path_sans_ext(basename(pptx)))
  expect_false(identical(th$theme, th2$theme))
})

test_that("a Word file's theme and styles become a report theme", {
  skip_if_not_installed("zip")
  dotx <- .rb_test_docx_template(tempfile(fileext = ".dotx"))
  th <- report_theme_from_file(dotx)
  expect_identical(th$template_kind, "docx")
  expect_false("background" %in% names(th))
  expect_identical(th$accent, "#2a7f62")
  # Heading 1's own colour and size, Normal's size, the theme's fonts the styles refer to
  expect_identical(th$heading_color, "#1f5c4a")
  expect_identical(th$h1_size, 20)
  expect_identical(th$body_size, 11)
  expect_identical(th$heading_font, "Cambria")
  expect_identical(th$body_font, "Corbel")
  expect_identical(th$text_color, "#202020")
  # the page: officer's blank document is A4 portrait
  expect_identical(th$size, "a4")
  expect_identical(th$orientation, "portrait")
})

test_that("a file that is not a PowerPoint or Word file is refused", {
  f <- tempfile(fileext = ".pptx")
  writeLines("not a zip", f)
  expect_error(report_theme_from_file(f), "PowerPoint or Word")
  expect_error(report_theme_from_file(tempfile()), "PowerPoint or Word")
})
