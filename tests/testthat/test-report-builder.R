test_that("half-width charts and images pair up on a row and are sized from the page", {
  blocks <- list(list(type = "chart", size = "half"), list(type = "image", size = "half"), list(type = "chart", size = "half"),
                 list(type = "heading"))
  expect_equal(.rb_rows(blocks), list(c(1, 2), 3, 4))
  thirds <- rep(list(list(type = "chart", size = "third")), 4)
  expect_equal(.rb_rows(c(thirds, list(list(type = "image", size = "half")))), list(1:3, 4L, 5L))
  expect_equal(report_block_size(list(type = "chart", size = "half", kind = "coverage")), c(3.15, 2.599), tolerance = 0.01)
  expect_equal(report_block_size(list(type = "chart", size = "full", kind = "coverage"))[1], 6.5, tolerance = 0.01)
  wide <- report_block_size(list(type = "chart", size = "full"), utils::modifyList(report_default_design(), list(orientation = "landscape")))
  expect_gt(wide[1], 9)
  expect_equal(report_block_size(list(type = "image", size = "full", width = 50, ratio = 0.5)), c(3.25, 1.625), tolerance = 0.01)
  expect_equal(unname(diff(report_block_size(list(type = "image", size = "half", shape = "circle", ratio = 0.3)))), 0)
})

test_that("formatted text is read into lines of runs", {
  lines <- .rb_lines('A <b>bold <i>both</i></b> and <span style="color: rgb(125, 63, 64);">red</span><br>next')
  expect_length(lines, 2)
  runs <- lines[[1]]
  expect_identical(vapply(runs, function(r) r$text, ""), c("A ", "bold ", "both", " and ", "red"))
  expect_true(runs[[3]]$bold && runs[[3]]$italic)
  expect_identical(runs[[5]]$color, "#7D3F40")
  expect_length(.rb_lines("<div>One</div><div>Two</div>"), 2)
  expect_identical(.rb_plain("plain
text"), "plain
text")
  expect_match(.rb_html_text(list(type = "paragraph", list = "bullet", text = "a<br>b"), list()), "<ul><li>a</li><li>b</li></ul>", fixed = TRUE)
})

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

test_that("the theme becomes the Word file's styles, keeping headings' outline levels", {
  doc <- officer::read_docx(system.file("rmd", "report-template.docx", package = "datasuite.ui"))
  design <- report_themes()$formal
  doc <- .rb_docx_styles(doc, design)
  xml <- xml2::read_xml(file.path(doc$package_dir, "word", "styles.xml"))
  ns <- xml2::xml_ns(xml)
  h1 <- xml2::xml_find_first(xml, "//w:style[@w:styleId='Heading1']", ns)
  expect_identical(xml2::xml_attr(xml2::xml_find_first(h1, "w:rPr/w:rFonts", ns), "ascii"), "Cambria")
  expect_false(inherits(xml2::xml_find_first(h1, "w:pPr/w:outlineLvl", ns), "xml_missing"))
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

test_that("only full-width pictures and charts wrap text", {
  expect_equal(.rb_wrap(list(type = "image", wrap = "left")), "left")
  expect_equal(.rb_wrap(list(type = "image", wrap = "left", size = "half")), "inline")
  expect_equal(.rb_wrap(list(type = "chart", wrap = "right")), "right")
  expect_equal(.rb_wrap(list(type = "chart", wrap = "right", size = "half")), "inline")
  expect_equal(.rb_wrap(list(type = "table", wrap = "right")), "inline")
  expect_equal(.rb_wrap(list(type = "image", wrap = "diagonal")), "inline")
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

test_that("a side handle stretches a picture or chart: only its height changes", {
  pic <- list(type = "image", ratio = 0.5, width = 50)
  plain <- report_block_size(pic)
  tall <- report_block_size(utils::modifyList(pic, list(stretch = 1.5)))
  expect_equal(tall[1], plain[1])
  expect_equal(tall[2], plain[2] * 1.5, tolerance = 0.002)
  # a circle is square before it is stretched (an oval after)
  oval <- report_block_size(list(type = "image", ratio = 0.5, width = 50, shape = "circle", stretch = 0.5))
  expect_equal(oval[2] / oval[1], 0.5, tolerance = 0.01)
  # nonsense is ignored, extremes are limited
  expect_equal(.rb_image_stretch(list(stretch = "x")), 1)
  expect_equal(.rb_image_stretch(list(stretch = -2)), 1)
  expect_equal(.rb_image_stretch(list(stretch = 50)), 10)
  chart <- list(type = "chart", kind = "coverage", size = "full", width = 50)
  s <- .rb_shown_size(chart)
  st <- .rb_shown_size(utils::modifyList(chart, list(stretch = 2)))
  expect_equal(st, c(s[1], s[2] * 2), tolerance = 0.002)
})

test_that("a chart is shown as a picture of its drawing", {
  chart <- list(type = "chart", kind = "coverage", size = "full")
  drawn <- report_block_size(chart)
  # unformatted: shown as drawn
  expect_equal(.rb_shown_size(chart), drawn, tolerance = 0.002)
  expect_false(.rb_chart_pictured(chart))
  # half its width, the same shape
  half <- .rb_shown_size(utils::modifyList(chart, list(width = 50)))
  expect_equal(half, drawn / 2, tolerance = 0.002)
  expect_false(.rb_chart_pictured(utils::modifyList(chart, list(width = 50))))
  # cropped: narrower, the drawing's scale kept only through width; turned: its shape swapped
  cropped <- utils::modifyList(chart, list(crop = list(0, 20, 0, 20)))
  expect_true(.rb_chart_pictured(cropped))
  s <- .rb_shown_size(cropped)
  expect_equal(s[2] / s[1], drawn[2] / drawn[1] / 0.6, tolerance = 0.01)
  turned <- .rb_shown_size(utils::modifyList(chart, list(rotate = 90, width = 40)))
  expect_equal(turned[2] / turned[1], drawn[1] / drawn[2], tolerance = 0.01)
  expect_true(.rb_chart_pictured(utils::modifyList(chart, list(greyscale = TRUE))))
  expect_true(.rb_chart_pictured(utils::modifyList(chart, list(shape = "rounded"))))
  # pictures are not charts
  expect_false(.rb_chart_pictured(list(type = "image", crop = list(10, 0, 0, 0))))
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

test_that("nested lists in the text keep their kind, level and number", {
  lines <- .rb_lines("<ol><li><p>One</p><ul><li><p>a</p></li><li><p>b</p></li></ul></li><li><p>Two</p><p>more</p></li></ol><p>after</p>")
  info <- vapply(lines, function(l) paste(attr(l, "list") %||% "-", attr(l, "level") %||% "-", attr(l, "num") %||% "-"), character(1))
  expect_equal(info, c("number 1 1", "bullet 2 1", "bullet 2 2", "number 1 2", "- 1 -", "- - -"))
  expect_equal(.rb_marker("number", 1, 3), "3.")
  expect_equal(.rb_marker("number", 2, 2), "b.")
  expect_equal(.rb_marker("number", 3, 4), "iv.")
  expect_equal(.rb_marker("bullet", 4, 1), .rb_marker("bullet", 1, 1))
})

test_that("lists, quotes and preformatted text become Word paragraphs", {
  design <- report_default_design()
  list_block <- list(type = "list", text = "<ul><li><p>a</p><ul><li><p>b</p></li></ul></li><li><p>c</p></li></ul>")
  expect_length(.rb_docx_fpars(list_block, design, list()), 3)
  expect_length(.rb_docx_fpars(list(type = "pre", text = "x\ny\nz"), design, list()), 1)
  expect_length(.rb_docx_fpars(list(type = "quote", text = "<p>q</p>"), design, list()), 1)
  expect_length(.rb_docx_fpars(list(type = "note", text = "<p>Notes</p><ul><li><p>one</p></li></ul>"), design, list()), 2)
  # older reports: a note as a list of lines
  expect_length(.rb_docx_fpars(list(type = "note", list = "bullet", text = "one<br>two"), design, list()), 2)
  expect_equal(.rb_heading_size(3, design), round(design$h2_size + design$body_size) / 2)
})

test_that("lists become Word numbering, each list starting at one", {
  project <- list(name = "t", design = report_default_design(), cover = report_default_cover(), blocks = list(
    list(id = "a", type = "list", text = "<ol><li><p>one</p><ul><li><p>sub</p></li></ul></li><li><p>two</p></li></ol>"),
    list(id = "b", type = "paragraph", text = "between, with a <a href=\"https://example.org\">link</a>"),
    list(id = "c", type = "list", text = "<ol><li><p>again one</p></li></ol>"),
    list(id = "h", type = "heading", level = 3, text = "A <em>formatted</em> heading")
  ))
  project$design$cover <- FALSE
  project$design$contents <- FALSE
  parts <- list(project = project, design = .rb_design(project$design), cover = .rb_cover(project$cover), blocks = project$blocks,
                rendered = vector("list", 4), fields = list(), cover_files = list(), page = report_page(.rb_design(project$design)),
                png_copies = character())
  file <- tempfile(fileext = ".docx")
  .rb_write_docx(parts, file, NULL)
  body <- paste(readLines(unz(file, "word/document.xml"), warn = FALSE), collapse = "")
  numbering <- paste(readLines(unz(file, "word/numbering.xml"), warn = FALSE), collapse = "")
  expect_false(grepl("@@RBN", body, fixed = TRUE))
  ids <- regmatches(body, gregexpr('(?<=<w:numId w:val=")[0-9]+', body, perl = TRUE))[[1]]
  expect_length(ids, 4)
  # the two lists are two numberings; the sub item is level 2 of the first
  expect_equal(length(unique(ids)), 2)
  expect_true(grepl('<w:ilvl w:val="1"/>', body, fixed = TRUE))
  expect_true(grepl('w:numFmt w:val="decimal"', numbering, fixed = TRUE))
  expect_true(grepl("w:hyperlink", body, fixed = TRUE))
  expect_true(grepl('w:val="Heading3"', body, fixed = TRUE))
  expect_true(grepl('<w:i( w:val="(true|1|on)")?/>', body))
})

test_that("the space around a wrapped picture is written to Word", {
  skip_if_not_installed("magick")
  b <- list(type = "image", wrap = "left", space_side = 12, space_top = 3, space_bottom = 6)
  png <- tempfile(fileext = ".png")
  magick::image_write(magick::image_blank(100, 50, "white"), png)
  parts <- list(design = .rb_design(report_default_design()), fields = list())
  ft <- .rb_float_table(parts, b, list(file = png), c(2, 1), "left")
  doc <- flextable::body_add_flextable(officer::read_docx(), ft)
  file <- tempfile(fileext = ".docx")
  print(doc, target = file)
  .rb_docx_float_tables(file)
  body <- paste(readLines(unz(file, "word/document.xml"), warn = FALSE), collapse = "")
  expect_true(grepl('w:rightFromText="240"', body, fixed = TRUE))
  expect_true(grepl('w:topFromText="60"', body, fixed = TRUE))
  expect_true(grepl('w:bottomFromText="120"', body, fixed = TRUE))
})

test_that("a chart's show_* options reach the report, FALSE and TRUE alike", {
  kept <- .rb_block_options(list(show_title = FALSE, show_legend = TRUE, title = ""))
  expect_identical(kept$show_title, FALSE)
  expect_identical(kept$show_legend, TRUE)
})
