# Reports written from R with Quire's writers (export_report(), export_deck(): quire::quire_export())

quire_ctx <- function() {
  d <- data.frame(region = c("North", "Coast", "Lake"), value = c(76, 84, 91))
  report_context(
    draw = function(block, i18n) {
      if (identical(block$kind, "tbl")) return(flextable::flextable(d))
      if (identical(block$kind, "broken")) stop("No data for this year")
      ggplot2::ggplot(d, ggplot2::aes(region, value)) + ggplot2::geom_col()
    },
    fields = function(project, lang) list(country = "Examplia")
  )
}

quire_part <- function(file, name) {
  con <- unz(file, name)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE), collapse = "")
}

test_that("a report is written as Word by Quire: fields filled, charts as pictures, tables as tables", {
  doc <- list(id = "r1", name = "Review", lang = "en", design = list(),
              blocks = list(list(id = "h", type = "heading", level = 1, text = "Summary of {country}"),
                            list(id = "c", type = "chart", kind = "bars", size = "full"),
                            list(id = "t", type = "table", kind = "tbl"),
                            list(id = "b", type = "chart", kind = "broken")))
  f <- tempfile(fileext = ".docx")
  steps <- numeric()
  out <- export_report(quire_ctx(), doc, f, converter = "libreoffice", progress = function(x) steps <<- c(steps, x))
  expect_equal(as.character(out), f)
  expect_equal(range(steps), c(0, 1))
  body <- quire_part(f, "word/document.xml")
  expect_match(body, "Summary of Examplia")
  expect_match(body, "Coast")
  expect_match(body, "No data for this year")
  expect_true(any(grepl("^word/media/", utils::unzip(f, list = TRUE)$Name)))
})

test_that("a deck is written as PowerPoint by Quire, each item at its box", {
  deck <- list(id = "d1", name = "Deck", kind = "deck", lang = "en", design = list(), blocks = list(),
               slides = list(list(id = "s1", notes = "Say this", items = list(
                 list(id = "t1", x = 0.6, y = 0.4, w = 12, h = 0.9, role = "title", block = list(id = "t1", type = "heading", text = "{country}")),
                 list(id = "c1", x = 0.6, y = 1.5, w = 7.5, h = 5, block = list(id = "c1", type = "chart", kind = "bars"))))))
  f <- tempfile(fileext = ".pptx")
  export_deck(quire_ctx(), deck, f)
  names <- utils::unzip(f, list = TRUE)$Name
  expect_true("ppt/slides/slide1.xml" %in% names)
  expect_match(quire_part(f, "ppt/slides/slide1.xml"), "Examplia")
  expect_true(any(grepl("^ppt/media/", names)))
})

test_that("a chart comes to Quire as an SVG with svglite, which Word gets with a PNG beside it", {
  skip_if_not_installed("svglite")
  r <- .rb_render_request(quire_ctx(), list(block = list(id = "c", type = "chart", kind = "bars", size = "full")), NULL, character())
  expect_equal(r$kind, "image")
  expect_null(r$src)
  expect_match(r$svg, "<svg", fixed = TRUE)
  expect_match(r$svg, "Coast", fixed = TRUE)

  doc <- list(id = "r1", name = "Review", lang = "en", design = list(), blocks = list(list(id = "c", type = "chart", kind = "bars", size = "full")))
  f <- tempfile(fileext = ".docx")
  export_report(quire_ctx(), doc, f, converter = "libreoffice")
  media <- grep("^word/media/", utils::unzip(f, list = TRUE)$Name, value = TRUE)
  expect_true(any(grepl("[.]svg$", media)))
  expect_true(any(grepl("[.]png$", media)))
})
