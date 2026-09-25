test_that("the slide layouts are PowerPoint's, their boxes inside the slide", {
  layouts <- report_deck_layouts()
  expect_length(layouts, 12)
  expect_identical(vapply(layouts, function(l) l$id, ""),
                   c("title", "title_content", "two_content", "three_content", "comparison", "title_only", "section", "blank",
                     "picture_caption", "content_caption", "picture_left", "full_picture"))
  for (l in layouts) {
    expect_true(all(c("en", "fr", "pt") %in% names(l$name)), info = l$id)
    for (it in l$items) {
      expect_true(it$role %in% c("title", "subtitle", "heading", "body", "picture"), info = l$id)
      expect_true(it$type %in% c("text", "content"), info = l$id)
      expect_true(it$x >= 0 && it$y >= 0 && it$w > 0 && it$h > 0, info = l$id)
      expect_true(it$x + it$w <= 1 + 1e-9 && it$y + it$h <= 1 + 1e-9, info = l$id)
    }
  }
  expect_length(layouts[[8]]$items, 0)
  expect_identical(report_deck_layouts("fr")[[1]]$name, "Diapositive de titre")
  # the picture layouts, as the editor has them
  box <- function(it) c(it$x, it$y, it$w, it$h)
  by_id <- stats::setNames(layouts, vapply(layouts, function(l) l$id, ""))
  pc <- by_id$picture_caption$items
  expect_identical(vapply(pc, function(i) i$role, ""), c("title", "picture", "body"))
  expect_equal(box(pc[[2]]), c(0.4688, 0.1111, 0.4625, 0.7778))
  expect_identical(pc[[2]]$type, "content")
  expect_identical(vapply(by_id$content_caption$items, function(i) i$role, ""), c("title", "body", "body"))
  expect_equal(box(by_id$picture_left$items[[1]]), c(0, 0, 0.5, 1))
  expect_equal(box(by_id$full_picture$items[[1]]), c(0, 0, 1, 1))
  expect_equal(box(by_id$full_picture$items[[2]]), c(0.05, 0.72, 0.9, 0.18))
  expect_identical(by_id$full_picture$name$en, "Full Picture with Title")
})

test_that("slides are 16:9 unless 4:3, and a block on a slide is drawn at its box", {
  expect_equal(report_slide_size(NULL), c(13.333, 7.5))
  expect_equal(report_slide_size(report_default_design()), c(13.333, 7.5))
  expect_equal(report_slide_size(list(slide_size = "4:3")), c(10, 7.5))
  expect_identical(report_default_design()$slide_size, "16:9")
  expect_identical(.rb_design(list(slide_size = "4:3"))$slide_size, "4:3")
  expect_equal(report_block_size(list(type = "chart", kind = "coverage", box = c(3.2, 5.1))), c(3.2, 5.1))
  expect_equal(report_block_size(list(type = "image", ratio = 2, box = list(4, 1))), c(4, 1))
  # without a box, as before
  expect_equal(report_block_size(list(type = "chart", size = "full", kind = "coverage"))[1], 6.5, tolerance = 0.01)
})

test_that("report_project_blocks gives a document's blocks and a deck's items' blocks with their boxes", {
  doc <- list(blocks = list(list(id = "b1", type = "heading", text = "Hi")))
  expect_identical(report_project_blocks(doc), doc$blocks)
  deck <- list(kind = "deck", slides = list(
    list(id = "s1", items = list(list(id = "i1", x = 1, y = 1, w = 4, h = 3, block = list(id = "x", type = "chart", kind = "coverage")))),
    list(id = "s2", items = list(list(id = "i2", x = 0, y = 0, w = 2, h = 1, role = "title", block = list(type = "paragraph", text = "<p>T</p>"))))
  ))
  blocks <- report_project_blocks(deck)
  expect_length(blocks, 2)
  expect_identical(blocks[[1]]$id, "i1")
  expect_equal(blocks[[1]]$box, c(4, 3))
  expect_equal(report_block_size(blocks[[1]]), c(4, 3))
  expect_identical(blocks[[2]]$type, "paragraph")
})

test_that("slide text keeps paragraphs' alignment, headings and empty lines", {
  lines <- .rb_lines('<p style="text-align: center">Hi <strong>b</strong></p><p></p><h2>Sub</h2><ul><li><p>one</p></li></ul>', keep_empty = TRUE)
  expect_length(lines, 4)
  expect_identical(attr(lines[[1]], "align"), "center")
  expect_length(lines[[2]], 0)
  expect_identical(attr(lines[[3]], "heading"), 2L)
  expect_identical(attr(lines[[4]], "list"), "bullet")
  # documents drop empty paragraphs, as before
  expect_length(.rb_lines("<p>a</p><p></p><p>b</p>"), 2)
  expect_match(.rb_deck_bullet("number", 1, 3), 'startAt="3"', fixed = TRUE)
  expect_match(.rb_deck_bullet("bullet", 2, 1), "buChar", fixed = TRUE)
})

test_that("a deck asks for PowerPoint, a document for Word", {
  deck <- list(kind = "deck", slides = list())
  expect_error(export_report(NULL, deck, tempfile(fileext = ".docx"), "docx"), "PowerPoint")
  expect_error(export_report(NULL, list(blocks = list()), tempfile(fileext = ".pptx"), "pptx"), "slide decks")
})
