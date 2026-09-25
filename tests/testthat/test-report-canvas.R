test_that("report_project_blocks gives a free-layout page's items, not the page itself", {
  doc <- list(blocks = list(
    list(id = "b1", type = "paragraph", text = "Hi"),
    list(id = "cv", type = "canvas", items = list(
      list(id = "i1", x = 0, y = 0, w = 4, h = 3, block = list(id = "x", type = "chart", kind = "coverage")),
      list(id = "i2", x = 0, y = 3, w = 2, h = 1, role = "title", block = list(type = "image", src = "asset:abc"))
    )),
    list(id = "b2", type = "pagebreak")
  ))
  blocks <- report_project_blocks(doc)
  expect_length(blocks, 4)
  expect_identical(vapply(blocks, function(b) b$type, ""), c("paragraph", "chart", "image", "pagebreak"))
  expect_identical(blocks[[2]]$id, "i1")
  expect_equal(blocks[[2]]$box, c(4, 3))
  expect_equal(report_block_size(blocks[[3]]), c(2, 1))
  expect_identical(blocks[[3]]$src, "asset:abc")
  # a document without a canvas is unchanged
  plain <- list(blocks = doc$blocks[c(1, 3)])
  expect_identical(report_project_blocks(plain), plain$blocks)
})

test_that("a canvas is as tall as the page's text unless given a height", {
  page <- report_page(report_default_design())
  expect_equal(.rb_canvas_height(list(type = "canvas"), page), page$text_height)
  expect_equal(.rb_canvas_height(list(type = "canvas", h = 5), page), 5)
})
