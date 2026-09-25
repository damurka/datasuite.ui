# A document with a free-layout page between two paragraphs: a title, a coverage chart, a bulleted text, a table and a
# picture with a text box in front of it (used by test-report-canvas.R)
.rb_test_canvas_project <- function() {
  img <- tempfile(fileext = ".png")
  pic <- magick::image_blank(600, 400, "#2f6f9f")
  pic <- magick::image_annotate(pic, "PICTURE", size = 70, color = "white", gravity = "center")
  magick::image_write(pic, img, format = "png")
  src <- paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(img, "raw", file.info(img)$size)))
  item <- function(id, x, y, w, h, block, role = NULL) list(id = id, x = x, y = y, w = w, h = h, role = role, block = block)
  list(
    id = "canvas-test", name = "Canvas test", kind = "document", design = report_default_design(),
    blocks = list(
      list(id = "p1", type = "paragraph", text = "<p>The page before the free-layout page.</p>"),
      list(id = "cv", type = "canvas", items = list(
        item("title", 0, 0, 6.7, 0.8, list(type = "paragraph", text = "<p>{chart_indicator} in {country}</p>"), role = "title"),
        item("chart", 0, 1, 4.2, 3.2, list(type = "chart", kind = "coverage", indicator = "penta3", admin_level = "national")),
        item("bullets", 4.4, 1, 2.3, 3.2, list(type = "paragraph", font_size = 11,
                                               text = "<ul><li><p>First point</p></li><li><p>Second <strong>bold</strong> point</p></li><li><p><a href=\"https://example.org/?a=1&amp;b=2\">A link</a></p></li></ul>"),
             role = "body"),
        item("table", 0, 4.5, 6.7, 2.4, list(type = "table", kind = "coverage_table")),
        item("picture", 0, 7.2, 3, 2, list(type = "image", src = src, ratio = 2 / 3)),
        item("label", 0.5, 8.5, 2, 0.6, list(type = "paragraph", text = "<p>On top</p>", color = "#ffffff", font_size = 20, align = "center",
                                             valign = "middle"))
      )),
      list(id = "p2", type = "paragraph", text = "<p>The page after the free-layout page.</p>")
    )
  )
}
