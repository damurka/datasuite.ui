# Slide designs: the look a PowerPoint file's slides have (logos, bands, the title's box), read by report_theme_from_file()
# and drawn by export_deck()

.sd_user_file <- function() {
  src <- "C:/Users/Murage/Downloads/Data extraction ppt.pptx"
  if (!file.exists(src)) return(NULL)
  copy <- tempfile(fileext = ".pptx")
  file.copy(src, copy)
  copy
}

.sd_slide_xml <- function(dir, n) {
  gsub(">\\s+<", "><", paste(readLines(file.path(dir, "ppt", "slides", paste0("slide", n, ".xml")), warn = FALSE, encoding = "UTF-8"), collapse = ""))
}

# A stand-in for the dataset: a deck of texts only reads no data
.sd_cache <- function() list(chart_options = list(), get_chart_options = function(...) NULL)

.sd_png <- function(file, colour = "#c0392b") {
  grDevices::png(file, width = 120, height = 60)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0, 0, 1, 1, col = colour, border = NA)
  grDevices::dev.off()
  file
}

test_that("a template without slides gives its designs from its title and content layouts, with the master's decor", {
  skip_if_not_installed("zip")
  potx <- .rb_test_pptx_template(tempfile(fileext = ".potx"))
  th <- report_theme_from_file(potx)
  expect_named(th$slide_designs, c("title", "content"))
  for (which in c("title", "content")) {
    d <- th$slide_designs[[which]]
    expect_false(is.null(d), info = which)
    expect_named(d, c("background", "decor", "title", "subtitle", "body"), info = which)
    # the band the master draws (accent1, along the bottom)
    rects <- Filter(function(x) identical(x$type, "rect"), d$decor)
    expect_length(rects, 1)
    expect_identical(rects[[1]]$fill, "#b5472b")
    expect_identical(rects[[1]]$from, "master")
    expect_equal(rects[[1]]$y, 7.2, tolerance = 0.01)
    expect_equal(rects[[1]]$w, 13.333, tolerance = 0.01)
    # the master's background
    expect_identical(d$background, "#f3eee4")
    # the title's font is the theme's heading font
    expect_identical(d$title$font, "Georgia")
    expect_true(d$title$font_size > 0)
    expect_true(d$title$align %in% c("left", "center", "right"))
  }
  expect_false(is.null(th$slide_designs$title$subtitle))
  expect_false(is.null(th$slide_designs$content$body))
  expect_identical(th$slide_designs$content$body$font, "Verdana")
})

test_that("the user's deck: logos on the title slide, a green title band on the content slides", {
  file <- .sd_user_file()
  skip_if(is.null(file), "The user's PowerPoint file is not on this computer")
  th <- report_theme_from_file(file)
  size <- c(13.333, 7.5)
  title <- th$slide_designs$title
  images <- Filter(function(x) identical(x$type, "image"), title$decor)
  expect_length(images, 2)
  for (im in images) {
    expect_true(file.exists(im$file))
    expect_identical(tolower(tools::file_ext(im$file)), "png")
    # logo-like: no big picture
    expect_lt(im$w * im$h, 0.2 * prod(size))
  }
  # the Countdown logo top left, the APHRC logo bottom right
  expect_lt(images[[1]]$x, 1)
  expect_lt(images[[1]]$y, 0.5)
  expect_gt(images[[2]]$x + images[[2]]$w, size[1] - 1)
  expect_gt(images[[2]]$y + images[[2]]$h, size[2] - 1)
  expect_identical(vapply(images, function(im) im$from, ""), c("slide", "slide"))
  # the title slide's title: the big bold text box
  expect_equal(title$title$font_size, 36)
  expect_true(title$title$bold)
  expect_identical(title$title$align, "center")
  expect_null(title$title$fill)
  expect_false(is.null(title$subtitle))

  content <- th$slide_designs$content
  expect_length(content$decor, 0)
  expect_identical(content$title$fill, "#4ea72e")
  expect_identical(content$title$color, "#ffffff")
  expect_equal(content$title$fill_opacity, 1)
  expect_identical(content$title$align, "left")
  expect_equal(content$title$y, 0.399, tolerance = 0.01)
  expect_false(is.null(content$body))
  expect_identical(content$body$color, "#000000")
})
