# Office files as themes.
#
# A PowerPoint file (.pptx, .potx) or a Word file (.docx, .dotx) can give a report its theme: report_theme_from_file()
# reads the file's colours, fonts, (Word) text sizes and page and (PowerPoint) slide designs into a design, as the
# ready-made themes are (report_themes()). The reading is Quire's (quire::quire_theme_from_file(), the builder's own
# reader run through V8), so a theme read here is the one the builder reads. The file itself is kept with the theme
# (its `template`), but the files are written by Quire's writers, which draw the theme, not on top of the file.
#
# A slide design (`slide_designs$title`, `slide_designs$content`):
#   background  "#rrggbb" or NULL
#   decor       list of, back first:
#                 list(type = "image", file = <picture file, png / jpg>, x, y, w, h, from)
#                 list(type = "rect", fill = "#rrggbb", fill_opacity = 0..1, outline = "#rrggbb" or NULL, x, y, w, h, from)
#               `from` ("master", "layout" or "slide") says where it was found: a deck written on top of the same
#               file as its template already has the master's, and does not draw them again
#   title, subtitle, body   the style of that text, or NULL:
#                 list(x, y, w, h, fill = "#rrggbb" or NULL, fill_opacity, color = "#rrggbb", font_size (pt), bold,
#                      align = "left" | "center" | "right", font = family or NULL)
# Boxes are in inches from the slide's top-left corner, on the builder's slide of the same shape.

# What an Office file is, from what it holds: "pptx", "docx", or NULL (not an Office file, or not readable)
.rb_office_kind <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  files <- tryCatch(utils::unzip(path, list = TRUE)$Name, error = function(e) NULL, warning = function(w) NULL)
  if ("ppt/presentation.xml" %in% files) return("pptx")
  if ("word/document.xml" %in% files) return("docx")
  NULL
}

#' A report theme from a PowerPoint or Word file
#'
#' Reads the colours and fonts of an Office theme or template (PowerPoint `.pptx` / `.potx`, Word `.docx` / `.dotx`)
#' into a report theme, as [report_themes()] gives them: the accent is the theme's first accent colour, the text colour
#' its first dark colour, the chart palette its six accent colours, the heading and body fonts its heading and body
#' fonts. A Word file's styles give the heading colour (Heading 1's), the fonts and the text sizes when they set them,
#' and its page size, orientation and margins. A PowerPoint file's master gives the title colour and fonts, the slide
#' size and the background colour. The reading is Quire's ([quire::quire_theme_from_file()]).
#'
#' The same file can be given to the export as the design's `template` (its path): see [export_deck()] and
#' [export_report()].
#'
#' @param path The file.
#' @param name The theme's name (by default the file's name without its extension).
#' @return A design (see [report_default_design()]) with `theme` (`"custom_<hash of the file>"`), `name`
#'   (`list(en, fr, pt)`), and `template_kind` (`"pptx"` or `"docx"`); for a PowerPoint file also `slide_size`
#'   (`"16:9"` or `"4:3"`, the nearer), `background` (the master's background colour, or `NULL`) and `slide_designs`:
#'   `list(title, content)`, the look of the file's title slide and of its other slides (either `NULL` when not found).
#'   A slide design is `list(background, decor, title, subtitle, body)`: `background` a colour or `NULL`; `decor` what
#'   is drawn behind a slide's content, back first, each `list(type = "image", file, x, y, w, h)` (a logo: `file` is
#'   the picture, copied out of the file into a temporary folder) or `list(type = "rect", fill, fill_opacity, outline,
#'   x, y, w, h)` (a band or bar), each also with `from` (`"master"`, `"layout"` or `"slide"`: where it was found);
#'   `title`, `subtitle`, `body` the style of that text, `list(x, y, w, h, fill, fill_opacity, color, font_size, bold,
#'   align, font)`, or `NULL`. Boxes are in inches from the slide's top-left corner; colours are `"#rrggbb"`.
#'   [export_deck()] draws a design's background and decor on its slides.
#' @export
report_theme_from_file <- function(path, name = NULL) {
  if (is.null(.rb_office_kind(path))) .ds_abort(c("x" = "This is not a PowerPoint or Word file (.pptx, .potx, .docx, .dotx)."))
  out <- report_default_design()
  label <- name %||% tools::file_path_sans_ext(basename(path))
  th <- tryCatch(quire::quire_theme_from_file(path, name = label, default_accent = out$accent),
                 error = function(e) .ds_abort(c("x" = conditionMessage(e))))
  for (k in names(th)) out[k] <- list(th[[k]])
  out$theme <- paste0("custom_", substr(unname(tools::md5sum(path)), 1, 10))
  out$name <- list(en = label, fr = label, pt = label)
  out$palette <- tolower(unlist(out$palette))
  if (!is.null(th$slide_designs)) out$slide_designs <- .rb_design_files(th$slide_designs)
  out
}

# The designs' pictures (data URLs as Quire reads them) as files in a temporary folder, as export_deck() and the
# dataset's store take them; a large one (over 800 KB) made a JPEG of at most 1920 pixels wide
.rb_design_files <- function(designs) {
  dir <- tempfile("slide_design_")
  dir.create(dir, showWarnings = FALSE)
  lapply(designs, function(d) {
    if (!is.list(d)) return(d)
    d$decor <- lapply(d$decor %||% list(), function(item) {
      if (!identical(item$type, "image") || !is.character(item$src)) return(item)
      file <- file.path(dir, item$name %||% basename(tempfile("picture_", fileext = ".png")))
      writeBin(jsonlite::base64_dec(sub("^data:[^,]*,", "", item$src)), file)
      if (isTRUE(file.info(file)$size > 8e5) && requireNamespace("magick", quietly = TRUE)) {
        small <- sub("\\.[A-Za-z]+$", ".jpg", file)
        ok <- tryCatch({
          img <- magick::image_read(file)
          if (magick::image_info(img)$width > 1920) img <- magick::image_resize(img, "1920x")
          magick::image_write(magick::image_background(img, "white"), small, format = "jpeg", quality = 82)
          TRUE
        }, error = function(e) FALSE)
        if (ok) {
          if (small != file) unlink(file)
          file <- small
        }
      }
      list(type = "image", file = normalizePath(file, winslash = "/"), x = item$x, y = item$y, w = item$w, h = item$h,
           from = item$from)
    })
    d
  })
}
