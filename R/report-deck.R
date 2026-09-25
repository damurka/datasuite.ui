# Slide decks: a report made of slides, written as a PowerPoint file (and a PDF made from it).
#
# A deck is a report with kind = "deck". Instead of a list of blocks that flow down pages, it has slides, and every
# slide holds items placed at a fixed box. Each item carries a normal report block (text, chart, table, picture), drawn
# the way documents draw them, at the item's box.
#
# Deck fields (project):
#   kind = "deck", name, lang, region (NULL or "": national), design (a document design plus slide_size, see
#   report_slide_size()), slides = list of slides
# Slide: id, layout (one of report_deck_layouts()), items, notes (the speaker notes, plain text, or NULL), design
#   ("title" or "content": which of design$slide_designs it is drawn on; by default "title" for the "title" and
#   "section" layouts, else "content")
# Item: id, x, y, w, h (inches from the slide's top-left corner), role = "title" | "subtitle" | "heading" | "body" |
#   "picture" | NULL, block. A text item's block is a "paragraph" (or "heading" / "list") whose text is HTML (see
#   R/report-text.R), with font_size (pt: the text's default size), align, valign ("top" / "middle" / "bottom") and
#   color; a text box can be filled: fill (hex colour), fill_opacity (0 to 1, 1 by default) and outline (hex colour of
#   its outline), on the block or on the item. Items are stacked in their order: the first is at the back.
#   A "picture" item (a layout's picture placeholder) with no picture in it writes nothing.
#
# The design's `template` (a PowerPoint file's path) is the file the deck is written on top of: its masters, layouts,
# background, fonts and slide size are kept, its slides removed (see R/report-template.R). The design's `slide_designs`
# (list(title, content), as report_theme_from_file() reads them from a PowerPoint file's slides, see
# R/report-slide-design.R) are drawn on the slides: a design's background colour, then its decor (logos, bands), behind
# the slide's items.
#
# In the PowerPoint file every item is placed at its box on a blank slide: text as a text box (a title as the slide's
# title), a chart as an editable drawing (rvg) unless it is formatted as a picture, a table as a PowerPoint table, a
# picture as a picture.

# ---- layouts -------------------------------------------------------------------------------------------------------

#' The layouts of a slide
#'
#' Where a new slide's placeholders go, as PowerPoint places them: each layout lists its boxes as fractions (0 to 1) of
#' the slide's width and height, so they fit a 16:9 or a 4:3 slide.
#'
#' @param lang `NULL` for the names in the three languages (`list(en, fr, pt)`), or `"en"`, `"fr"`, `"pt"` for one.
#' @return A list of layouts, each `list(id, name, items)`; each item is `list(role, type, x, y, w, h)` with `role`
#'   `"title"`, `"subtitle"`, `"heading"`, `"body"` or `"picture"` (a picture placeholder: nothing is written for it
#'   until a picture is put in it), and `type` `"text"` (a text box) or `"content"` (a text, chart, table or picture).
#' @export
report_deck_layouts <- function(lang = NULL) {
  it <- function(role, type, x, y, w, h) list(role = role, type = type, x = x, y = y, w = w, h = h)
  title <- it("title", "text", 0.0688, 0.0532, 0.8625, 0.1933)
  body_top <- 0.2662
  body_h <- 0.6345
  layouts <- list(
    list(id = "title", name = .rb_tx("Title slide", "Diapositive de titre", "Diapositivo de t\u00edtulo"), items = list(
      it("title", "text", 0.125, 0.1637, 0.75, 0.3481),
      it("subtitle", "text", 0.125, 0.5252, 0.75, 0.2414))),
    list(id = "title_content", name = .rb_tx("Title and content", "Titre et contenu", "T\u00edtulo e conte\u00fado"), items = list(
      title,
      it("body", "content", 0.0688, body_top, 0.8625, body_h))),
    list(id = "two_content", name = .rb_tx("Two content", "Deux contenus", "Dois conte\u00fados"), items = list(
      title,
      it("body", "content", 0.0688, body_top, 0.425, body_h),
      it("body", "content", 0.5063, body_top, 0.425, body_h))),
    list(id = "three_content", name = .rb_tx("Three content", "Trois contenus", "Tr\u00eas conte\u00fados"), items = list(
      title,
      it("body", "content", 0.0688, body_top, 0.2775, body_h),
      it("body", "content", 0.3613, body_top, 0.2775, body_h),
      it("body", "content", 0.6538, body_top, 0.2775, body_h))),
    list(id = "comparison", name = .rb_tx("Comparison", "Comparaison", "Compara\u00e7\u00e3o"), items = list(
      title,
      it("heading", "text", 0.0688, 0.2451, 0.4231, 0.1204),
      it("body", "content", 0.0688, 0.3655, 0.4231, 0.5383),
      it("heading", "text", 0.5063, 0.2451, 0.425, 0.1204),
      it("body", "content", 0.5063, 0.3655, 0.425, 0.5383))),
    list(id = "title_only", name = .rb_tx("Title only", "Titre seul", "Apenas t\u00edtulo"), items = list(title)),
    list(id = "section", name = .rb_tx("Section header", "En-t\u00eate de section", "Cabe\u00e7alho de sec\u00e7\u00e3o"), items = list(
      it("title", "text", 0.0688, 0.2493, 0.8625, 0.416),
      it("subtitle", "text", 0.0688, 0.6692, 0.8625, 0.2187))),
    list(id = "blank", name = .rb_tx("Blank", "Vide", "Em branco"), items = list()),
    list(id = "picture_caption", name = .rb_tx("Picture with Caption", "Image avec l\u00e9gende", "Imagem com legenda"), items = list(
      it("title", "text", 0.0688, 0.1333, 0.3563, 0.2),
      it("picture", "content", 0.4688, 0.1111, 0.4625, 0.7778),
      it("body", "content", 0.0688, 0.3533, 0.3563, 0.5356))),
    list(id = "content_caption", name = .rb_tx("Content with Caption", "Contenu avec l\u00e9gende", "Conte\u00fado com legenda"), items = list(
      it("title", "text", 0.0688, 0.1333, 0.3563, 0.2),
      it("body", "content", 0.4688, 0.1111, 0.4625, 0.7778),
      it("body", "content", 0.0688, 0.3533, 0.3563, 0.5356))),
    list(id = "picture_left", name = .rb_tx("Picture and Text", "Image et texte", "Imagem e texto"), items = list(
      it("picture", "content", 0, 0, 0.5, 1),
      it("title", "text", 0.54, 0.08, 0.42, 0.18),
      it("body", "content", 0.54, 0.3, 0.42, 0.6))),
    list(id = "full_picture", name = .rb_tx("Full Picture with Title", "Image pleine page avec titre", "Imagem inteira com t\u00edtulo"), items = list(
      it("picture", "content", 0, 0, 1, 1),
      it("title", "text", 0.05, 0.72, 0.9, 0.18)))
  )
  if (is.null(lang)) {
    lapply(layouts, function(l) { l$name <- unclass(l$name); l })
  } else {
    .rb_in(layouts, if (lang %in% c("en", "fr", "pt")) lang else "en")
  }
}

# ---- the program that makes the PDF --------------------------------------------------------------------------------

#' The program that turns a slide deck into a PDF
#'
#' Microsoft PowerPoint (Windows) or LibreOffice.
#' @return `"powerpoint"`, `"libreoffice"`, or `NULL` when neither is installed.
#' @export
report_deck_converter <- function() {
  if (.Platform$OS.type == "windows") {
    found <- tryCatch(length(utils::readRegistry("PowerPoint.Application\\CurVer", "HCR")) > 0, error = function(e) FALSE)
    if (isTRUE(found)) return("powerpoint")
  }
  if (nzchar(.rb_soffice())) return("libreoffice")
  NULL
}

# PowerPoint opens the file (read-only, without a window) and saves it as PDF. PowerPoint runs once per computer: it
# is closed afterwards only when no other presentation is open in it.
.rb_powerpoint_pdf <- function(pptx, pdf) {
  script <- tempfile(fileext = ".ps1")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    "param([string]$In, [string]$Out)",
    "$ErrorActionPreference = 'Stop'",
    "$pp = New-Object -ComObject PowerPoint.Application",
    "try {",
    "  $pres = $pp.Presentations.Open($In, -1, 0, 0)",
    "  $pres.SaveAs($Out, 32)",
    "  $pres.Close()",
    "} finally { if ($pp.Presentations.Count -eq 0) { $pp.Quit() } }"
  ), script)
  args <- c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(normalizePath(script, winslash = "\\")),
            "-In", shQuote(normalizePath(pptx, winslash = "\\")), "-Out", shQuote(normalizePath(pdf, winslash = "\\", mustWork = FALSE)))
  out <- suppressWarnings(system2("powershell", args, stdout = TRUE, stderr = TRUE, timeout = 300))
  status <- attr(out, "status") %||% 0
  if (!identical(as.integer(status), 0L) || !file.exists(pdf)) {
    .ds_abort(c("x" = "Microsoft PowerPoint could not make the PDF.", "i" = paste(utils::tail(out, 3), collapse = " ")))
  }
  invisible(TRUE)
}

# ---- export --------------------------------------------------------------------------------------------------------

#' Export a slide deck
#'
#' Draws every chart and table of a deck from the loaded data and writes it as a PowerPoint file, or as a PDF made from
#' that file (by PowerPoint or LibreOffice, see [report_deck_converter()]). Every item is placed at its box: text as
#' text boxes in the design's fonts and colours, charts as editable drawings (or pictures, when formatted as pictures or
#' when they cannot be drawn as shapes), tables as PowerPoint tables, pictures as pictures. The slides' speaker notes
#' are kept.
#'
#' @param context A report context ([report_context()]), or what [as_report_context()] turns into one.
#' @param project The deck (see the top of `R/report-deck.R`): `kind = "deck"`, `name`, `design`, `region`, `slides`.
#' @param file The file to write.
#' @param format `"pptx"` or `"pdf"`.
#' @param i18n An object with a `t(key)` method, or `NULL`.
#' @param progress A function called with a number between 0 and 1 as the export advances, or `NULL`.
#' @param converter What makes the PDF: `"auto"` (PowerPoint, else LibreOffice), `"powerpoint"` or `"libreoffice"`.
#' @return `file`, invisibly, with attribute `converter` (what made the PDF; `NULL` for a PowerPoint file).
#' @export
export_deck <- function(context, project, file, format = c("pptx", "pdf"), i18n = NULL, progress = NULL,
                        converter = c("auto", "powerpoint", "libreoffice")) {
  format <- arg_match(format)
  converter <- arg_match(converter)
  if (format == "pdf" && converter == "auto") {
    converter <- report_deck_converter() %||%
      .ds_abort(c("x" = "Making a PDF of slides needs Microsoft PowerPoint or LibreOffice.", "i" = "Download as PowerPoint instead."))
  }
  step <- function(x) if (is.function(progress)) progress(x)
  dir <- tempfile("deck_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  pptx <- if (format == "pptx") file else file.path(dir, "deck.pptx")
  .rb_write_pptx(context, project, pptx, dir, i18n, function(x) step(if (format == "pdf") 0.8 * x else x))
  if (format == "pptx") {
    step(1)
    return(invisible(structure(file, converter = NULL)))
  }
  if (converter == "powerpoint") .rb_powerpoint_pdf(pptx, file) else .rb_libreoffice_pdf(pptx, file)
  step(1)
  invisible(structure(file, converter = converter))
}

.rb_write_pptx <- function(context, project, file, dir, i18n, step) {
  context <- as_report_context(context)
  design <- .rb_design(project$design)
  size <- report_slide_size(design)
  # no region: a national deck
  if (!(is.character(project$region) && length(project$region) == 1 && nzchar(project$region))) project$region <- NULL
  fields <- report_fields(context, project, lang = project$lang %||% (if (is.list(i18n)) i18n$lang) %||% "en")
  slides <- project$slides %||% list()
  total <- max(1, sum(vapply(slides, function(s) length(s$items %||% list()), numeric(1))))
  done <- 0

  # a template: its masters, layouts, background and slide size, on its blank layout
  template <- .rb_deck_template(design$template, size)
  x <- template$x %||% officer::read_pptx()
  layout <- template$layout %||% "Blank"
  master <- template$master %||% "Office Theme"
  with_report_chart_options(context, design = design, {
    for (s in seq_along(slides)) {
      slide <- slides[[s]]
      x <- officer::add_slide(x, layout = layout, master = master)
      # the slide's design (a PowerPoint file's logos and bands, see R/report-slide-design.R), behind its items
      sd <- .rb_slide_design(slide, design)
      x <- .rb_deck_design(x, sd, templated = !is.null(template))
      # its title, subtitle and body text styled as the design's (what an item sets itself is kept)
      items <- lapply(slide$items %||% list(), .rb_designed_item, sd = sd)
      blocks <- lapply(items, function(item) report_resolve_block(item$block %||% list(), project))
      blocks <- .rb_deck_chart_fields(blocks, context, i18n)
      for (k in seq_along(items)) {
        x <- .rb_deck_item(x, items[[k]], blocks[[k]], context, design, fields, i18n, file.path(dir, paste0("s", s, "_", k)))
        done <- done + 1
        step(done / total)
      }
      notes <- slide$notes
      if (is.character(notes) && length(notes) && any(nzchar(notes))) {
        lines <- strsplit(gsub("\r", "", paste(notes, collapse = "\n")), "\n", fixed = TRUE)[[1]]
        pars <- lapply(lines, function(l) officer::fpar(officer::ftext(l)))
        x <- officer::set_notes(x, value = do.call(officer::block_list, pars), location = officer::notes_location_type("body"))
      }
    }
  })
  print(x, target = file)
  # a template keeps its own slide size and theme
  if (is.null(template)) .rb_pptx_finish(file, size, design)
  invisible(file)
}

# The design a slide is drawn on: design$slide_designs[[slide$design]], where slide$design is "title" or "content"
# (by default "title" for the title and section layouts, "content" for the others). NULL when there is none.
.rb_slide_design <- function(slide, design) {
  designs <- design$slide_designs
  if (!is.list(designs) || !length(designs)) return(NULL)
  which <- slide$design
  if (!(is.character(which) && length(which) == 1 && which %in% c("title", "content"))) {
    which <- if (isTRUE(slide$layout %in% c("title", "section"))) "title" else "content"
  }
  d <- designs[[which]]
  if (is.list(d) && length(d)) d else NULL
}

# A text item with the slide design's style for its role (title, subtitle, body) where the item sets none: fill, colour,
# size, alignment, bold and font. The editor draws a slide's text the same way (designedBlock() in deck.ts).
.rb_designed_item <- function(item, sd) {
  role <- item$role
  if (is.null(sd) || !(is.character(role) && length(role) == 1 && role %in% c("title", "subtitle", "body"))) return(item)
  st <- sd[[role]]
  b <- item$block
  if (!is.list(st) || !is.list(b) || !(b$type %||% "") %in% c("paragraph", "heading", "list")) return(item)
  unset <- function(v) is.null(v) || (length(v) == 1 && (is.na(v) || identical(v, "")))
  for (f in c("fill", "fill_opacity", "color", "font_size", "align", "bold", "font")) {
    if (unset(b[[f]]) && !unset(st[[f]])) b[[f]] <- st[[f]]
  }
  item$block <- b
  item
}

# A slide design drawn on the current slide: its background colour, then its decor (pictures, filled rectangles) at their
# boxes. Called before the slide's items are added, so it is behind them. On a deck written on top of a template
# (`templated`), decor found on the master (`from = "master"`) is not drawn: the template's master already draws it.
.rb_deck_design <- function(x, d, templated = FALSE) {
  if (is.null(d)) return(x)
  num <- function(v) { v <- suppressWarnings(as.numeric(v)); if (length(v) == 1 && is.finite(v)) v else NA_real_ }
  bg <- if (is.character(d$background) && length(d$background) == 1) .rb_hex(d$background) else NA
  if (!is.na(bg)) {
    doc <- x$slide$get_slide(x$cursor)$get()
    csld <- xml2::xml_find_first(doc, "//p:cSld")
    old <- xml2::xml_find_first(csld, "p:bg")
    if (!inherits(old, "xml_missing")) xml2::xml_remove(old)
    node <- xml2::read_xml(sprintf(paste0(
      '<p:bg xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main">',
      '<p:bgPr><a:solidFill><a:srgbClr val="%s"/></a:solidFill><a:effectLst/></p:bgPr></p:bg>'), toupper(sub("#", "", bg))))
    xml2::xml_add_child(csld, node, .where = 0)
  }
  for (k in seq_along(d$decor %||% list())) {
    item <- d$decor[[k]]
    if (templated && identical(item$from, "master")) next
    box <-c(x = num(item$x), y = num(item$y), w = num(item$w), h = num(item$h))
    if (anyNA(box) || box[["w"]] <= 0 || box[["h"]] <= 0) next
    name <- paste0("Design ", k)
    if (identical(item$type, "image")) {
      file <- item$file
      if (!(is.character(file) && length(file) == 1 && file.exists(file))) next
      loc <- officer::ph_location(left = box[["x"]], top = box[["y"]], width = box[["w"]], height = box[["h"]], newlabel = name)
      x <- tryCatch(officer::ph_with(x, officer::external_img(file, width = box[["w"]], height = box[["h"]]), location = loc),
                    error = function(e) x)
    } else if (identical(item$type, "rect")) {
      fill <- .rb_text_fill(list(fill = item$fill, fill_opacity = item$fill_opacity %||% 1, outline = item$outline), list())
      if (is.null(fill)) next
      x <- .rb_deck_shape(x, box, name, "<a:p/>", fill = fill)
    }
  }
  x
}

# {chart_indicator} and {chart_year} in a slide's texts are those of the slide's first chart or table, {chart_indicators}
# names the indicators of all of them
.rb_deck_chart_fields <- function(blocks, context, i18n) {
  is_data <- vapply(blocks, function(b) isTRUE(b$type %in% c("chart", "table")), logical(1))
  if (!any(is_data)) return(blocks)
  charts <- blocks[is_data]
  for (i in which(!is_data)) {
    # the text followed by the slide's charts: report_chart_fields() fills it from them
    blocks[[i]] <- report_chart_fields(c(list(blocks[[i]]), charts), context, i18n)[[1]]
  }
  blocks
}

# The location of an item on its slide (inches)
.rb_item_box <- function(item) {
  num <- function(v, default) {
    v <- suppressWarnings(as.numeric(v))
    if (length(v) == 1 && is.finite(v)) v else default
  }
  c(x = num(item$x, 0), y = num(item$y, 0), w = max(0.05, num(item$w, 1)), h = max(0.05, num(item$h, 1)))
}

# One item on the current slide
.rb_deck_item <- function(x, item, b, context, design, fields, i18n, path) {
  box <- .rb_item_box(item)
  name <- as.character(item$id %||% "")
  loc <- officer::ph_location(left = box[["x"]], top = box[["y"]], width = box[["w"]], height = box[["h"]], newlabel = name)
  type <- b$type %||% "paragraph"
  if (type %in% c("chart", "table", "image")) b$box <- unname(box[c("w", "h")])
  # a layout's picture placeholder never filled
  if (.rb_empty_placeholder(item, b)) return(x)
  if (type == "image") {
    # a picture kept in the dataset (src "asset:<id>")
    if (is.character(b$src) && startsWith(b$src, "asset:")) b$src_file <- .rb_asset_file(context, b$src, paste0(path, "_asset"))
    pic <- .rb_image_file(b, paste0(path, "_image"), b$box)
    if (identical(pic$type, "error")) return(.rb_deck_error(x, box, name, pic$message, i18n))
    return(officer::ph_with(x, officer::external_img(pic$file, width = box[["w"]], height = box[["h"]]), location = loc))
  }
  if (type %in% c("chart", "table")) {
    r <- render_report_block(context, b, i18n, design)
    if (identical(r$type, "error")) return(.rb_deck_error(x, box, name, r$message, i18n))
    if (identical(r$type, "table")) {
      ft <- .rb_deck_table(r$value, b$box)
      return(officer::ph_with(x, ft, location = loc))
    }
    if (!.rb_chart_pictured(b) && requireNamespace("rvg", quietly = TRUE)) {
      # an editable drawing: shapes and text PowerPoint can change
      drawn <- tryCatch(officer::ph_with(x, rvg::dml(ggobj = r$value, bg = "transparent"), location = loc), error = function(e) NULL)
      if (!is.null(drawn)) return(drawn)
    }
    # formatted as a picture (cropped, turned, recoloured, a shape), or not drawable as shapes: a 300 dpi picture
    png <- paste0(path, "_chart.png")
    save_report_chart(r, b, png, dpi = 300, design = design)
    file <- png
    if (.rb_chart_pictured(b)) {
      pic <- .rb_image_file(utils::modifyList(b, list(src_file = png, ratio = box[["h"]] / box[["w"]])), paste0(path, "_chart"), b$box)
      file <- pic$file %||% png
    }
    return(officer::ph_with(x, officer::external_img(file, width = box[["w"]], height = box[["h"]]), location = loc))
  }
  if (type %in% c("paragraph", "heading", "list", "note", "quote")) return(.rb_deck_text(x, item, b, box, name, design, fields))
  x
}

# Whether an item is a layout's picture placeholder with nothing in it (no picture chosen)
.rb_empty_placeholder <- function(item, b) {
  if (!identical(item$role, "picture")) return(FALSE)
  type <- b$type %||% ""
  if (type %in% c("chart", "table")) return(FALSE)
  if (type %in% c("paragraph", "heading", "list", "note", "quote")) return(!nzchar(.rb_plain(b$text %||% "")))
  !((is.character(b$src) && length(b$src) == 1 && nzchar(b$src)) || (!is.null(b$src_file) && file.exists(b$src_file)))
}

# A text box's fill and outline, from its item or its block: list(fill, opacity (0 to 1), outline) with the colours as
# "RRGGBB" (NULL when not set), or NULL when it has neither
.rb_text_fill <- function(item, b) {
  pick <- function(field) {
    v <- item[[field]] %||% b[[field]]
    if (is.character(v) && length(v) == 1 && !is.na(v) && nzchar(v)) v else NULL
  }
  hex <- function(v) {
    if (is.null(v) || identical(tolower(v), "transparent") || identical(tolower(v), "none")) return(NULL)
    h <- .rb_hex(v)
    if (is.na(h)) NULL else toupper(sub("#", "", h))
  }
  fill <- hex(pick("fill"))
  outline <- hex(pick("outline"))
  if (is.null(fill) && is.null(outline)) return(NULL)
  op <- suppressWarnings(as.numeric(item$fill_opacity %||% b$fill_opacity %||% 1))
  if (length(op) != 1 || !is.finite(op)) op <- 1
  list(fill = fill, opacity = min(1, max(0, op)), outline = outline)
}

# A text box's fill and outline as DrawingML (the same in PowerPoint and in Word's text boxes)
.rb_text_fill_xml <- function(f) {
  colour <- function(hex, alpha = 1) {
    if (alpha >= 1) return(sprintf('<a:solidFill><a:srgbClr val="%s"/></a:solidFill>', hex))
    sprintf('<a:solidFill><a:srgbClr val="%s"><a:alpha val="%d"/></a:srgbClr></a:solidFill>', hex, round(alpha * 100000))
  }
  paste0(
    if (!is.null(f$fill)) colour(f$fill, f$opacity) else "<a:noFill/>",
    if (!is.null(f$outline)) paste0('<a:ln w="12700">', colour(f$outline), "</a:ln>") else "<a:ln><a:noFill/></a:ln>"
  )
}

# A chart or table that cannot be drawn with this dataset: a short red text in its box
.rb_deck_error <- function(x, box, name, message, i18n) {
  run <- officer::ftext(.rb_error_text(i18n, message %||% ""), officer::fp_text_lite(font.size = 10, color = "#9b2c2c"))
  .rb_deck_shape(x, box, name, officer::to_pml(officer::fpar(run)), anchor = "t")
}

# A table fitted to its box: its text made smaller until it fits (not below 5 pt), then its columns spread to the box's
# width
.rb_deck_table <- function(ft, box) {
  if (is.null(box)) return(ft)
  fits <- function(f) {
    d <- flextable::flextable_dim(f)
    d$widths <= box[1] + 1e-6 && d$heights <= box[2] + 1e-6
  }
  if (!fits(ft)) {
    sizes <- unlist(lapply(c("header", "body"), function(p) ft[[p]]$styles$text$font.size$data))
    start <- suppressWarnings(max(sizes[is.finite(sizes)]))
    if (!is.finite(start)) start <- 10
    best <- ft
    for (s in seq(start - 0.5, 5, by = -0.5)) {
      f <- flextable::fontsize(ft, size = s, part = "all")
      f <- flextable::padding(f, padding.top = 1, padding.bottom = 1, padding.left = 2, padding.right = 2, part = "all")
      f <- flextable::autofit(f, add_w = 0.02, add_h = 0)
      best <- f
      if (fits(f)) break
    }
    ft <- best
  }
  widths <- ft$body$colwidths
  if (length(widths) && sum(widths) > 0) ft <- flextable::width(ft, j = seq_along(widths), width = widths * box[1] / sum(widths))
  ft
}

# ---- text ----------------------------------------------------------------------------------------------------------

# How a text item looks by default, from its role and the design: its size (pt), font, colour, weight, line spacing and
# the space after a paragraph (pt)
.rb_deck_text_style <- function(role, b, design) {
  role <- if (is.character(role) && length(role) == 1) role else "body"
  heading_block <- identical(b$type, "heading")
  level <- as.integer(b$level %||% 1)
  size <- switch(role, title = 32, subtitle = 20, heading = 20, if (heading_block) (if (level <= 1) 28 else 24) else 18)
  given <- suppressWarnings(as.numeric(b$font_size))
  if (length(given) == 1 && is.finite(given) && given > 0) size <- given
  titled <- role == "title" || (heading_block && role == "body")
  color <- if (titled) design$heading_color else if (role == "subtitle") design$muted_color else design$text_color
  own <- .rb_hex(if (is.character(b$color) && length(b$color) == 1) b$color else NA)
  if (!is.na(own)) color <- own
  family <- if (is.character(b$font) && length(b$font) == 1 && nzchar(b$font)) b$font else if (titled) design$heading_font else design$body_font
  bold <- if (is.logical(b$bold) && length(b$bold) == 1 && !is.na(b$bold)) b$bold else role == "heading" || heading_block
  list(size = size, family = family, color = color,
       bold = bold, line = if (role %in% c("title", "subtitle")) 1 else 1.1,
       after = if (role %in% c("title", "subtitle")) 0 else round(size * 0.35, 1))
}

# A text item's lines, as a slide's text box and a free-layout page's text box are written from: each line's runs with
# their size, font and colour filled in from the item's style, and the line's alignment, list marker (kind, level, num),
# indent (pt) and spacing. NULL when the text is empty.
.rb_deck_lines <- function(item, b, design, fields) {
  style <- .rb_deck_text_style(item$role, b, design)
  text <- b$text %||% ""
  html <- grepl("<", text, fixed = TRUE)
  lines <- .rb_lines(.rb_fill(text, fields, escape = html), keep_empty = TRUE)
  if (!any(vapply(lines, length, integer(1)) > 0)) return(NULL)
  given <- function(v) !is.null(v) && length(v) == 1 && !is.na(v)
  lapply(seq_along(lines), function(i) {
    line <- lines[[i]]
    level <- attr(line, "level")
    kind <- attr(line, "list")
    num <- attr(line, "num")
    # a list block of plain lines: one item per line
    if (is.null(level) && isTRUE(b$list %in% c("bullet", "number"))) {
      kind <- b$list
      level <- 1L
      num <- i
    }
    heading <- attr(line, "heading")
    size <- style$size
    family <- style$family
    color <- style$color
    bold <- style$bold
    if (!is.null(heading)) {
      size <- round(size * c(1.5, 1.3, 1.15, 1, 1, 1)[heading] * 2) / 2
      family <- design$heading_font
      bold <- TRUE
      if (heading == 1) color <- design$heading_color
    }
    runs <- lapply(line, function(r) {
      if (!given(r$size)) r$size <- size
      if (!given(r$family)) r$family <- family
      if (!given(r$color)) r$color <- color
      if (bold) r$bold <- TRUE
      r
    })
    align <- attr(line, "align") %||% b$align %||% "left"
    if (!align %in% c("left", "center", "right", "justify")) align <- "left"
    step <- round(max(12, size * 1.2))
    list(runs = runs, size = size, family = family, color = color, align = align, kind = kind, level = level, num = num,
         step = step, left = if (!is.null(level)) step * level else 0, hanging = if (!is.null(kind)) step else 0,
         line = style$line, after = if (i == length(lines)) 0 else style$after)
  })
}

# A line's runs as officer runs (an empty line keeps the height of its text)
.rb_deck_runs <- function(l) {
  runs <- lapply(l$runs, .rb_ftext, text_color = l$color)
  if (!length(runs)) runs <- list(officer::ftext(" ", officer::fp_text_lite(font.size = l$size, font.family = l$family)))
  runs
}

# A text item as a text box (a title as the slide's title): its HTML as paragraphs, lists with PowerPoint bullets and
# numbers
.rb_deck_text <- function(x, item, b, box, name, design, fields) {
  lines <- .rb_deck_lines(item, b, design, fields)
  fill <- .rb_text_fill(item, b)
  # an empty text box is left out, unless it is filled or outlined (a coloured box)
  if (is.null(lines) && is.null(fill)) return(x)
  pars <- if (is.null(lines)) "<a:p/>" else vapply(lines, function(l) {
    indent <- if (l$hanging > 0) list(padding.left = l$left, hanging = l$hanging) else list(padding.left = l$left)
    fp <- do.call(officer::fp_par, c(list(text.align = l$align, line_spacing = l$line, padding.top = 0, padding.bottom = l$after), indent))
    pml <- officer::to_pml(do.call(officer::fpar, c(.rb_deck_runs(l), list(fp_p = fp))))
    if (!is.null(l$kind)) pml <- sub("<a:buNone/>", .rb_deck_bullet(l$kind, l$level, l$num), pml, fixed = TRUE)
    pml
  }, character(1))
  anchor <- switch(b$valign %||% (if (identical(item$role, "title")) "middle" else "top"), middle = "ctr", bottom = "b", "t")
  .rb_deck_shape(x, box, name, paste(pars, collapse = ""), anchor = anchor, title = identical(item$role, "title"), fill = fill)
}

# A list item's marker as PowerPoint writes it, by level as the editor draws them: bullet, circle, square; 1. a. i.
.rb_deck_bullet <- function(kind, level, num) {
  k <- ((as.integer(level %||% 1) - 1) %% 3) + 1
  if (identical(kind, "number")) {
    type <- c("arabicPeriod", "alphaLcPeriod", "romanLcPeriod")[k]
    return(sprintf('<a:buFont typeface="+mj-lt"/><a:buAutoNum type="%s" startAt="%d"/>', type, max(1L, as.integer(num %||% 1))))
  }
  sprintf('<a:buFont typeface="Arial"/><a:buChar char="%s"/>', c("&#8226;", "&#9702;", "&#9642;")[k])
}

# A text box at `box` holding the paragraphs `pars` (PowerPoint XML); `title`: the slide's title placeholder; `fill`:
# its fill and outline (.rb_text_fill()), none by default. Added last, so it is in front of what the slide has already.
.rb_deck_shape <- function(x, box, name, pars, anchor = "t", title = FALSE, fill = NULL) {
  emu <- function(v) format(round(v * 914400), scientific = FALSE, trim = TRUE)
  slide <- x$slide$get_slide(x$cursor)
  doc <- slide$get()
  id <- length(xml2::xml_find_all(doc, "//p:cNvPr")) + 2
  xml <- paste0(
    '<p:sp xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ',
    'xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main">',
    '<p:nvSpPr><p:cNvPr id="', id, '" name="', htmltools::htmlEscape(name, attribute = TRUE), '"/>',
    if (title) '<p:cNvSpPr><a:spLocks noGrp="1"/></p:cNvSpPr><p:nvPr><p:ph type="title"/></p:nvPr>' else '<p:cNvSpPr txBox="1"/><p:nvPr/>',
    '</p:nvSpPr><p:spPr><a:xfrm><a:off x="', emu(box[["x"]]), '" y="', emu(box[["y"]]), '"/><a:ext cx="', emu(box[["w"]]), '" cy="', emu(box[["h"]]),
    '"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom>', if (is.null(fill)) "<a:noFill/>" else .rb_text_fill_xml(fill), '</p:spPr>',
    '<p:txBody><a:bodyPr wrap="square" rtlCol="0" anchor="', anchor, '"><a:noAutofit/></a:bodyPr><a:lstStyle/>',
    pars, "</p:txBody></p:sp>"
  )
  xml2::xml_add_child(xml2::xml_find_first(doc, "//p:spTree"), xml2::as_xml_document(xml))
  x
}

# ---- the file ------------------------------------------------------------------------------------------------------

# The written file set to the deck's slide size (officer's template is 4:3; the master and layouts are widened with it,
# for slides added in PowerPoint later) and the design's fonts as the theme's heading and body fonts
.rb_pptx_finish <- function(file, size, design) {
  if (!requireNamespace("zip", quietly = TRUE)) return(invisible(FALSE))
  dir <- tempfile("pptx_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(file, exdir = dir)
  read <- function(p) readChar(p, file.info(p)$size, useBytes = TRUE)
  write <- function(txt, p) writeChar(txt, p, eos = NULL, useBytes = TRUE)
  emu <- function(v) format(round(v * 914400), scientific = FALSE, trim = TRUE)

  pres_path <- file.path(dir, "ppt", "presentation.xml")
  pres <- read(pres_path)
  old <- regmatches(pres, regexec('<p:sldSz cx="([0-9]+)" cy="([0-9]+)"', pres))[[1]]
  # 16:9 as PowerPoint writes it (13.333 in is 13 1/3 in)
  cx <- if (abs(size[1] - 13.333) < 0.001) "12192000" else emu(size[1])
  pres <- sub("<p:sldSz [^>]*/>", sprintf('<p:sldSz cx="%s" cy="%s"/>', cx, emu(size[2])), pres)
  write(pres, pres_path)

  if (length(old) == 3) {
    fx <- as.numeric(cx) / as.numeric(old[2])
    fy <- size[2] * 914400 / as.numeric(old[3])
    scale <- function(txt, attr, f) {
      if (abs(f - 1) < 1e-6) return(txt)
      pattern <- paste0(" ", attr, '="([0-9]+)"')
      m <- gregexpr(pattern, txt)
      hits <- regmatches(txt, m)[[1]]
      if (!length(hits)) return(txt)
      values <- as.numeric(sub(pattern, "\\1", hits))
      regmatches(txt, m) <- list(sprintf(' %s="%s"', attr, format(round(values * f), scientific = FALSE, trim = TRUE)))
      txt
    }
    for (p in list.files(file.path(dir, "ppt"), pattern = "^slide(Master|Layout)[0-9]+\\.xml$", recursive = TRUE, full.names = TRUE)) {
      txt <- read(p)
      txt <- scale(scale(txt, "x", fx), "cx", fx)
      txt <- scale(scale(txt, "y", fy), "cy", fy)
      write(txt, p)
    }
  }

  theme_path <- file.path(dir, "ppt", "theme", "theme1.xml")
  if (file.exists(theme_path)) {
    theme <- read(theme_path)
    font <- function(txt, which, value) {
      if (!is.character(value) || !nzchar(value)) return(txt)
      sub(paste0("(<a:", which, ">\\s*<a:latin typeface=\")[^\"]*\""), paste0("\\1", htmltools::htmlEscape(value, attribute = TRUE), "\""), txt)
    }
    write(font(font(theme, "majorFont", design$heading_font), "minorFont", design$body_font), theme_path)
  }

  out <- tempfile(fileext = ".pptx")
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- c(files[files == "[Content_Types].xml"], files[files != "[Content_Types].xml"])
  zip::zip(out, files = files, root = dir, mode = "mirror")
  file.copy(out, file, overwrite = TRUE)
  unlink(out)
  invisible(TRUE)
}
