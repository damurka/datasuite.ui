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
# The design's `slide_designs` (list(title, content), as report_theme_from_file() reads them from a PowerPoint file's
# slides, see R/report-slide-design.R) are drawn on the slides: a design's background colour, then its decor (logos,
# bands), behind the slide's items.
#
# The PowerPoint file is written by Quire's writers (quire::quire_export(), R/report-quire.R), as the builder writes it:
# every item placed at its box, text as text boxes, charts and pictures as pictures, tables as PowerPoint tables.

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
#' Microsoft PowerPoint (Windows) or LibreOffice. (Quire's: [quire::quire_converter()].)
#' @return `"powerpoint"`, `"libreoffice"`, or `NULL` when neither is installed.
#' @export
report_deck_converter <- function() quire::quire_converter("deck")

# ---- export --------------------------------------------------------------------------------------------------------

#' Export a slide deck
#'
#' Draws every chart and table of a deck from the loaded data and writes it as a PowerPoint file, or as a PDF made from
#' that file (by PowerPoint or LibreOffice, see [report_deck_converter()]), with the report builder's own writers
#' (Quire's, through [quire::quire_export()]). Every item is placed at its box: text as text boxes in the design's fonts
#' and colours, charts and pictures as pictures, tables as PowerPoint tables. The slides' speaker notes are kept.
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
  step(0)
  .rb_quire_write(context, project, pptx, "pptx", i18n)
  step(if (format == "pdf") 0.8 else 1)
  if (format == "pptx") {
    step(1)
    return(invisible(structure(file, converter = NULL)))
  }
  quire::quire_to_pdf(pptx, file, converter)
  step(1)
  invisible(structure(file, converter = converter))
}

# ---- text ----------------------------------------------------------------------------------------------------------

# ---- the file ------------------------------------------------------------------------------------------------------

