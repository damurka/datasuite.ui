# Writing a report built in the report builder, from R.
#
# The files are written by Quire's writers, the builder's own (quire::quire_export(), R/report-quire.R): a report
# written here is the file the builder downloads. The PDF is made FROM the Word file (Microsoft Word, or LibreOffice),
# so both have the same pages and page breaks; report_final_pages() renders those pages. Only when neither program is
# installed is the PDF printed from Quire's printable page in a browser (close to the Word file, but not page for page).

# ---- chart options -------------------------------------------------------------------------------------------------

# The chart options a report renders with (see cd_finish_plot()): the theme's font, then the dataset-wide options saved
# for reports, then those saved per chart type / chart id
.report_chart_options <- function(context, design = NULL) {
  saved <- as_report_context(context)$chart_options() %||% list()
  types <- saved[startsWith(names(saved) %||% character(), "report/")]
  names(types) <- sub("^report/", "", names(types))
  base <- if (!is.null(design)) .rb_theme_chart_options(.rb_design(design))
  list(default = merge_chart_options(base, saved[["default"]]), types = types)
}

#' Run code with the report's saved chart options in effect
#' @param context A report context ([report_context()]), or what [as_report_context()] turns into one.
#' @param code Code that draws charts.
#' @param design The report's design, whose fonts the charts use (`NULL`: the charts keep their own).
#' @return The value of `code`.
#' @export
with_report_chart_options <- function(context, code, design = NULL) {
  old <- options(datasuite.report_chart_options = .report_chart_options(context, design))
  on.exit(options(old), add = TRUE)
  force(code)
}

# ---- the program that makes the PDF --------------------------------------------------------------------------------

#' The program that turns the Word file into a PDF
#'
#' Microsoft Word (Windows) or LibreOffice. With one of them the PDF has exactly the Word file's pages. (Quire's:
#' [quire::quire_converter()].)
#' @return `"word"`, `"libreoffice"`, or `NULL` when neither is installed.
#' @export
report_converter <- function() quire::quire_converter("document")

# ---- export --------------------------------------------------------------------------------------------------------

#' Export a report built in the report builder
#'
#' Draws every chart and table from the loaded data and writes the report as a Word document or a PDF, with the report
#' builder's own writers (Quire's, through [quire::quire_export()]). The PDF is made from the Word file (see
#' [report_converter()]), so the two match page for page. A slide deck (`kind = "deck"`) is
#' written by [export_deck()], as a PowerPoint file or a PDF.
#'
#' @param context A report context ([report_context()]), or what [as_report_context()] turns into one.
#' @param project The report: `list(name, design, cover, blocks)` (such as the app's standard reports, see
#'   [report_register()]), or a slide deck.
#' @param file The file to write.
#' @param format `"docx"` or `"pdf"` for a document; `"pptx"` or `"pdf"` for a slide deck (by default `"docx"` for a
#'   document, `"pptx"` for a deck).
#' @param i18n An object with a `t(key)` method, or `NULL`.
#' @param subtitle Not used any more: the cover page has its own subtitle. Kept so older calls work.
#' @param progress A function called with a number between 0 and 1 as the export advances, or `NULL`.
#' @param converter `"auto"` (Word, else LibreOffice, else a browser), or one of `"word"`, `"libreoffice"`, `"browser"`;
#'   for a deck `"auto"`, `"powerpoint"` or `"libreoffice"` (`"word"` is taken as PowerPoint).
#'
#' @return `file`, invisibly, with attribute `converter`: what made the PDF (`"word"`, `"libreoffice"`, `"browser"`,
#'   `"powerpoint"`) or, for Word, what finished the file (`"word"`, or `NULL`).
#' @export
export_report <- function(context, project, file, format = c("docx", "pdf", "pptx"), i18n = NULL, subtitle = NULL, progress = NULL,
                          converter = c("auto", "word", "libreoffice", "browser", "powerpoint")) {
  deck <- identical(project$kind, "deck")
  if (missing(format)) format <- if (deck) "pptx" else "docx"
  format <- arg_match(format)
  converter <- arg_match(converter)
  if (deck) {
    if (format == "docx") .ds_abort(c("x" = "A slide deck is written as PowerPoint ({.val pptx}) or PDF, not Word."))
    converter <- switch(converter, word = "powerpoint", browser = "auto", converter)
    return(export_deck(context, project, file, format, i18n = i18n, progress = progress, converter = converter))
  }
  if (format == "pptx") .ds_abort(c("x" = "A document is written as Word ({.val docx}) or PDF; only slide decks are PowerPoint files."))
  if (converter == "powerpoint") converter <- "auto"
  if (converter == "auto") converter <- report_converter() %||% "browser"
  step <- function(x) if (is.function(progress)) progress(x)
  step(0)

  dir <- tempfile("report_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  if (format == "pdf" && converter == "browser") {
    html <- file.path(dir, "report.html")
    .rb_quire_write(context, project, html, "html", i18n)
    step(0.8)
    quire::quire_html_pdf(html, file)
    step(1)
    return(invisible(structure(file, converter = "browser")))
  }

  docx <- if (format == "docx") file else file.path(dir, "report.docx")
  .rb_quire_write(context, project, docx, "docx", i18n)
  step(0.8)
  if (format == "docx") {
    # Word fills in the contents page and embeds the fonts; without it Word asks to update the contents when opened
    finished <- if (converter == "word") tryCatch(quire::quire_word_finish(docx), error = function(e) NULL)
    step(1)
    return(invisible(structure(file, converter = if (isTRUE(finished)) "word")))
  }
  quire::quire_to_pdf(docx, file, converter)
  step(1)
  invisible(structure(file, converter = converter))
}

#' The pages of a report as they will be printed
#'
#' Writes the Word file, makes the PDF from it and renders each page as a picture: what the builder's "Final pages" view
#' shows. Needs Microsoft Word or LibreOffice ([report_converter()]). For a slide deck, the PowerPoint file and its PDF
#' (PowerPoint or LibreOffice, [report_deck_converter()]): one picture per slide.
#'
#' @inheritParams export_report
#' @param dpi Resolution of the page pictures.
#' @return `list(pages = <character vector of PNG files>, converter, pdf = <path>)`. The files are in a temporary folder
#'   the caller removes.
#' @export
report_final_pages <- function(context, project, i18n = NULL, dpi = 60, progress = NULL) {
  deck <- identical(project$kind, "deck")
  converter <- if (deck) report_deck_converter() else report_converter()
  if (is.null(converter)) {
    .ds_abort(c("x" = if (deck) "Showing the final slides needs Microsoft PowerPoint or LibreOffice on the computer running the app."
               else "Showing the final pages needs Microsoft Word or LibreOffice on the computer running the app."))
  }
  # the pages are drawn by DataSuite inside it (report-print.R), else by pdftools
  if (!.ds_can_print() && !requireNamespace("pdftools", quietly = TRUE)) {
    .ds_abort(c("x" = "Showing the final pages needs the {.pkg pdftools} package."))
  }
  out <- tempfile("pages_")
  dir.create(out)
  pdf <- file.path(out, "report.pdf")
  export_report(context, project, pdf, "pdf", i18n = i18n, progress = function(x) if (is.function(progress)) progress(0.9 * x), converter = converter)
  if (.ds_can_print()) {
    .ds_print(pdf, pages = out, dpi = dpi)
    files <- sort(list.files(out, pattern = "^page_[0-9]+[.]png$", full.names = TRUE))
  } else {
    n <- pdftools::pdf_info(pdf)$pages
    files <- file.path(out, sprintf("page_%03d.png", seq_len(n)))
    pdftools::pdf_convert(pdf, format = "png", dpi = dpi, filenames = files, verbose = FALSE)
  }
  if (is.function(progress)) progress(1)
  list(pages = files, converter = converter, pdf = pdf)
}

# ---- what is drawn -------------------------------------------------------------------------------------------------

.rb_regions <- function(context) {
  tryCatch(sort(unique(as_report_context(context)$regions())), error = function(e) character())
}

#' A report picture as a data URL
#'
#' The picture a block refers to as `"asset:<id>"` (see `CacheConnection$set_report_asset()`), for showing in a browser.
#' @param context A report context ([report_context()]), or what [as_report_context()] turns into one.
#' @param id The picture's id (with or without `"asset:"`).
#' @return A `data:` URL, or `NULL` when the dataset has no such picture.
#' @export
report_asset_data_url <- function(context, id) {
  asset <- tryCatch(as_report_context(context)$asset_get(sub("^asset:", "", id)), error = function(e) NULL)
  if (is.null(asset) || !is.raw(asset$data)) return(NULL)
  paste0("data:", asset$type %||% "image/png", ";base64,", jsonlite::base64_enc(asset$data))
}

#' Store a picture for the reports
#'
#' Keeps a picture once in the dataset, from a `data:` URL (as the report builder uploads it) or from a web address
#' (downloaded now, so the report does not need the internet later). A picture larger than 4000 pixels is made smaller;
#' formats Word cannot show (WebP, for example) are converted to PNG.
#' @param context A report context ([report_context()]), or what [as_report_context()] turns into one.
#' @param id The picture's id.
#' @param src A `data:` URL, or an `http(s)://` address.
#' @return A list: `id`, `ratio` (height / width) and `url` (a `data:` URL of the picture as stored), invisibly.
#' @export
report_store_asset <- function(context, id, src) {
  if (!is.character(src) || length(src) != 1 || !nzchar(src)) .ds_abort(c("x" = "{.arg src} must be a data URL or a web address."))
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  if (startsWith(src, "data:")) {
    writeBin(jsonlite::base64_dec(sub("^data:[^,]*,", "", src)), file)
  } else if (grepl("^https?://", src)) {
    old <- options(timeout = 30)
    on.exit(options(old), add = TRUE)
    ok <- tryCatch(utils::download.file(src, file, mode = "wb", quiet = TRUE), error = function(e) conditionMessage(e), warning = function(w) conditionMessage(w))
    if (!identical(ok, 0L)) .ds_abort(c("x" = "The picture could not be downloaded from {.url {src}}.", "i" = if (is.character(ok)) ok))
  } else {
    .ds_abort(c("x" = "{.arg src} must be a data URL or a web address."))
  }
  if (!requireNamespace("magick", quietly = TRUE)) .ds_abort(c("x" = "The {.pkg magick} package is needed to store pictures."))
  img <- tryCatch(magick::image_read(file)[1], error = function(e) NULL)
  if (is.null(img)) .ds_abort(c("x" = "This is not a picture (PNG, JPEG, GIF, WebP or SVG)."))
  info <- magick::image_info(img)
  fmt <- tolower(info$format[[1]])
  w <- info$width[[1]]
  h <- info$height[[1]]
  if (max(w, h) > 4000) {
    img <- magick::image_resize(img, if (w >= h) "4000x" else "x4000")
    info <- magick::image_info(img)
    w <- info$width[[1]]
    h <- info$height[[1]]
    fmt <- paste0(fmt, "-resized")
  }
  if (fmt %in% c("jpeg", "jpg")) {
    type <- "image/jpeg"
    data <- readBin(file, "raw", file.info(file)$size)
  } else if (fmt == "png") {
    type <- "image/png"
    data <- readBin(file, "raw", file.info(file)$size)
  } else {
    # anything else (resized, WebP, GIF, SVG, TIFF...) as PNG, which Word shows
    type <- "image/png"
    out <- tempfile(fileext = ".png")
    on.exit(unlink(out), add = TRUE)
    magick::image_write(img, out, format = "png")
    data <- readBin(out, "raw", file.info(out)$size)
  }
  as_report_context(context)$asset_set(id, list(type = type, data = data))
  invisible(list(id = id, ratio = h / w, url = paste0("data:", type, ";base64,", jsonlite::base64_enc(data))))
}

# ---- picture styles ------------------------------------------------------------------------------------------------

#' A country's flag
#'
#' Downloaded once from flagcdn.com and kept in the package's context folder. `NULL` when the country is not known or the
#' flag cannot be downloaded (no internet): the cover is then drawn without it.
#' @param iso3 The country's ISO3 code.
#' @return The path of a PNG file, or `NULL`.
#' @export
report_flag_file <- function(iso3) .rb_flag_file(iso3)

.rb_flag_file <- function(iso3) {
  if (is.null(iso3) || !nzchar(iso3 %||% "")) return(NULL)
  iso2 <- tryCatch(tolower(countrycode::countrycode(iso3, "iso3c", "iso2c", warn = FALSE)), error = function(e) NA)
  if (is.na(iso2) || !nzchar(iso2)) return(NULL)
  dir <- tools::R_user_dir("datasuite.ui", "cache")
  dir.create(file.path(dir, "flags"), recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, "flags", paste0(iso2, ".png"))
  if (!file.exists(path)) {
    ok <- tryCatch({
      suppressWarnings(utils::download.file(paste0("https://flagcdn.com/w320/", iso2, ".png"), path, mode = "wb", quiet = TRUE))
      file.exists(path) && file.info(path)$size > 100
    }, error = function(e) FALSE)
    if (!isTRUE(ok)) {
      unlink(path)
      return(NULL)
    }
  }
  path
}
