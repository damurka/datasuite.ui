# Writing a report built in the report builder.
#
# The Word file is written first. The PDF is made FROM that Word file (Microsoft Word, or LibreOffice), so both have the
# same pages and page breaks; report_final_pages() renders those pages for the builder to show. Only when neither program
# is installed is the PDF printed from HTML in a browser (close to the Word file, but not page for page).
#
# Layout rules that keep Word from moving things around: every chart and image is placed inline (never floating), two
# half-width blocks sit in the cells of an invisible two-column table with fixed widths, and the theme's fonts are
# embedded when Word saves the file.

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
#' Microsoft Word (Windows) or LibreOffice. With one of them the PDF has exactly the Word file's pages.
#' @return `"word"`, `"libreoffice"`, or `NULL` when neither is installed.
#' @export
report_converter <- function() {
  if (.Platform$OS.type == "windows") {
    found <- tryCatch(length(utils::readRegistry("Word.Application\\CurVer", "HCR")) > 0, error = function(e) FALSE)
    if (isTRUE(found)) return("word")
  }
  if (nzchar(.rb_soffice())) return("libreoffice")
  NULL
}

.rb_soffice <- function() {
  candidates <- c(
    Sys.which(c("soffice", "libreoffice")),
    file.path(Sys.getenv("PROGRAMFILES"), "LibreOffice/program/soffice.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "LibreOffice/program/soffice.exe"),
    "/Applications/LibreOffice.app/Contents/MacOS/soffice"
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit)) hit[[1]] else ""
}

# Word opens the file, fills in the contents page and page numbers, embeds the fonts, saves it and (with `pdf`) exports
# the PDF: the Word file and the PDF are then the same document.
.rb_word_finish <- function(docx, pdf = NULL) {
  script <- tempfile(fileext = ".ps1")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    "param([string]$In, [string]$Out)",
    "$ErrorActionPreference = 'Stop'",
    "$word = New-Object -ComObject Word.Application",
    "$word.Visible = $false",
    "$word.DisplayAlerts = 0",
    "try {",
    "  $doc = $word.Documents.Open($In, $false, $false, $false)",
    "  foreach ($t in $doc.TablesOfContents) { $t.Update() }",
    "  $doc.Fields.Update() | Out-Null",
    "  $doc.EmbedTrueTypeFonts = $true",
    "  $doc.SaveSubsetFonts = $true",
    "  $doc.Save()",
    "  if ($Out) { $doc.ExportAsFixedFormat($Out, 17) }",
    "  $doc.Close($false)",
    "} finally { $word.Quit() }"
  ), script)
  args <- c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(normalizePath(script, winslash = "\\")),
            "-In", shQuote(normalizePath(docx, winslash = "\\")))
  if (!is.null(pdf)) args <- c(args, "-Out", shQuote(normalizePath(pdf, winslash = "\\", mustWork = FALSE)))
  out <- suppressWarnings(system2("powershell", args, stdout = TRUE, stderr = TRUE, timeout = 300))
  status <- attr(out, "status") %||% 0
  if (!identical(as.integer(status), 0L) || (!is.null(pdf) && !file.exists(pdf))) {
    .ds_abort(c("x" = "Microsoft Word could not make the PDF.", "i" = paste(utils::tail(out, 3), collapse = " ")))
  }
  invisible(TRUE)
}

# LibreOffice turns a Word file or a PowerPoint file into a PDF
.rb_libreoffice_pdf <- function(docx, pdf) {
  outdir <- tempfile("lo_")
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  out <- suppressWarnings(system2(.rb_soffice(), c("--headless", "--convert-to", "pdf", "--outdir", shQuote(outdir), shQuote(docx)),
                                  stdout = TRUE, stderr = TRUE, timeout = 300))
  made <- file.path(outdir, sub("\\.[^.]*$", ".pdf", basename(docx)))
  if (!file.exists(made)) .ds_abort(c("x" = "LibreOffice could not make the PDF.", "i" = paste(utils::tail(out, 3), collapse = " ")))
  file.copy(made, pdf, overwrite = TRUE)
  invisible(TRUE)
}

# ---- export --------------------------------------------------------------------------------------------------------

#' Export a report built in the report builder
#'
#' Draws every chart and table from the loaded data and writes the report as a Word document or a PDF. The PDF is made
#' from the Word file (see [report_converter()]), so the two match page for page. A slide deck (`kind = "deck"`) is
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

  dir <- tempfile("report_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  parts <- .rb_prepare(context, project, dir, i18n, function(x) step(0.7 * x))
  if (format == "pdf" && converter == "browser") {
    .rb_write_pdf(parts, file, dir, i18n)
    step(1)
    return(invisible(structure(file, converter = "browser")))
  }

  docx <- if (format == "docx") file else file.path(dir, "report.docx")
  .rb_write_docx(parts, docx, i18n)
  step(0.8)
  if (format == "docx") {
    # Word fills in the contents page and embeds the fonts; without it Word asks to update the contents when opened
    finished <- if (converter == "word") tryCatch(.rb_word_finish(docx), error = function(e) NULL)
    step(1)
    return(invisible(structure(file, converter = if (isTRUE(finished)) "word")))
  }
  if (converter == "word") .rb_word_finish(docx, file) else .rb_libreoffice_pdf(docx, file)
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
  if (!requireNamespace("pdftools", quietly = TRUE)) .ds_abort(c("x" = "Showing the final pages needs the {.pkg pdftools} package."))
  out <- tempfile("pages_")
  dir.create(out)
  pdf <- file.path(out, "report.pdf")
  export_report(context, project, pdf, "pdf", i18n = i18n, progress = function(x) if (is.function(progress)) progress(0.9 * x), converter = converter)
  n <- pdftools::pdf_info(pdf)$pages
  files <- file.path(out, sprintf("page_%03d.png", seq_len(n)))
  pdftools::pdf_convert(pdf, format = "png", dpi = dpi, filenames = files, verbose = FALSE)
  if (is.function(progress)) progress(1)
  list(pages = files, converter = converter, pdf = pdf)
}

# ---- what is drawn -------------------------------------------------------------------------------------------------

# Everything the writers need: the design, the cover, the fields and each block drawn (charts as PNG files, images as files)
.rb_prepare <- function(context, project, dir, i18n, step) {
  context <- as_report_context(context)
  design <- .rb_design(project$design)
  cover <- .rb_cover(project$cover)
  regions <- .rb_regions(context)
  # no region: a national report
  if (!(is.character(project$region) && length(project$region) == 1 && nzchar(project$region))) project$region <- NULL
  blocks <- lapply(project$blocks %||% list(), report_resolve_block, project = project, regions = regions)
  fields <- report_fields(context, project, lang = (if (is.list(i18n)) i18n$lang) %||% "en")
  blocks <- report_chart_fields(blocks, context, i18n)

  rendered <- vector("list", length(blocks))
  # charts as vector graphics (SVG, which Word draws sharp and keeps in the PDF it makes; officer adds a PNG copy for
  # older Word, made with rsvg), else as 300 dpi pictures
  vector <- requireNamespace("svglite", quietly = TRUE) && requireNamespace("rsvg", quietly = TRUE)
  todo <- which(vapply(blocks, function(b) isTRUE(b$type %in% c("chart", "table", "image", "canvas")), logical(1)))
  with_report_chart_options(context, design = design, {
    for (k in seq_along(todo)) {
      i <- todo[[k]]
      b <- blocks[[i]]
      if (identical(b$type, "canvas")) {
        # a free-layout page: each of its items drawn at its box (R/report-canvas.R)
        rendered[[i]] <- .rb_canvas_prepare(context, b, project, design, i18n, file.path(dir, paste0("canvas_", i)), vector)
      } else if (identical(b$type, "image")) {
        # a picture kept in the dataset (src "asset:<id>"): its file, embedded in the Word file like any other
        if (is.character(b$src) && startsWith(b$src, "asset:")) b$src_file <- .rb_asset_file(context, b$src, file.path(dir, paste0("asset_", i)))
        rendered[[i]] <- .rb_image_file(b, file.path(dir, paste0("image_", i)), report_block_size(b, design))
      } else {
        r <- render_report_block(context, b, i18n, design)
        if (identical(r$type, "plot") && .rb_chart_pictured(b)) {
          # a chart formatted as a picture (cropped, turned, recoloured, a shape): drawn at 300 dpi, then changed as a
          # picture is
          png <- file.path(dir, paste0("chart_", i, ".png"))
          save_report_chart(r, b, png, dpi = 300, design = design)
          size <- report_block_size(b, design)
          pic <- .rb_image_file(utils::modifyList(b, list(src_file = png, ratio = size[2] / size[1])), file.path(dir, paste0("chart_", i)), .rb_shown_size(b, design))
          r$file <- pic$file %||% png
        } else if (identical(r$type, "plot")) {
          r$file <- file.path(dir, paste0("chart_", i, if (vector) ".svg" else ".png"))
          save_report_chart(r, b, r$file, dpi = 300, design = design)
          if (vector) {
            r$png <- file.path(dir, paste0("chart_", i, ".png"))
            save_report_chart(r, b, r$png, dpi = 300, design = design)
          }
        }
        rendered[[i]] <- r
      }
      step(k / max(1, length(todo)))
    }
  })

  cover_files <- list(
    flag = if (isTRUE(cover$show_flag)) tryCatch(context$flag(), error = function(e) NULL),
    logos = Filter(Negate(is.null), lapply(seq_along(cover$logos), function(k) .rb_data_file(cover$logos[[k]]$src, file.path(dir, paste0("logo_", k))))),
    photo = if (identical(cover$layout, "photo")) .rb_data_file(cover$photo, file.path(dir, "photo"))
  )
  # each SVG chart's own PNG, found again by the SVG's content once officer has copied it into the Word file
  svgs <- Filter(function(r) !is.null(r$png), rendered)
  png_copies <- stats::setNames(vapply(svgs, function(r) r$png, character(1)),
                                unname(tools::md5sum(vapply(svgs, function(r) r$file, character(1)))))
  list(project = project, design = design, cover = cover, blocks = blocks, rendered = rendered, fields = fields,
       cover_files = cover_files, page = report_page(design), png_copies = png_copies)
}

.rb_regions <- function(context) {
  tryCatch(sort(unique(as_report_context(context)$regions())), error = function(e) character())
}

# A data URL written to a file; NULL when there is none
.rb_data_file <- function(src, path) {
  if (is.null(src) || !is.character(src) || !startsWith(src, "data:")) return(NULL)
  ext <- if (grepl("^data:image/jpe?g", src)) ".jpg" else if (grepl("^data:image/svg", src)) ".svg" else ".png"
  path <- paste0(path, ext)
  writeBin(jsonlite::base64_dec(sub("^data:[^,]*,", "", src)), path)
  path
}

# A picture kept with the report's data (the context's asset_get()) written to a file; NULL when there is none
.rb_asset_file <- function(context, src, path) {
  id <- sub("^asset:", "", src)
  asset <- tryCatch(as_report_context(context)$asset_get(id), error = function(e) NULL)
  if (is.null(asset) || !is.raw(asset$data)) return(NULL)
  ext <- switch(asset$type %||% "", "image/jpeg" = ".jpg", "image/svg+xml" = ".svg", "image/gif" = ".gif", ".png")
  path <- paste0(path, ext)
  writeBin(asset$data, path)
  path
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

.rb_image_dims <- function(path) {
  if (is.null(path) || !requireNamespace("magick", quietly = TRUE)) return(NULL)
  info <- tryCatch(magick::image_info(magick::image_read(path)), error = function(e) NULL)
  if (is.null(info)) NULL else c(info$width[[1]], info$height[[1]])
}

# An image block's picture with its formatting drawn into it, so every program shows it the same: turned and flipped,
# cropped, made at 300 dpi for the size it is shown (`size`, inches), its colours adjusted (the same arithmetic as the
# editor's CSS filters: brightness multiplies, contrast spreads around the middle grey), cut to its shape, given its
# border, then its picture style (pic_style: .rb_pic_style())
.rb_image_file <- function(b, path, size = NULL) {
  file <- if (!is.null(b$src_file) && file.exists(b$src_file)) b$src_file else .rb_data_file(b$src, path)
  if (is.null(file)) return(list(type = "error", message = "No picture has been chosen."))
  if (!requireNamespace("magick", quietly = TRUE)) return(list(type = "image", file = file))
  img <- tryCatch(magick::image_read(file)[1], error = function(e) NULL)
  if (is.null(img)) return(list(type = "image", file = file))
  shape <- b$shape %||% "rect"
  border <- isTRUE(b$border)
  rotate <- as.integer(b$rotate %||% 0) %% 360
  crop <- .rb_image_crop(b)
  bright <- 1 + max(-100, min(100, as.numeric(b$brightness %||% 0))) / 100
  contrast <- 1 + max(-90, min(100, as.numeric(b$contrast %||% 0))) / 100
  grey <- isTRUE(b$greyscale)
  changed <- FALSE

  if (rotate != 0) img <- magick::image_rotate(img, rotate)
  if (isTRUE(b$flip_h)) img <- magick::image_flop(img)
  if (isTRUE(b$flip_v)) img <- magick::image_flip(img)
  changed <- rotate != 0 || isTRUE(b$flip_h) || isTRUE(b$flip_v)
  info <- magick::image_info(img)
  w <- info$width[[1]]
  h <- info$height[[1]]
  if (any(crop > 0)) {
    cw <- max(1, round(w * (1 - crop[2] - crop[4])))
    ch <- max(1, round(h * (1 - crop[1] - crop[3])))
    img <- magick::image_repage(magick::image_crop(img, magick::geometry_area(cw, ch, round(w * crop[4]), round(h * crop[1]))))
    w <- cw
    h <- ch
    changed <- TRUE
  }
  # no more pixels than 300 dpi at the size it is shown: sharp in print, and a smaller file
  if (length(size) == 2 && all(is.finite(size))) {
    target <- ceiling(size[1] * 300)
    if (w > target * 1.05) {
      img <- magick::image_resize(img, magick::geometry_size_pixels(width = target))
      h <- round(h * target / w)
      w <- target
      changed <- TRUE
    }
  }
  if (grey) {
    img <- magick::image_convert(magick::image_convert(img, colorspace = "gray"), colorspace = "sRGB")
    changed <- TRUE
  }
  if (bright != 1 || contrast != 1) {
    # out = in * bright * contrast + (1 - contrast) / 2, which is a levels change from `black` to `white`
    a <- max(bright * contrast, 0.01)
    black <- -(0.5 * (1 - contrast)) / a
    img <- magick::image_level(img, black_point = black * 100, white_point = (black + 1 / a) * 100)
    changed <- TRUE
  }
  shown_w <- if (length(size) == 2) size[1] else w / 150
  border_px <- max(1, round(as.numeric(b$border_width %||% 2.25) / 72 * w / shown_w))
  border_col <- b$border_color %||% "#5c6670"
  if (shape != "rect" || border) {
    changed <- TRUE
    if (shape == "circle") {
      side <- min(w, h)
      img <- magick::image_crop(img, magick::geometry_area(side, side, (w - side) %/% 2, (h - side) %/% 2))
      w <- h <- side
    }
    if (shape != "rect") {
      # the shape in white on black: multiplying keeps the picture inside it, screening with the inverse whitens the rest
      draw_shape <- function(canvas, fill, stroke = NA, lwd = 1) {
        canvas <- magick::image_draw(canvas)
        if (shape == "circle") {
          graphics::symbols(w / 2, h / 2, circles = w / 2 - 1 - lwd * 0.375, inches = FALSE, add = TRUE, bg = fill, fg = stroke, lwd = lwd)
        } else {
          graphics::polygon(.rb_round_rect(w, h, min(w, h) * 0.06), col = fill, border = stroke, lwd = lwd)
        }
        grDevices::dev.off()
        canvas
      }
      mask <- magick::image_flatten(draw_shape(magick::image_blank(w, h, "black"), "white"))
      img <- magick::image_composite(img, mask, operator = "Multiply")
      img <- magick::image_composite(img, magick::image_negate(mask), operator = "Screen")
      # a line width of 1 is 0.75 pixel on this device
      if (border) img <- draw_shape(img, NA, border_col, lwd = border_px / 0.75)
    } else if (border) {
      # inside the picture's edge, so the picture keeps its size
      img <- magick::image_border(magick::image_crop(img, magick::geometry_area(max(1, w - 2 * border_px), max(1, h - 2 * border_px), border_px, border_px)),
                                  border_col, paste0(border_px, "x", border_px))
    }
  }
  style <- .rb_pic_style_of(b)
  if (nzchar(style)) {
    # a shape's outside made transparent, so the shadow, the soft edge and the reflection follow the shape
    shape_mask <- if (shape != "rect") magick::image_convert(mask, matte = FALSE)
    if (!is.null(shape_mask)) {
      # (on a transparent canvas first: a picture without an alpha channel would be cut to black)
      img <- magick::image_composite(magick::image_composite(magick::image_blank(w, h, "none"), img, operator = "over"),
                                     .rb_alpha_image(shape_mask), operator = "DstIn")
    }
    img <- .rb_pic_style(img, style, shape_mask)
    changed <- TRUE
  }
  if (changed) {
    file <- paste0(path, "_formatted.png")
    magick::image_write(img, file, format = "png")
  }
  list(type = "image", file = file)
}

# ---- picture styles ------------------------------------------------------------------------------------------------

# A picture's style (block field pic_style), as PowerPoint's picture styles: "shadow" (a soft shadow to the bottom
# right), "frame" (a thick white frame, a thin grey line and a shadow), "soft" (edges fading out), "reflection" (a faded
# reflection below the picture); "" for none
.rb_pic_styles <- c("shadow", "frame", "soft", "reflection")
.rb_pic_style_of <- function(b) {
  s <- b$pic_style
  if (is.character(s) && length(s) == 1 && !is.na(s) && s %in% .rb_pic_styles) s else ""
}

# A picture in one colour (white by default) whose opacity is `grey` (white opaque, black transparent): a shape, or
# what another picture is cut with ("DstIn")
.rb_alpha_image <- function(grey, colour = "white") {
  info <- magick::image_info(grey)
  magick::image_composite(magick::image_blank(info$width[[1]], info$height[[1]], colour), magick::image_convert(grey, matte = FALSE),
                          operator = "CopyOpacity")
}

# A picture style drawn into the picture, as the editor draws it inside the item's box: the result has the picture's
# size (the same shape, so it still fills its box), the picture made smaller inside it where the style needs room (the
# shadow and the frame around it; the reflection takes the bottom quarter, the picture the top three quarters, drawn
# the full width as the editor does). `mask`: the picture's shape (white on black) when it is not a rectangle.
.rb_pic_style <- function(img, style, mask = NULL) {
  info <- magick::image_info(img)
  w <- info$width[[1]]
  h <- info$height[[1]]
  m <- min(w, h)
  blank <- function(width = w, height = h) magick::image_blank(width, height, "none")
  over <- function(base, pic, x, y) magick::image_composite(base, pic, operator = "over", offset = sprintf("%+d%+d", round(x), round(y)))
  # the picture drawn to fill the room it is given, as the editor draws it (the whole is stretched to its box anyway)
  fit <- function(pic, aw, ah) magick::image_resize(pic, sprintf("%dx%d!", max(1, round(aw)), max(1, round(ah))))
  dims <- function(p) { i <- magick::image_info(p); c(i$width[[1]], i$height[[1]]) }

  if (style %in% c("shadow", "frame")) {
    off <- max(2, round(m * 0.022))
    sigma <- max(1, m * 0.016)
    lead <- ceiling(sigma * 1.5)
    trail <- off + ceiling(sigma * 2.5)
    aw <- w - lead - trail
    ah <- h - lead - trail
    if (style == "frame") {
      # a white band and a thin grey line around the picture, in its shape
      frame <- max(2, round(m * 0.04))
      line <- max(1, round(m * 0.004))
      pic <- fit(img, aw - 2 * (frame + line), ah - 2 * (frame + line))
      d <- dims(pic)
      shape <- function(width, height, colour) {
        cut <- if (is.null(mask)) magick::image_blank(width, height, "white") else magick::image_resize(mask, sprintf("%dx%d!", width, height))
        .rb_alpha_image(cut, colour)
      }
      outer <- d + 2 * (frame + line)
      framed <- shape(outer[1], outer[2], "#b9bec4")
      framed <- over(framed, shape(outer[1] - 2 * line, outer[2] - 2 * line, "white"), line, line)
      pic <- over(framed, pic, frame + line, frame + line)
    } else {
      pic <- fit(img, aw, ah)
    }
    d <- dims(pic)
    x <- lead + (aw - d[1]) / 2
    y <- lead + (ah - d[2]) / 2
    # the picture's outline in black, 45% opaque, moved and blurred
    shadow <- over(blank(), magick::image_colorize(pic, 100, "black"), x + off, y + off)
    shadow <- magick::image_blur(magick::image_fx(shadow, expression = "a*0.45", channel = "alpha"), radius = 0, sigma = sigma)
    return(over(shadow, pic, x, y))
  }
  if (style == "soft") {
    f <- max(2, round(m * 0.06))
    base <- if (is.null(mask)) magick::image_blank(w, h, "white") else mask
    base <- magick::image_resize(magick::image_convert(base, matte = FALSE), sprintf("%dx%d!", max(1, w - 2 * f), max(1, h - 2 * f)))
    base <- magick::image_border(base, "black", sprintf("%dx%d", f, f))
    base <- magick::image_blur(magick::image_convert(base, matte = FALSE), radius = 0, sigma = f / 2)
    return(magick::image_composite(over(blank(), img, 0, 0), .rb_alpha_image(base), operator = "DstIn"))
  }
  if (style == "reflection") {
    # the picture in the top three quarters, its reflection below it fading out
    top <- round(h * 0.75)
    gap <- max(1, round(h * 0.012))
    pic <- fit(img, w, top)
    d <- dims(pic)
    x <- (w - d[1]) / 2
    y <- top - d[2]
    rh <- max(1, min(d[2], h - top - gap))
    refl <- magick::image_crop(magick::image_flip(over(blank(d[1], d[2]), pic, 0, 0)), magick::geometry_area(d[1], rh, 0, 0))
    fade <- magick::image_fx(magick::image_blank(d[1], rh, pseudo_image = "gradient:white-black"), expression = "u*0.4")
    refl <- magick::image_composite(refl, .rb_alpha_image(fade), operator = "DstIn")
    return(over(over(blank(), pic, x, y), refl, x, top + gap))
  }
  img
}

# A picture with text wrapped around it: a one-cell table (the picture and its caption) that Word floats at the left or
# right of the text (the marker in its title is turned into the floating position by .rb_docx_float_tables())
.rb_float_table <- function(parts, b, r, size, side) {
  design <- parts$design
  chunks <- list(flextable::as_image(src = r$file, width = size[1], height = size[2]))
  if (nzchar(b$caption %||% "")) {
    chunks <- c(chunks, list(flextable::as_chunk(paste0("\n", .rb_fill(b$caption, parts$fields)),
                                                 props = officer::fp_text_lite(font.family = design$body_font, font.size = .rb_pt(design$caption_size), color = design$muted_color, italic = TRUE))))
  }
  ft <- flextable::flextable(data.frame(c1 = "", stringsAsFactors = FALSE))
  ft <- flextable::delete_part(ft, "header")
  ft <- flextable::compose(ft, i = 1, j = 1, value = do.call(flextable::as_paragraph, chunks))
  ft <- flextable::border_remove(ft)
  ft <- flextable::padding(ft, padding = 0, part = "all")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::width(ft, j = 1, width = size[1])
  # the side, and the space kept between the picture and the text (beside, above, below; twips) in the marker
  twips <- function(x, default) round(20 * (if (is.numeric(x) && length(x) == 1 && !is.na(x)) x else default))
  marker <- sprintf("cd-rb-float-%s-%d-%d-%d", side, twips(b$space_side, 8.5), twips(b$space_top, 0), twips(b$space_bottom, 5.65))
  flextable::set_table_properties(ft, layout = "fixed", word_title = marker, word_description = "")
}

# The marked tables become floating tables, which text wraps around (Word and LibreOffice both keep them so)
.rb_docx_float_tables <- function(file) {
  if (!requireNamespace("zip", quietly = TRUE)) return(invisible(FALSE))
  dir <- tempfile("docx_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(file, exdir = dir)
  doc_path <- file.path(dir, "word", "document.xml")
  body <- paste(readLines(doc_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl("cd-rb-float-", body, fixed = TRUE)) return(invisible(FALSE))
  props <- gregexpr("<w:tblPr>.*?</w:tblPr>", body)[[1]]
  starts <- as.integer(props)
  ends <- starts + attr(props, "match.length") - 1
  pieces <- character()
  last <- 1
  for (k in seq_along(starts)) {
    chunk <- substr(body, starts[k], ends[k])
    side <- regmatches(chunk, regexec('w:val="cd-rb-float-(left|right)(?:-([0-9]+)-([0-9]+)-([0-9]+))?"', chunk, perl = TRUE))[[1]]
    if (length(side) >= 2) {
      # the space between the picture and the text beside it (170 twips, 3 mm, unless set), above and below it
      num <- function(k, default) if (length(side) > k && nzchar(side[k + 1])) as.integer(side[k + 1]) else default
      gap <- num(2, 170L)
      pos <- sprintf(paste0('<w:tblpPr w:leftFromText="%d" w:rightFromText="%d" w:topFromText="%d" w:bottomFromText="%d" ',
                            'w:vertAnchor="text" w:horzAnchor="margin" w:tblpXSpec="%s" w:tblpY="1"/><w:tblOverlap w:val="never"/>'),
                     if (side[2] == "right") gap else 0L, if (side[2] == "left") gap else 0L, num(3, 0L), num(4, 113L), side[2])
      chunk <- gsub("<w:tblCaption [^>]*/>", "", chunk)
      chunk <- gsub("<w:tblDescription [^>]*/>", "", chunk)
      chunk <- gsub("<w:jc [^>]*/>", "", chunk)
      # tblpPr comes right after the table's style, as the schema orders it
      if (grepl("<w:tblStyle [^>]*/>", chunk)) chunk <- sub("(<w:tblStyle [^>]*/>)", paste0("\\1", pos), chunk)
      else chunk <- sub("<w:tblPr>", paste0("<w:tblPr>", pos), chunk, fixed = TRUE)
    }
    pieces <- c(pieces, substr(body, last, starts[k] - 1), chunk)
    last <- ends[k] + 1
  }
  body <- paste0(paste(pieces, collapse = ""), substr(body, last, nchar(body)))
  writeLines(body, doc_path, useBytes = TRUE)
  out <- tempfile(fileext = ".docx")
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- c(files[files == "[Content_Types].xml"], files[files != "[Content_Types].xml"])
  zip::zip(out, files = files, root = dir, mode = "mirror")
  file.copy(out, file, overwrite = TRUE)
  unlink(out)
  invisible(TRUE)
}

# How a full-width picture or chart sits in the text: "left" or "right" when the text wraps around it, else "inline"
.rb_wrap <- function(b) {
  wrap <- b$wrap %||% "inline"
  if (isTRUE(b$type %in% c("image", "chart")) && identical(b$size %||% "full", "full") && wrap %in% c("left", "right")) wrap else "inline"
}

.rb_round_rect <- function(w, h, r) {
  a <- seq(0, pi / 2, length.out = 12)
  list(
    x = c(w - r + r * cos(a), r - r * sin(a), r - r * cos(a), w - r + r * sin(a)),
    y = c(r - r * sin(a), r - r * cos(a), h - r + r * sin(a), h - r + r * cos(a))
  )
}

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

# Consecutive half-width charts and images share a row (two to a row), and so do third-width ones (three to a row)
.rb_rows <- function(blocks) {
  cell <- function(x) if (isTRUE(x$type %in% c("chart", "image")) && isTRUE(x$size %in% c("half", "third"))) x$size else ""
  rows <- list()
  i <- 1
  while (i <= length(blocks)) {
    size <- cell(blocks[[i]])
    n <- 1
    if (nzchar(size)) {
      most <- if (size == "third") 3 else 2
      while (n < most && i + n <= length(blocks) && identical(cell(blocks[[i + n]]), size)) n <- n + 1
    }
    rows[[length(rows) + 1]] <- seq(i, length.out = n)
    i <- i + n
  }
  rows
}

.rb_error_text <- function(i18n, message) paste0("[", .rb_t(i18n, "lbl_rb_not_drawn", "This chart could not be drawn with this dataset"), ": ", message, "]")

# ---- Word ----------------------------------------------------------------------------------------------------------

.rb_write_docx <- function(parts, file, i18n) {
  design <- parts$design
  page <- parts$page
  fields <- parts$fields
  blocks <- parts$blocks
  rendered <- parts$rendered
  # the design's Word template (its styles, headers, footers and page), else the package's
  own <- .rb_docx_template(design$template)
  if (!is.null(own)) on.exit(unlink(own$file), add = TRUE)
  template <- own$file %||% system.file("rmd", "report-template.docx", package = "datasuite.ui")
  doc <- officer::read_docx(template)
  doc <- .rb_docx_styles(doc, design)

  if (isTRUE(design$cover)) {
    doc <- flextable::body_add_flextable(doc, .rb_cover_table(parts), align = "left")
    doc <- officer::body_add_break(doc)
  }
  if (isTRUE(design$contents)) {
    doc <- officer::body_add_par(doc, .rb_t(i18n, "lbl_rb_contents", "Contents"), style = "TOC Heading")
    doc <- officer::body_add_toc(doc, level = 2)
    doc <- officer::body_add_break(doc)
  }

  # A page break is written as "page break before" on the paragraph that follows it: a break paragraph that happens to
  # fall at the end of a full page would leave an empty page. Tables cannot carry it, so before a table it is a break.
  new_page <- FALSE
  # nothing written yet on the current page (the first page, or the page after the cover or contents)
  page_top <- TRUE
  # a free-layout page's items, written as markers here and made into Word's floating objects once the file is written
  canvas <- new.env(parent = emptyenv())
  canvas$specs <- list()
  rows <- .rb_rows(blocks)
  for (k in seq_along(rows)) {
    row <- rows[[k]]
    b <- blocks[[row[1]]]
    # a short paragraph (a chart's title) right before a chart, table or picture stays on its page
    following <- if (k < length(rows)) blocks[[rows[[k + 1]][1]]]
    keep <- identical(b$type, "paragraph") && is.null(b$list) && nchar(.rb_plain(b$text)) <= 200 &&
      isTRUE(following$type %in% c("chart", "table", "image"))
    if (length(row) == 1 && identical(b$type, "pagebreak")) {
      new_page <- TRUE
      next
    }
    if (length(row) == 1 && identical(b$type, "canvas")) {
      # a page of its own: it starts a new page (unless nothing is on this one yet), and what follows starts another
      doc <- .rb_canvas_docx(doc, rendered[[row]], parts, i18n, canvas)
      if (!page_top) .rb_page_break_before(doc, 1)
      new_page <- TRUE
      page_top <- FALSE
      next
    }
    page_top <- FALSE
    paragraphs <- if (length(row) == 1) .rb_docx_paragraphs(b, rendered[[row]], parts) else 0
    if (new_page && paragraphs == 0) doc <- officer::body_add_break(doc)
    if (length(row) > 1) {
      doc <- flextable::body_add_flextable(doc, .rb_pair_table(parts, row, i18n), align = "left")
    } else {
      doc <- .rb_docx_block(doc, b, rendered[[row]], parts, i18n, keep_next = keep)
    }
    if (new_page && paragraphs > 0) .rb_page_break_before(doc, paragraphs)
    new_page <- FALSE
  }

  muted <- officer::fp_text_lite(font.family = design$body_font, font.size = 8, color = design$muted_color)
  tabs <- officer::fp_tabs(officer::fp_tab(pos = page$text_width, style = "right"))
  header_text <- .rb_plain(.rb_fill(design$header, fields))
  footer_text <- .rb_plain(.rb_fill(design$footer, fields))
  header <- if (nzchar(header_text)) officer::block_list(officer::fpar(officer::ftext(header_text, muted),
                                                                      fp_p = officer::fp_par(border.bottom = officer::fp_border(color = design$note_border, width = 0.75), padding.bottom = 3)))
  footer_runs <- list(officer::ftext(footer_text, muted))
  if (isTRUE(design$page_numbers)) {
    footer_runs <- c(footer_runs, list(officer::run_tab(), officer::run_word_field("PAGE", prop = muted), officer::ftext(" / ", muted),
                                       officer::run_word_field("NUMPAGES", prop = muted)))
  }
  footer <- if (nzchar(footer_text) || isTRUE(design$page_numbers)) {
    officer::block_list(do.call(officer::fpar, c(footer_runs, list(fp_p = officer::fp_par(tabs = tabs)))))
  }
  empty <- officer::block_list(officer::fpar(""))
  size <- officer::page_size(width = if (design$orientation == "landscape") page$height else page$width,
                             height = if (design$orientation == "landscape") page$width else page$height,
                             orient = design$orientation)
  margins <- officer::page_mar(top = page$top, bottom = page$bottom, left = page$left, right = page$right, header = 0.4, footer = 0.4)
  sect <- own$section
  if (!is.null(sect) && all(is.finite(c(sect$width, sect$height)))) {
    # a template's page and margins
    size <- officer::page_size(width = sect$width, height = sect$height, orient = if (isTRUE(sect$landscape)) "landscape" else "portrait")
    num <- function(v, d) if (is.finite(v)) v else d
    margins <- officer::page_mar(top = num(sect$top, page$top), bottom = num(sect$bottom, page$bottom), left = num(sect$left, page$left),
                                 right = num(sect$right, page$right), header = num(sect$header, 0.4), footer = num(sect$footer, 0.4))
  }
  # a template with its own header or footer keeps its section as it is
  if (!isTRUE(sect$header_footer)) {
    section <- do.call(officer::prop_section, Filter(Negate(is.null), list(
      page_size = size, page_margins = margins, header_default = header, footer_default = footer,
      # the cover page has no header or footer
      header_first = if (isTRUE(design$cover)) empty, footer_first = if (isTRUE(design$cover)) empty
    )))
    doc <- officer::body_set_default_section(doc, section)
  }
  print(doc, target = file)
  .rb_docx_png_copies(file, parts$png_copies)
  .rb_docx_float_tables(file)
  .rb_docx_numbering(file)
  .rb_docx_canvas(file, canvas$specs)
}

# How many paragraphs a block is written as (0: it is a table)
.rb_docx_paragraphs <- function(b, r, parts) {
  switch(
    b$type,
    heading = 1,
    paragraph = , note = , list = , quote = , pre = length(.rb_docx_fpars(b, parts$design, parts$fields)),
    chart = , table = , image = {
      if (is.null(r) || identical(r$type, "error")) 1
      else if (identical(r$type, "table")) 0
      else if (.rb_wrap(b) != "inline") 0
      else 1 + (identical(b$type, "image") && nzchar(b$caption %||% ""))
    },
    0
  )
}

# Give the first of the last `back` paragraphs written "page break before"
.rb_page_break_before <- function(doc, back) {
  node <- officer::docx_current_block_xml(doc)
  if (back > 1) for (k in seq_len(back - 1)) node <- xml2::xml_find_first(node, "preceding-sibling::*[1]")
  if (inherits(node, "xml_missing") || xml2::xml_name(node) != "p") return(invisible())
  ppr <- xml2::xml_find_first(node, "w:pPr")
  if (inherits(ppr, "xml_missing")) {
    xml2::xml_add_child(node, "w:pPr", .where = 0)
    ppr <- xml2::xml_find_first(node, "w:pPr")
  }
  # it goes after the style and the keep settings, as Word's schema orders them
  before <- xml2::xml_find_all(ppr, "w:pStyle|w:keepNext|w:keepLines")
  xml2::xml_add_child(ppr, "w:pageBreakBefore", .where = length(before))
  invisible()
}

# Each SVG image in a Word file should come with a PNG copy for programs that cannot draw SVG (older Word, some viewers).
# officer makes one by converting the SVG at screen resolution, and none for images in a table cell (charts side by side).
# Here every SVG gets the PNG drawn from the same ggplot at 300 dpi (`copies`: md5 of the SVG -> its PNG); an SVG with
# none (not a chart) is converted at `dpi`.
.rb_docx_png_copies <- function(file, copies = character(), dpi = 300) {
  if (!requireNamespace("zip", quietly = TRUE)) return(invisible(FALSE))
  dir <- tempfile("docx_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(file, exdir = dir)
  doc_path <- file.path(dir, "word", "document.xml")
  rels_path <- file.path(dir, "word", "_rels", "document.xml.rels")
  body <- paste(readLines(doc_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  rels <- paste(readLines(rels_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  target_of <- function(id) {
    m <- regmatches(rels, regexec(sprintf('<Relationship[^>]*Id="%s"[^>]*Target="([^"]+)"', id), rels))[[1]]
    if (length(m) < 2) {
      m <- regmatches(rels, regexec(sprintf('<Relationship[^>]*Target="([^"]+)"[^>]*Id="%s"', id), rels))[[1]]
    }
    if (length(m) < 2) NA_character_ else m[[2]]
  }
  # the PNG for one SVG in the package: the chart's own, else the SVG converted
  png_for <- function(svg_file, out, width_px) {
    own <- copies[unname(tools::md5sum(svg_file))]
    if (length(own) == 1 && !is.na(own) && file.exists(own)) return(file.copy(own, out, overwrite = TRUE))
    if (!requireNamespace("rsvg", quietly = TRUE)) return(FALSE)
    isTRUE(tryCatch({ rsvg::rsvg_png(svg_file, file = out, width = width_px); TRUE }, error = function(e) FALSE))
  }
  width_px <- function(chunk) {
    cx <- regmatches(chunk, regexec('<wp:extent cx="([0-9]+)"', chunk))[[1]]
    if (length(cx) < 2) 1800 else round(as.numeric(cx[[2]]) / 914400 * dpi)
  }
  changed <- FALSE
  next_id <- max(c(0, as.numeric(regmatches(rels, gregexpr('(?<=Id="rId)[0-9]+', rels, perl = TRUE))[[1]]))) + 1

  # every drawing, one at a time (its size is in the same drawing as its picture)
  drawings <- gregexpr("<w:drawing>.*?</w:drawing>", body)[[1]]
  if (drawings[1] > 0) {
    starts <- as.integer(drawings)
    ends <- starts + attr(drawings, "match.length") - 1
    pieces <- character()
    last <- 1
    for (k in seq_along(starts)) {
      chunk <- substr(body, starts[k], ends[k])
      pieces <- c(pieces, substr(body, last, starts[k] - 1))
      last <- ends[k] + 1
      w <- width_px(chunk)
      paired <- regmatches(chunk, regexec('<a:blip r:embed="([^"]+)">.*?svgBlip[^>]*r:embed="([^"]+)"', chunk))[[1]]
      single <- regmatches(chunk, regexec('<a:blip r:embed="([^"]+)"/>', chunk))[[1]]
      if (length(paired) == 3) {
        # officer's copy: replaced by the chart's own
        png <- target_of(paired[[2]])
        svg <- target_of(paired[[3]])
        if (!is.na(png) && !is.na(svg)) {
          if (png_for(file.path(dir, "word", svg), file.path(dir, "word", png), w)) changed <- TRUE
        }
      } else if (length(single) == 2) {
        svg <- target_of(single[[2]])
        if (!is.na(svg) && grepl("\\.svg$", svg)) {
          # no copy at all (an image in a table cell): the PNG is added and the picture points to both
          png <- sub("\\.svg$", ".png", svg)
          if (png_for(file.path(dir, "word", svg), file.path(dir, "word", png), w)) {
            id <- paste0("rId", next_id)
            next_id <- next_id + 1
            rels <- sub("</Relationships>", sprintf('<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="%s"/></Relationships>', id, png), rels, fixed = TRUE)
            chunk <- sub(single[[1]], sprintf(paste0('<a:blip r:embed="%s"><a:extLst><a:ext uri="{96DAC541-7B7A-43D3-8B79-37D633B846F1}">',
                                                     '<asvg:svgBlip xmlns:asvg="http://schemas.microsoft.com/office/drawing/2016/SVG/main" r:embed="%s"/>',
                                                     '</a:ext></a:extLst></a:blip>'), id, single[[2]]), chunk, fixed = TRUE)
            changed <- TRUE
          }
        }
      }
      pieces <- c(pieces, chunk)
    }
    body <- paste0(paste(pieces, collapse = ""), substr(body, last, nchar(body)))
  }
  if (!changed) return(invisible(FALSE))
  writeLines(body, doc_path, useBytes = TRUE)
  writeLines(rels, rels_path, useBytes = TRUE)
  types_path <- file.path(dir, "[Content_Types].xml")
  types <- paste(readLines(types_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl('Extension="png"', types, fixed = TRUE)) {
    types <- sub("<Default ", '<Default Extension="png" ContentType="image/png"/><Default ', types, fixed = TRUE)
    writeLines(types, types_path, useBytes = TRUE)
  }
  out <- tempfile(fileext = ".docx")
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  # [Content_Types].xml first, as Word writes it
  files <- c(files[files == "[Content_Types].xml"], files[files != "[Content_Types].xml"])
  zip::zip(out, files = files, root = dir, mode = "mirror")
  file.copy(out, file, overwrite = TRUE)
  unlink(out)
  invisible(TRUE)
}

# A heading with its formatting (bold, colour, a field...) in its Word style (Heading 1 to 6, which the contents page is
# built from); the paragraph settings repeat the style's, as Word replaces them with the ones given
.rb_heading_fpar <- function(b, design, fields) {
  level <- min(6L, max(1L, as.integer(b$level %||% 1)))
  lines <- .rb_lines(.rb_fill(b$text, fields, escape = grepl("<", b$text %||% "", fixed = TRUE)))
  color <- if (level == 1) design$heading_color else if (level == 6) design$muted_color else design$text_color
  runs <- list()
  for (i in seq_along(lines)) {
    if (i > 1) runs <- c(runs, list(officer::run_linebreak()))
    runs <- c(runs, lapply(lines[[i]], .rb_ftext, text_color = color))
  }
  if (!length(runs)) runs <- list(officer::ftext(""))
  space <- list(c(14, 6), c(10, 4), c(8, 3), c(6, 2), c(6, 2), c(6, 2))[[level]]
  align <- b$align %||% "left"
  if (!align %in% c("left", "center", "right", "justify")) align <- "left"
  par <- officer::fp_par(text.align = align, padding.top = space[1], padding.bottom = space[2], line_spacing = 1, keep_with_next = TRUE,
                         border.bottom = if (level == 1) officer::fp_border(color = design$accent, width = 1.5) else officer::fp_border(width = 0),
                         word_style = paste("heading", level))
  do.call(officer::fpar, c(runs, list(fp_p = par)))
}

.rb_docx_block <- function(doc, b, r, parts, i18n, keep_next = FALSE) {
  design <- parts$design
  switch(
    b$type,
    heading = officer::body_add_fpar(doc, .rb_heading_fpar(b, design, parts$fields)),
    paragraph = , note = , list = , quote = , pre = {
      for (p in .rb_docx_fpars(b, design, parts$fields, keep_next)) doc <- officer::body_add_fpar(doc, p)
      doc
    },
    pagebreak = officer::body_add_break(doc),
    chart = , table = , image = {
      if (is.null(r) || identical(r$type, "error")) {
        officer::body_add_par(doc, .rb_error_text(i18n, r$message %||% ""), style = "Normal")
      } else if (identical(r$type, "table")) {
        flextable::body_add_flextable(doc, r$value, align = "center")
      } else if (.rb_wrap(b) != "inline") {
        flextable::body_add_flextable(doc, .rb_float_table(parts, b, r, .rb_shown_size(b, design), .rb_wrap(b)), align = .rb_wrap(b))
      } else {
        size <- .rb_shown_size(b, design)
        align <- b$align %||% "center"
        pt <- function(x, default) if (is.numeric(x) && length(x) == 1 && !is.na(x)) x else default
        doc <- officer::body_add_fpar(doc, officer::fpar(officer::external_img(r$file, width = size[1], height = size[2]),
                                                         fp_p = officer::fp_par(text.align = align, padding.top = pt(b$space_top, 0), padding.bottom = pt(b$space_bottom, 4),
                                                                                keep_with_next = identical(b$type, "image") && is.character(b$caption) && nzchar(b$caption))))
        if (identical(b$type, "image") && nzchar(b$caption %||% "")) doc <- officer::body_add_par(doc, .rb_fill(b$caption, parts$fields), style = "caption")
        doc
      }
    },
    doc
  )
}

# Half- or third-width blocks side by side: an invisible table of two or three columns with fixed widths, so Word keeps
# them on one row
.rb_pair_table <- function(parts, row, i18n) {
  design <- parts$design
  full <- parts$page$text_width - 0.27
  n <- if (identical(parts$blocks[[row[1]]]$size, "third")) 3 else 2
  col <- full / n
  cell <- function(i) {
    b <- parts$blocks[[i]]
    r <- parts$rendered[[i]]
    if (is.null(r) || identical(r$type, "error") || is.null(r$file)) {
      return(flextable::as_paragraph(flextable::as_chunk(.rb_error_text(i18n, r$message %||% ""), props = officer::fp_text_lite(font.size = 8, color = "#9b2c2c"))))
    }
    size <- .rb_shown_size(b, design)
    chunks <- list(flextable::as_image(src = r$file, width = size[1], height = size[2]))
    if (identical(b$type, "image") && nzchar(b$caption %||% "")) {
      chunks <- c(chunks, list(flextable::as_chunk(paste0("\n", .rb_fill(b$caption, parts$fields)),
                                                   props = officer::fp_text_lite(font.family = design$body_font, font.size = .rb_pt(design$caption_size), color = design$muted_color, italic = TRUE))))
    }
    do.call(flextable::as_paragraph, chunks)
  }
  ft <- flextable::flextable(as.data.frame(stats::setNames(as.list(rep("", length(row))), paste0("c", seq_along(row))), stringsAsFactors = FALSE))
  ft <- flextable::delete_part(ft, "header")
  for (k in seq_along(row)) ft <- flextable::compose(ft, i = 1, j = k, value = cell(row[k]))
  ft <- flextable::border_remove(ft)
  ft <- flextable::padding(ft, padding = 0, part = "all")
  ft <- flextable::padding(ft, padding.bottom = 6, part = "body")
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::width(ft, j = seq_along(row), width = col)
  flextable::set_table_properties(ft, layout = "fixed")
}

# The cover page: a one-row table the height of the page (a narrow coloured column beside the text for "band", a
# coloured cell for "full", the photo above the text for "photo", plain for "minimal")
.rb_cover_table <- function(parts) {
  design <- parts$design
  cover <- parts$cover
  fields <- parts$fields
  files <- parts$cover_files
  page <- parts$page
  layout <- cover$layout %||% "band"
  on_colour <- identical(layout, "full")
  ink <- if (on_colour) "#ffffff" else design$text_color
  soft <- if (on_colour) "#f1e9e9" else design$muted_color
  kicker_col <- if (on_colour) "#ffffff" else design$accent
  txt <- function(size, color, bold = FALSE, font = design$body_font) {
    officer::fp_text_lite(font.family = font, font.size = .rb_pt(size), color = color, bold = bold)
  }
  chunk <- function(text, props) flextable::as_chunk(text, props = props)
  title <- .rb_plain(.rb_fill(if (nzchar(cover$title %||% "")) cover$title else parts$project$name %||% "Report", fields))
  subtitle <- .rb_plain(.rb_fill(cover$subtitle %||% "", fields))
  kicker <- .rb_plain(.rb_fill(cover$kicker %||% "", fields))
  text_w <- page$text_width - (if (layout == "band") 0.55 else 0.3)

  chunks <- list()
  add <- function(...) chunks <<- c(chunks, list(...))
  if (layout == "photo" && !is.null(files$photo)) {
    dims <- .rb_image_dims(files$photo) %||% c(3, 2)
    h <- min(text_w * dims[2] / dims[1], page$text_height * 0.45)
    add(flextable::as_image(src = files$photo, width = h * dims[1] / dims[2], height = h), chunk("\n\n", txt(10, ink)))
  }
  marks <- c(if (!is.null(files$flag)) files$flag, unlist(files$logos))
  for (k in seq_along(marks)) {
    dims <- .rb_image_dims(marks[[k]]) %||% c(3, 2)
    h <- 0.55
    add(flextable::as_image(src = marks[[k]], width = h * dims[1] / dims[2], height = h), chunk("   ", txt(10, ink)))
  }
  if (length(marks)) add(chunk("\n\n", txt(10, ink)))
  if (nzchar(kicker)) add(chunk(paste0(toupper(kicker), "\n"), txt(10, kicker_col, bold = TRUE)))
  add(chunk(paste0(title, "\n"), txt(design$title_size, ink, bold = TRUE, font = design$heading_font)))
  if (nzchar(subtitle)) add(chunk(paste0(subtitle, "\n"), txt(13, soft)))
  editors <- Filter(function(e) nzchar(e$name %||% ""), cover$editors %||% list())
  if (length(editors)) {
    add(chunk("\n", txt(10, ink)))
    for (e in editors) {
      add(chunk(e$name, txt(10.5, ink, bold = TRUE)))
      add(chunk(if (nzchar(e$role %||% "")) paste0(" \u00b7 ", e$role, "\n") else "\n", txt(10, soft)))
    }
  }
  add(chunk(paste0("\n", fields$report_date %||% ""), txt(10, soft)))
  if (nzchar(cover$reference %||% "")) add(chunk(paste0("\n", .rb_fill(cover$reference, fields)), txt(9, soft)))
  content <- do.call(flextable::as_paragraph, chunks)

  height <- page$text_height - 0.2
  if (layout == "band") {
    ft <- flextable::flextable(data.frame(a = "", b = "", stringsAsFactors = FALSE))
    ft <- flextable::delete_part(ft, "header")
    ft <- flextable::compose(ft, i = 1, j = 2, value = content)
    ft <- flextable::bg(ft, j = 1, bg = design$accent, part = "body")
    ft <- flextable::width(ft, j = 1, width = 0.22)
    ft <- flextable::width(ft, j = 2, width = page$text_width - 0.22)
    ft <- flextable::padding(ft, j = 2, padding.left = 22, padding.bottom = 30, part = "body")
  } else {
    ft <- flextable::flextable(data.frame(a = "", stringsAsFactors = FALSE))
    ft <- flextable::delete_part(ft, "header")
    ft <- flextable::compose(ft, i = 1, j = 1, value = content)
    ft <- flextable::width(ft, j = 1, width = page$text_width)
    if (on_colour) ft <- flextable::bg(ft, bg = design$accent, part = "body")
    ft <- flextable::padding(ft, padding.left = 24, padding.right = 24, padding.bottom = 30, padding.top = 24, part = "body")
  }
  ft <- flextable::border_remove(ft)
  if (layout == "minimal") ft <- flextable::hline_top(ft, border = officer::fp_border(color = design$accent, width = 3), part = "body")
  ft <- flextable::valign(ft, valign = if (layout == "photo") "top" else "bottom", part = "body")
  ft <- flextable::align(ft, align = "left", part = "body")
  ft <- flextable::height(ft, height = height, part = "body")
  ft <- flextable::hrule(ft, rule = "exact", part = "body")
  ft <- flextable::line_spacing(ft, space = 1.1, part = "body")
  flextable::set_table_properties(ft, layout = "fixed")
}

# ---- PDF without Word or LibreOffice -------------------------------------------------------------------------------

.rb_esc <- function(x) htmltools::htmlEscape(x %||% "")

.rb_write_pdf <- function(parts, file, dir, i18n) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    .ds_abort(c("x" = "PDF export needs Microsoft Word, LibreOffice, or the {.pkg chromote} package with Chrome.", "i" = "Download as Word instead."))
  }
  .rb_find_browser()
  design <- parts$design
  html <- .rb_html(parts, i18n)
  page <- file.path(dir, "report.html")
  writeLines(html, page, useBytes = TRUE)

  b <- chromote::ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file:///", normalizePath(page, winslash = "/")), wait_ = FALSE)
  b$wait_for(loaded)
  style <- paste0("width:100%;font-size:8px;color:", design$muted_color, ";padding:0 ", parts$page$left, "in;font-family:Arial;display:flex")
  footer_text <- .rb_esc(.rb_plain(.rb_fill(design$footer, parts$fields)))
  header_text <- .rb_esc(.rb_plain(.rb_fill(design$header, parts$fields)))
  numbers <- if (isTRUE(design$page_numbers)) '<span class="pageNumber"></span> / <span class="totalPages"></span>' else ""
  pdf <- b$Page$printToPDF(
    printBackground = TRUE, preferCSSPageSize = TRUE, displayHeaderFooter = TRUE,
    headerTemplate = paste0('<div style="', style, '"><span>', header_text, "</span></div>"),
    footerTemplate = paste0('<div style="', style, '"><span style="flex:1">', footer_text, "</span>", numbers, "</div>")
  )
  writeBin(jsonlite::base64_dec(pdf$data), file)
}

# chromote looks for Google Chrome; on a machine without it, use another Chromium browser (Edge is on every Windows machine)
.rb_find_browser <- function() {
  found <- tryCatch(nzchar(chromote::find_chrome() %||% ""), error = function(e) FALSE)
  if (isTRUE(found)) return(invisible(TRUE))
  candidates <- c(
    file.path(Sys.getenv("PROGRAMFILES"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"),
    file.path(Sys.getenv("PROGRAMFILES(X86)"), "Microsoft/Edge/Application/msedge.exe"),
    file.path(Sys.getenv("PROGRAMFILES"), "Microsoft/Edge/Application/msedge.exe"),
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    Sys.which(c("chromium", "chromium-browser", "google-chrome", "microsoft-edge"))
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(hit)) .ds_abort(c("x" = "PDF export needs Microsoft Word, LibreOffice, Google Chrome, Microsoft Edge or Chromium.", "i" = "Download as Word instead."))
  Sys.setenv(CHROMOTE_CHROME = hit[[1]])
  invisible(TRUE)
}

.rb_data_uri <- function(path) {
  type <- if (grepl("\\.jpe?g$", path)) "image/jpeg" else if (grepl("\\.svg$", path)) "image/svg+xml" else "image/png"
  paste0("data:", type, ";base64,", jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size)))
}

# The HTML a PDF is printed from when there is no Word or LibreOffice: the same layout, drawn by the browser
.rb_html <- function(parts, i18n) {
  design <- parts$design
  cover <- parts$cover
  fields <- parts$fields
  blocks <- parts$blocks
  rendered <- parts$rendered
  pg <- parts$page
  font <- function(f) paste0("'", f, "', Arial, sans-serif")
  figure <- function(i) {
    r <- rendered[[i]]
    b <- blocks[[i]]
    if (is.null(r) || identical(r$type, "error")) return(paste0('<p class="err">', .rb_esc(.rb_error_text(i18n, r$message %||% "")), "</p>"))
    if (identical(r$type, "table")) return(paste0('<div class="tbl">', as.character(flextable::htmltools_value(r$value)), "</div>"))
    size <- .rb_shown_size(b, design)
    align <- b$align %||% "center"
    cap <- if (identical(b$type, "image") && nzchar(b$caption %||% "")) paste0('<figcaption>', .rb_esc(.rb_fill(b$caption, fields)), "</figcaption>") else ""
    float <- switch(.rb_wrap(b), left = "float:left;margin:0 0.12in 0.08in 0;", right = "float:right;margin:0 0 0.08in 0.12in;", "")
    paste0('<figure style="', float, 'text-align:', align, '"><img src="', .rb_data_uri(r$file), '" style="width:', size[1], "in;height:", size[2],
           'in" alt="', .rb_esc(b$alt %||% b$title %||% b$kind %||% ""), '">', cap, "</figure>")
  }
  body <- character()
  if (isTRUE(design$cover)) {
    files <- parts$cover_files
    marks <- c(if (!is.null(files$flag)) files$flag, unlist(files$logos))
    marks_html <- paste0(vapply(marks, function(m) paste0('<img class="mark" src="', .rb_data_uri(m), '">'), character(1)), collapse = "")
    editors <- Filter(function(e) nzchar(e$name %||% ""), cover$editors %||% list())
    editors_html <- paste0(vapply(editors, function(e) paste0("<div><b>", .rb_esc(e$name), "</b>", if (nzchar(e$role %||% "")) paste0(" \u00b7 ", .rb_esc(e$role)) else "", "</div>"), character(1)), collapse = "")
    title <- .rb_fill(if (nzchar(cover$title %||% "")) cover$title else parts$project$name %||% "Report", fields)
    body <- c(body, paste0(
      '<section class="cover cover--', .rb_esc(cover$layout), '">',
      if (identical(cover$layout, "photo") && !is.null(files$photo)) paste0('<img class="photo" src="', .rb_data_uri(files$photo), '">') else "",
      '<div class="cover__text">',
      if (length(marks)) paste0('<div class="marks">', marks_html, "</div>") else "",
      '<div class="kicker">', .rb_esc(toupper(.rb_plain(.rb_fill(cover$kicker, fields)))), "</div>",
      '<h1 class="title">', .rb_esc(.rb_plain(title)), "</h1>",
      if (nzchar(cover$subtitle %||% "")) paste0('<p class="sub">', .rb_esc(.rb_plain(.rb_fill(cover$subtitle, fields))), "</p>") else "",
      if (length(editors)) paste0('<div class="editors">', editors_html, "</div>") else "",
      '<p class="date">', .rb_esc(fields$report_date %||% ""), "</p>",
      if (nzchar(cover$reference %||% "")) paste0('<p class="date">', .rb_esc(.rb_fill(cover$reference, fields)), "</p>") else "",
      "</div></section>"
    ))
  }
  if (isTRUE(design$contents)) {
    items <- vapply(Filter(function(b) identical(b$type, "heading"), blocks), function(b) {
      paste0('<li class="l', b$level %||% 1, '">', .rb_esc(.rb_plain(.rb_fill(b$text, fields))), "</li>")
    }, character(1))
    body <- c(body, paste0('<section class="toc"><h1>', .rb_esc(.rb_t(i18n, "lbl_rb_contents", "Contents")), "</h1><ul>", paste(items, collapse = ""), "</ul></section>"))
  }
  for (row in .rb_rows(blocks)) {
    if (length(row) > 1) {
      n <- if (identical(blocks[[row[1]]]$size, "third")) 3 else 2
      cells <- paste0('<div style="flex:0 0 calc((100% - ', (n - 1) * 0.2, 'in) / ', n, ')">', vapply(row, figure, character(1)), "</div>", collapse = "")
      body <- c(body, paste0('<div class="pair">', cells, "</div>"))
      next
    }
    b <- blocks[[row]]
    h <- paste0("h", min(6L, max(1L, as.integer(b$level %||% 1))))
    body <- c(body, switch(
      b$type,
      heading = paste0("<", h, ">", .rb_esc(.rb_plain(.rb_fill(b$text, fields))), "</", h, ">"),
      paragraph = , note = , list = , quote = , pre = .rb_html_text(b, fields),
      pagebreak = '<div class="break"></div>',
      chart = , table = , image = figure(row),
      canvas = .rb_canvas_html(rendered[[row]], parts, i18n),
      ""
    ))
  }
  css <- paste0(
    "@page{size:", pg$width, "in ", pg$height, "in;margin:", pg$top, "in ", pg$right, "in ", pg$bottom, "in ", pg$left, "in}",
    "body{margin:0;font-family:", font(design$body_font), ";font-size:", design$body_size, "pt;line-height:1.45;color:", design$text_color, "}",
    "h1,h2,.title{font-family:", font(design$heading_font), ";font-weight:700}",
    "h1{font-size:", design$h1_size, "pt;color:", design$heading_color, ";border-bottom:1.5px solid ", design$accent, ";padding-bottom:4px;margin:18px 0 8px;break-after:avoid}",
    "h2{font-size:", design$h2_size, "pt;margin:12px 0 5px;break-after:avoid}",
    ".p{margin:0 0 6pt}.p ul,.p ol,.note ul,.note ol{margin:0;padding-left:18pt}",
    "figure{margin:6px 0;break-inside:avoid}figcaption{font-size:", design$caption_size, "pt;color:", design$muted_color, ";font-style:italic;text-align:center}",
    ".pair{display:flex;gap:0.2in;break-inside:avoid}",
    ".note{padding:6pt;border:1px solid ", design$note_border, ";background:", design$note_fill, ";font-size:", design$note_size, "pt;break-inside:avoid;margin:0 0 6pt}",
    ".quote{border-left:2pt solid ", design$accent, ";padding-left:10pt;font-style:italic;color:", design$muted_color, ";margin:0 0 6pt}",
    "pre.pre{font-family:Consolas,monospace;font-size:", design$body_size - 1, "pt;background:#f4f5f6;border:1px solid #e3e6e9;padding:6pt;white-space:pre-wrap;margin:0 0 6pt}",
    "h3{font-size:", .rb_heading_size(3, design), "pt}h4,h5,h6{font-size:", design$body_size, "pt}h5{font-style:italic}h6{color:", design$muted_color, "}",
    ".err{color:#9b2c2c;font-size:9pt}.tbl{margin:10px 0;break-inside:avoid}.break{break-after:page}",
    ".canvas{position:relative;break-before:page;break-after:page;break-inside:avoid}.canvas>.it{position:absolute;box-sizing:border-box;display:flex;flex-direction:column}",
    ".canvas>.it>.ln{margin:0}.canvas>.it img{display:block;width:100%;height:100%}.canvas .tbl{margin:0}",
    ".cover{position:relative;height:", pg$text_height - 0.2, "in;display:flex;flex-direction:column;justify-content:flex-end;break-after:page;box-sizing:border-box}",
    ".cover--band{border-left:0.22in solid ", design$accent, ";padding-left:0.3in}",
    ".cover--full{background:", design$accent, ";color:#fff;padding:0.35in}.cover--full .kicker,.cover--full .sub,.cover--full .date{color:#fff}",
    ".cover--minimal{border-top:3pt solid ", design$accent, ";padding:0.3in}",
    ".cover--photo{justify-content:flex-start;padding:0.3in}.cover .photo{max-width:100%;max-height:45%;object-fit:cover;margin-bottom:0.3in}",
    ".marks{margin-bottom:0.3in}.marks .mark{height:0.55in;margin-right:0.2in}",
    ".cover .kicker{font-size:10pt;font-weight:700;letter-spacing:.06em;color:", design$accent, "}",
    ".cover .title{font-size:", design$title_size, "pt;line-height:1.1;margin:6px 0 10px;border:0;color:inherit}",
    ".cover .sub{font-size:13pt;color:", design$muted_color, ";margin:0 0 12pt}.cover .editors{margin:0 0 10pt;font-size:10pt}.cover .date{font-size:10pt;color:", design$muted_color, ";margin:0}",
    ".toc{break-after:page}.toc ul{list-style:none;padding:0}.toc li{padding:3px 0;border-bottom:1px dotted #c9ced3}.toc li.l2{padding-left:16px;font-size:.92em}"
  )
  paste0('<!doctype html><html><head><meta charset="utf-8"><title>', .rb_esc(parts$project$name), "</title>",
         "<style>", css, "</style></head><body>", paste(body, collapse = "\n"), "</body></html>")
}
