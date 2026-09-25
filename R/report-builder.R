# The report builder: a report is a list of blocks (headings, text, notes, page breaks, charts and tables) plus a design.
# Charts and tables are described by what to draw (kind, indicator, level, region, year), never stored as pictures, so a
# report is drawn from the loaded data every time it is exported. The app's builder edits the block list; export_report()
# writes it as Word or PDF. Where reports are kept, and what the charts are drawn from, is the app's: its report
# context (R/report-context.R).
#
# Block fields:
#   id, type = "heading" | "paragraph" | "note" | "pagebreak" | "chart" | "table" | "image"
#   heading: text, level (1 or 2)
#   paragraph / note: text (formatted, see R/report-text.R), align = "left" | "center" | "right" | "justify",
#                     list = "bullet" | "number" (one item per line)
#   chart / table: kind (see report_block_kinds()), indicator, admin_level, region, year, variant,
#                  size = "full" | "half" | "third", title (overrides the drawn title), caption (FALSE hides the caption),
#                  options (chart options for this chart only, as the chart customize panel gives them)
#   image: src (a data URL), ratio (height / width), size = "full" | "half" | "third", width (percent of its column),
#          align, shape = "rect" | "rounded" | "circle", border (TRUE / FALSE), caption, alt
#   canvas: a free-layout page (see R/report-canvas.R): h (height of its area, inches), items placed like a slide's
#
# Consecutive half-width (or third-width) charts and images share a row. The design (fonts, colours, page) is R/report-theme.R; writing
# the files is R/report-export.R.

#' A translated string
#'
#' @param i18n An object with a `t(key)` method (an app's translator), or `NULL`.
#' @param key The key.
#' @param fallback What to give when there is no translation (or no `i18n`).
#' @return A string.
#' @export
report_translate <- function(i18n, key, fallback = key) {
  if (is.null(i18n)) return(fallback)
  value <- tryCatch(i18n$t(key), error = function(e) key)
  if (is.null(value) || identical(value, key) || !nzchar(value)) fallback else value
}


.rb_t <- report_translate

# ---- drawing one block ---------------------------------------------------------------------------------------------

#' Draw one chart or table of a report
#'
#' @param context A report context ([report_context()]), or what [as_report_context()] turns into one.
#' @param block A chart or table block (see the top of this file for its fields). A `region` of `"@report"` is the
#'   report's region: the caller puts it in first ([report_resolve_block()]).
#' @param i18n An object with a `t(key)` method for translated labels, or `NULL` for English.
#' @param design The report's design: its palette colours the chart's series (see [report_themes()]). `NULL` keeps the
#'   chart's own colours.
#'
#' @return `list(type = "plot", value = <ggplot>)`, `list(type = "table", value = <flextable>)`, or
#'   `list(type = "error", message = <text>)` when it cannot be drawn with this dataset.
#' @export
render_report_block <- function(context, block, i18n = NULL, design = NULL) {
  context <- as_report_context(context)
  tryCatch(
    {
      value <- context$draw(block, i18n)
      if (inherits(value, "ggplot")) {
        # a title typed for this chart ({chart_indicator} and {chart_year} are this chart's)
        if (!is.null(block$title) && nzchar(block$title)) value <- value + ggplot2::labs(title = report_chart_fields(list(block), context, i18n)[[1]]$title)
        if (isFALSE(block$caption)) value <- value + ggplot2::labs(caption = NULL)
        # the theme's palette, then what was set for this chart in the builder
        palette <- if (!is.null(design)) .rb_palette_options(value, .rb_design(design))
        own <- .rb_block_options(block$options)
        # a chart on a slide: its text at a size read on a slide (.rb_slide_text_options()), under what was set for it
        box <- .rb_box(block)
        slide <- if (!is.null(box)) .rb_slide_text_options(value, box)
        if (length(palette) || length(slide) || length(own)) value <- apply_chart_options(value, merge_chart_options(palette, slide, own))
        # a ggplot is built only when it is printed: build it now, so a chart that cannot be drawn says so here
        ggplot2::ggplot_build(value)
        list(type = "plot", value = value)
      } else if (inherits(value, "flextable")) {
        list(type = "table", value = value)
      } else {
        list(type = "error", message = "This block did not produce a chart or table.")
      }
    },
    error = function(e) list(type = "error", message = conditionMessage(e))
  )
}

# The text of a chart on a slide, as PowerPoint sizes a chart's text: 12 pt on a wide chart (8 in and wider), down to 9 pt
# on a narrow one (3 in) and never smaller (a chart is drawn at its box's size on a slide, so these are the sizes on the slide); the title
# 4 pt larger, a source note 1 pt smaller (at least 9 pt), text drawn on the chart 2 pt smaller (at least 8 pt). So that
# it fits the box: the title, subtitle and source note wrap at the box's width, a legend too wide for it is laid out in
# columns (rows of entries), and category labels that do not fit side by side are turned upright. What the user set
# for the chart replaces these (merged after them).
.rb_slide_text_options <- function(p, box) {
  s <- round(min(12, max(9, 9 + (box[1] - 3) * 3 / 5)), 1)
  opts <- list(title_size = s + 4, subtitle_size = s, caption_size = max(9, s - 1), x_title_size = s, y_title_size = s,
               x_text_size = s, y_text_size = s, legend_title_size = s, legend_text_size = s, strip_text_size = s,
               label_size = max(8, s - 2), title_wrap = max(15, floor(box[1] * 72 / ((s + 4) * 0.52))),
               legend_key_size = round(s * 0.45, 1))
  built <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
  if (is.null(built)) return(do.call(cd_chart_options, opts))
  # text of `n` characters at this size, in inches
  inches <- function(n) n * s * 0.55 / 72
  # the legend: its entries in one row, else in as many columns as fit
  entries <- unlist(lapply(c("fill", "colour", "shape", "linetype"), function(a) {
    sc <- built$plot$scales$get_scales(a)
    if (is.null(sc) || identical(sc$guide, "none")) return(NULL)
    tryCatch(as.character(unlist(sc$get_labels())), error = function(e) NULL)
  }))
  entries <- unique(entries[!is.na(entries) & nzchar(entries)])
  if (length(entries) > 1) {
    each <- inches(nchar(entries)) + 0.35
    if (sum(each) > 0.95 * box[1]) opts$legend_ncol <- max(1, floor(0.95 * box[1] / max(each)))
  }
  # the horizontal axis's labels: their width at this size against the chart's
  labels <- tryCatch(if (inherits(p$coordinates, "CoordFlip")) NULL else built$layout$panel_params[[1]]$x$get_labels(),
                     error = function(e) NULL)
  labels <- as.character(unlist(labels))
  labels <- labels[!is.na(labels)]
  panels <- tryCatch(max(1, length(unique(built$layout$layout$COL))), error = function(e) 1)
  if (length(labels) > 1) {
    wide <- inches(sum(nchar(labels))) + length(labels) * s * 0.5 / 72
    if (wide > 0.85 * box[1] / panels) opts$x_text_angle <- 90
  }
  do.call(cd_chart_options, opts)
}

#' A block as it is drawn in a report
#'
#' Puts the report's region in a block that asks for it (`region = "@report"`). A report without a region is a
#' national report: such a block is then drawn for the whole country (or asks for a region when it needs one).
#' @param block A block.
#' @param project The report (its `region`; `NULL` or `""`: national).
#' @param regions Not used; kept for older callers.
#' @return The block.
#' @export
report_resolve_block <- function(block, project, regions = NULL) {
  if (identical(block$region, "@report")) {
    region <- project$region
    block$region <- if (is.character(region) && length(region) == 1 && nzchar(region)) region else NULL
  }
  block
}

# ---- sizes ---------------------------------------------------------------------------------------------------------

#' The size a chart or image block is shown at, in inches
#'
#' Worked out from the page (see [report_page()]): a full-width block spans the text, a half-width one half of it.
#' Shared by the builder's previews and by the exported files, so the page on screen and the file agree. A block on a
#' slide carries its box, `box = c(width, height)` in inches (see [report_project_blocks()]): it is drawn at that size.
#'
#' @param block A chart or image block.
#' @param design The report's design (`NULL`: the default page).
#' @return `c(width, height)` in inches.
#' @export
report_block_size <- function(block, design = NULL) {
  box <- .rb_box(block)
  if (!is.null(box)) return(box)
  page <- report_page(design %||% report_default_design())
  full <- page$text_width - 0.27
  size <- block$size %||% "full"
  w <- switch(size, half = (full - 0.2) / 2, third = (full - 0.4) / 3, full)
  if (identical(block$type, "image")) {
    w <- w * min(100, max(5, as.numeric(block$width %||% 100))) / 100
    h <- w * .rb_image_ratio(block)
    cap <- page$text_height * 0.8
    if (h > cap) { w <- w * cap / h; h <- cap }
    return(round(c(w, h), 3))
  }
  tall <- isTRUE(.ds_report_kinds()[[block$kind %||% ""]]$tall)
  ratio <- switch(size, half = if (tall) 1.143 else 0.825, third = if (tall) 1 else 0.7, if (tall) 0.8 else 0.569)
  # wide pages (chartbook, poster) would make very tall charts: the height stops at what an A4 page gives
  h <- min(w * ratio, if (tall) 5.2 else 3.7, page$text_height * 0.8)
  round(c(w, h), 3)
}

# A slide item's box, c(width, height) in inches, or NULL when the block is not on a slide
.rb_box <- function(block) {
  box <- suppressWarnings(as.numeric(unlist(block$box)))
  if (length(box) == 2 && all(is.finite(box)) && all(box > 0)) box else NULL
}

# The size a chart or image is shown at on the page (inches). A picture's is report_block_size(); a chart is drawn at
# report_block_size() and then treated as a picture of that shape: its width (percent of its column), crop and turn
# change the size it is shown at, as in the editor. Unformatted, the two are the same.
.rb_shown_size <- function(block, design = NULL) {
  size <- report_block_size(block, design)
  if (!identical(block$type, "chart")) return(size)
  block$type <- "image"
  block$ratio <- size[2] / size[1]
  report_block_size(block, design)
}

# Whether a chart is formatted as a picture (its drawing is then made into one and changed as pictures are)
.rb_chart_pictured <- function(block) {
  identical(block$type, "chart") && (
    any(.rb_image_crop(block) > 0) || as.integer(block$rotate %||% 0) %% 360 != 0 || isTRUE(block$flip_h) || isTRUE(block$flip_v) ||
      !(block$shape %||% "rect") %in% "rect" || isTRUE(block$border) || isTRUE(block$greyscale) ||
      !(as.numeric(block$brightness %||% 0) %in% 0) || !(as.numeric(block$contrast %||% 0) %in% 0) || nzchar(.rb_pic_style_of(block)))
}

# An image block's picture once turned, cropped and stretched: its height over its width (a circle is square before
# it is stretched)
.rb_image_ratio <- function(block) {
  if (identical(block$shape, "circle")) return(.rb_image_stretch(block))
  ratio <- suppressWarnings(as.numeric(block$ratio %||% 0.6))
  if (length(ratio) != 1 || !is.finite(ratio) || ratio <= 0) ratio <- 0.6
  if (as.integer(block$rotate %||% 0) %% 180 == 90) ratio <- 1 / ratio
  crop <- .rb_image_crop(block)
  ratio * (1 - crop[1] - crop[3]) / (1 - crop[2] - crop[4]) * .rb_image_stretch(block)
}

# How much a picture is stretched by a side handle: its height over the height its shape gives (1: not stretched)
.rb_image_stretch <- function(block) {
  s <- suppressWarnings(as.numeric(block$stretch %||% 1))
  if (length(s) != 1 || !is.finite(s) || s <= 0) return(1)
  min(10, max(0.1, s))
}

# How much of each side of a picture is cut off, as fractions: top, right, bottom, left (a tenth is always left)
.rb_image_crop <- function(block) {
  crop <- suppressWarnings(as.numeric(unlist(block$crop %||% list())))
  if (length(crop) != 4 || anyNA(crop)) return(c(0, 0, 0, 0))
  crop <- pmin(pmax(crop, 0), 90) / 100
  if (crop[1] + crop[3] > 0.9) crop[c(1, 3)] <- crop[c(1, 3)] * 0.9 / (crop[1] + crop[3])
  if (crop[2] + crop[4] > 0.9) crop[c(2, 4)] <- crop[c(2, 4)] * 0.9 / (crop[2] + crop[4])
  crop
}

#' Draw a chart block to a PNG file
#'
#' Charts are written as SVG (vector: sharp at any size, on screen, printed or in the PDF Word makes) when the
#' \pkg{svglite} package is installed and `file` ends in `.svg`; otherwise as PNG at `dpi`.
#'
#' @param rendered The result of [render_report_block()] with `type = "plot"`.
#' @param block The block (for its size).
#' @param file Path of the file to write: `.svg` or `.png`.
#' @param dpi Resolution of a PNG.
#' @param design The report's design (for the page size).
#' @return `file`, invisibly.
#' @export
save_report_chart <- function(rendered, block, file, dpi = 200, design = NULL) {
  # a narrow chart is drawn somewhat larger and shown smaller, so its legend and labels fit; not by more than 40%, so its
  # text stays readable once shrunk. A chart on a slide is drawn at its box's size, as it is in the slide file.
  shown <- report_block_size(block, design)
  size <- if (!is.null(.rb_box(block))) shown else shown * min(1.4, max(1, 4.6 / shown[1]))
  if (grepl("\\.svg$", file)) {
    svglite::svglite(file, width = size[1], height = size[2])
    on.exit(grDevices::dev.off(), add = TRUE)
    print(rendered$value)
    return(invisible(file))
  }
  draw <- function(device) {
    device(file, width = size[1], height = size[2], units = "in", res = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(rendered$value)
  }
  # ragg draws nothing, or only some letters, without an error, for some sizes of the fonts that carry bitmap versions of
  # their letters (Calibri, Cambria and the other ClearType fonts); the cairo device draws them
  cairo <- function(...) grDevices::png(..., type = "cairo")
  family <- tryCatch(ggplot2::calc_element("text", ggplot2::theme_grey() + rendered$value$theme)$family, error = function(e) "")
  bitmap_font <- isTRUE(family %in% c("Calibri", "Cambria", "Candara", "Consolas", "Constantia", "Corbel"))
  if (bitmap_font && isTRUE(capabilities("cairo"))) {
    draw(cairo)
  } else if (requireNamespace("ragg", quietly = TRUE)) {
    draw(ragg::agg_png)
    if (.rb_png_blank(file) && isTRUE(capabilities("cairo"))) draw(cairo)
  } else {
    draw(grDevices::png)
  }
  invisible(file)
}

# Whether a PNG is one flat colour (nothing was drawn)
.rb_png_blank <- function(file) {
  if (requireNamespace("png", quietly = TRUE)) {
    px <- tryCatch(png::readPNG(file), error = function(e) NULL)
    if (!is.null(px)) return(length(unique(as.vector(px))) <= 1)
  }
  isTRUE(file.info(file)$size < 3000)
}

# ---- building blocks -----------------------------------------------------------------------------------------------
#
# What an app's standard reports are written with: texts in several languages and the blocks they are made of.

#' Text in several languages
#'
#' A standard report is written once, in the app's languages; [report_in()] gives it in one.
#' @param en,fr,pt The text in English, French and Portuguese (French and Portuguese default to the English).
#' @return An object of class `rb_tx`.
#' @export
report_tx <- function(en, fr = en, pt = en) structure(list(en = en, fr = fr, pt = pt), class = "rb_tx")
.rb_tx <- report_tx

#' A text, block or report in one language
#'
#' Every [report_tx()] in `x` (a block, a list of blocks, a cover...) replaced by its text in `lang`.
#' @param x Anything holding texts made with [report_tx()].
#' @param lang The language.
#' @return `x` in that language.
#' @export
report_in <- function(x, lang = "en") {
  if (inherits(x, "rb_tx")) return(x[[lang]] %||% x$en)
  if (is.list(x)) {
    out <- lapply(x, report_in, lang = lang)
    attributes(out) <- attributes(x)
    return(out)
  }
  x
}

.rb_in <- report_in

#' The blocks of a report
#'
#' What a standard report is written with. Texts may be plain strings or [report_tx()].
#' @param text The text: plain for a heading, formatted (a small subset of HTML) for a paragraph or note.
#' @param level A heading's level (1 or 2).
#' @param kind,indicator,admin_level,size,variant,region,year,title A chart's or table's kind (one of the registered
#'   kinds, see [report_register()]) and what to draw; `size` is `"full"`, `"half"` or `"third"`.
#' @param type `"chart"` or `"table"`.
#' @param ... For `report_table()`, passed to `report_chart()`; for `report_questions()`, the questions (strings or
#'   [report_tx()]), one per line.
#' @return A block (a list).
#' @name report_blocks
NULL

#' @rdname report_blocks
#' @export
report_heading <- function(text, level = 1) list(type = "heading", text = text, level = level)
#' @rdname report_blocks
#' @export
report_paragraph <- function(text) list(type = "paragraph", text = text)
#' @rdname report_blocks
#' @export
report_note <- function(text) list(type = "note", text = text)
#' @rdname report_blocks
#' @export
report_page_break <- function() list(type = "pagebreak")
#' @rdname report_blocks
#' @export
report_chart <- function(kind, indicator = NULL, admin_level = "national", size = "full", variant = NULL, type = "chart", region = NULL,
                         year = NULL, title = NULL) {
  Filter(Negate(is.null), list(type = type, kind = kind, indicator = indicator, admin_level = admin_level, size = size, variant = variant,
                               region = region, year = year, title = title, caption = TRUE))
}
#' @rdname report_blocks
#' @export
report_table <- function(kind, ...) report_chart(kind, ..., type = "table", admin_level = NULL)

# A notes box of questions for the analyst ("Interpretations"), one per line. Each question is a string or a report_tx().
#' @rdname report_blocks
#' @export
report_questions <- function(..., title = report_tx("Interpretations", "Interpr\u00e9tations", "Interpreta\u00e7\u00f5es")) {
  items <- list(...)
  dot <- intToUtf8(0x2022)
  text <- lapply(c(en = "en", fr = "fr", pt = "pt"), function(lang) {
    lines <- vapply(items, function(q) .rb_in(q, lang), character(1))
    paste0("<b>", .rb_in(title, lang), "</b><br>", paste0(dot, " ", lines, collapse = "<br>"))
  })
  report_note(do.call(report_tx, unname(text)))
}

#' Every block of a report
#'
#' A document's blocks, or every item's block of a slide deck, in order, so a caller can count, check or draw them
#' the same way for both. A slide item's block gets the item's id and its box (`box = c(width, height)`, inches), which
#' [report_block_size()] draws it at. A document's free-layout page (a block of type `"canvas"`, whose `items` are
#' placed like a slide's) is given as its items' blocks, in the same way; the canvas block itself is not in the list.
#' @param project A report (`kind = "deck"` for a slide deck; anything else is a document).
#' @return A list of blocks.
#' @export
report_project_blocks <- function(project) {
  item_block <- function(item) {
    b <- item$block %||% list()
    b$id <- item$id %||% b$id
    b$box <- c(as.numeric(item$w %||% 1), as.numeric(item$h %||% 1))
    b
  }
  if (!identical(project$kind, "deck")) {
    out <- list()
    for (b in project$blocks %||% list()) {
      if (identical(b$type, "canvas")) {
        for (item in b$items %||% list()) out[[length(out) + 1]] <- item_block(item)
      } else {
        out[[length(out) + 1]] <- b
      }
    }
    return(out)
  }
  out <- list()
  for (slide in project$slides %||% list()) {
    for (item in slide$items %||% list()) out[[length(out) + 1]] <- item_block(item)
  }
  out
}
