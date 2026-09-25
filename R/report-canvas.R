# Free-layout pages: a page of a document laid out like a slide.
#
# A document block of type "canvas" is a whole page whose items sit at fixed boxes, as on a slide (see R/report-deck.R):
# text boxes, charts, tables and pictures placed freely (an infographic or dashboard page).
#
# Canvas block: id, type = "canvas", h (the height of its area in inches; by default the page's text height, see
#   report_page()), items
# Item: id, x, y, w, h (inches from the top-left corner of the page's text area, inside the margins),
#   role = "title" | "subtitle" | "heading" | "body" | NULL, block (a text, chart, table or image block, as on a slide).
#   The items are stacked in their order: the first is at the back. A text box can be filled (fill, fill_opacity,
#   outline: see R/report-deck.R), so text can sit on a coloured band over a picture.
#
# In the Word file the canvas takes a page of its own (it starts a new page, and what follows it starts another). Its
# items are Word's floating objects, all anchored to one paragraph at the top of that page and placed from the margins:
# pictures and charts as pictures (charts as the same SVG, with a PNG copy, that the document's charts are), text as
# text boxes whose paragraphs keep the item's formatting, tables as Word tables inside text boxes. All of them stay
# editable in Word. The PDF is made from that Word file, so it shows the same page. Without Word or LibreOffice, the
# page printed from HTML places the same items with CSS.

# The height of a canvas's area (inches)
.rb_canvas_height <- function(b, page) {
  h <- suppressWarnings(as.numeric(b$h))
  if (length(h) == 1 && is.finite(h) && h > 0) h else page$text_height
}

# Every item of a canvas drawn at its box: list(type = "canvas", height, items), each item list(id, role, box, block,
# r) where `r` is what is drawn (a chart's files, a table fitted to its box, a picture's file; NULL for text).
# {chart_indicator} and {chart_year} in its texts are those of its first chart or table, as on a slide.
.rb_canvas_prepare <- function(context, canvas, project, design, i18n, path, vector = FALSE) {
  items <- Filter(function(it) is.list(it) && is.list(it$block), canvas$items %||% list())
  blocks <- lapply(items, function(it) report_resolve_block(it$block, project))
  blocks <- .rb_deck_chart_fields(blocks, context, i18n)
  out <- vector("list", length(items))
  for (k in seq_along(items)) {
    item <- items[[k]]
    b <- blocks[[k]]
    box <- .rb_item_box(item)
    size <- unname(box[c("w", "h")])
    type <- b$type %||% "paragraph"
    file <- paste0(path, "_", k)
    r <- NULL
    if (.rb_empty_placeholder(item, b)) next
    if (type == "image") {
      b$box <- size
      # a picture kept in the dataset (src "asset:<id>")
      if (is.character(b$src) && startsWith(b$src, "asset:")) b$src_file <- .rb_asset_file(context, b$src, paste0(file, "_asset"))
      r <- .rb_image_file(b, paste0(file, "_image"), size)
    } else if (type %in% c("chart", "table")) {
      b$box <- size
      r <- render_report_block(context, b, i18n, design)
      if (identical(r$type, "table")) {
        r$value <- .rb_deck_table(r$value, size)
      } else if (identical(r$type, "plot")) {
        png <- paste0(file, "_chart.png")
        save_report_chart(r, b, png, dpi = 300, design = design)
        r$png <- png
        r$file <- png
        if (.rb_chart_pictured(b)) {
          # formatted as a picture (cropped, turned, recoloured, a shape): changed as a picture is
          pic <- .rb_image_file(utils::modifyList(b, list(src_file = png, ratio = size[2] / size[1])), paste0(file, "_chart"), size)
          r$file <- r$png <- pic$file %||% png
        } else if (vector) {
          r$file <- paste0(file, "_chart.svg")
          save_report_chart(r, b, r$file, design = design)
        }
      }
    } else if (!type %in% c("paragraph", "heading", "list", "note", "quote")) {
      next
    }
    out[[k]] <- list(id = as.character(item$id %||% paste0("item", k)), role = item$role, box = box, block = b, r = r,
                     fill = if (type %in% c("paragraph", "heading", "list", "note", "quote")) .rb_text_fill(item, b))
  }
  list(type = "canvas", height = .rb_canvas_height(canvas, report_page(design)), items = Filter(Negate(is.null), out))
}

# ---- Word ----------------------------------------------------------------------------------------------------------

# A canvas as one paragraph holding a marker per item (`store` keeps what each marker becomes); .rb_docx_canvas() turns
# the markers into floating objects once the file is written
.rb_canvas_docx <- function(doc, r, parts, i18n, store) {
  runs <- list()
  tiny <- officer::fp_text_lite(font.size = 1)
  for (it in r$items %||% list()) {
    spec <- .rb_canvas_spec(it, parts, i18n)
    if (is.null(spec)) next
    key <- sprintf("@@RBCI:%d@@", length(store$specs) + 1L)
    spec$box <- it$box
    spec$name <- it$id
    spec$fill <- it$fill
    store$specs[[key]] <- spec
    runs <- c(runs, list(officer::ftext(key, tiny)))
  }
  if (!length(runs)) runs <- list(officer::ftext("", tiny))
  par <- officer::fp_par(padding = 0, line_spacing = 1)
  officer::body_add_fpar(doc, do.call(officer::fpar, c(runs, list(fp_p = par))))
}

# What one item becomes: list(kind = "picture", file, svg) or list(kind = "text", xml (the text box's content), anchor
# ("t", "ctr", "b"), inset (inches: left and right, top and bottom))
.rb_canvas_spec <- function(it, parts, i18n) {
  b <- it$block
  r <- it$r
  type <- b$type %||% "paragraph"
  error <- function(message) {
    run <- officer::ftext(.rb_error_text(i18n, message %||% ""), officer::fp_text_lite(font.size = 9, color = "#9b2c2c"))
    list(kind = "text", xml = .rb_wml(officer::fpar(run)), anchor = "t", inset = c(0.1, 0.05))
  }
  if (type %in% c("chart", "table", "image")) {
    if (is.null(r) || identical(r$type, "error") || (!identical(r$type, "table") && is.null(r$file))) return(error(r$message))
    if (identical(r$type, "table")) {
      # a table must be followed by a paragraph in a text box, as in a table cell
      end <- '<w:p><w:pPr><w:spacing w:before="0" w:after="0" w:line="20" w:lineRule="exact"/></w:pPr></w:p>'
      return(list(kind = "text", xml = paste0(.rb_wml(r$value), end), anchor = "t", inset = c(0, 0)))
    }
    svg <- if (grepl("\\.svg$", r$file, ignore.case = TRUE)) r$file
    png <- if (is.null(svg)) r$file else r$png %||% .rb_svg_png(r$file, it$box[["w"]])
    if (is.null(png)) return(error("The picture could not be read."))
    return(list(kind = "picture", file = png, svg = svg))
  }
  lines <- .rb_deck_lines(list(role = it$role), b, parts$design, parts$fields)
  # an empty text box is left out, unless it is filled or outlined (a coloured box)
  if (is.null(lines) && is.null(it$fill)) return(NULL)
  if (is.null(lines)) return(list(kind = "text", xml = "<w:p/>", anchor = "t", inset = c(0.1, 0.05)))
  xml <- vapply(lines, function(l) {
    runs <- .rb_deck_runs(l)
    tabs <- NULL
    if (!is.null(l$kind)) {
      # the marker at the hanging indent, the text at the tab stop where the indent ends
      mark <- officer::ftext(paste0(.rb_marker(l$kind, l$level, l$num), "\t"),
                             officer::fp_text_lite(font.size = l$size, font.family = l$family, hansi.family = l$family, color = l$color))
      runs <- c(list(mark), runs)
      tabs <- officer::fp_tabs(officer::fp_tab(pos = l$left / 72, style = "left"))
    }
    args <- list(text.align = l$align, line_spacing = l$line, padding.top = 0, padding.bottom = l$after, padding.left = l$left)
    if (l$hanging > 0) args$hanging <- l$hanging
    if (!is.null(tabs)) args$tabs <- tabs
    .rb_wml(do.call(officer::fpar, c(runs, list(fp_p = do.call(officer::fp_par, args)))))
  }, character(1))
  anchor <- switch(b$valign %||% (if (identical(it$role, "title")) "middle" else "top"), middle = "ctr", bottom = "b", "t")
  # PowerPoint's space inside a text box (0.1 in at the sides, 0.05 in above and below), as on a slide
  list(kind = "text", xml = paste(xml, collapse = ""), anchor = anchor, inset = c(0.1, 0.05))
}

# Word XML of a paragraph or a table, as it goes inside a text box: without the namespaces officer declares on it and
# without its style reference (officer resolves that only for what it writes into the body itself: the text box
# paragraphs take the document's default paragraph style, their formatting is their own)
.rb_wml <- function(x) {
  xml <- enc2utf8(paste(officer::to_wml(x), collapse = ""))
  xml <- gsub(' xmlns:[A-Za-z0-9]+="[^"]*"', "", xml)
  gsub("<w:pStyle w:pstlname=\"[^\"]*\"/>", "", xml)
}

# A PNG copy of an SVG picture, for programs that cannot draw SVG; NULL when it cannot be made
.rb_svg_png <- function(svg, width_in, dpi = 300) {
  if (!requireNamespace("rsvg", quietly = TRUE)) return(NULL)
  out <- sub("\\.svg$", "_copy.png", svg, ignore.case = TRUE)
  ok <- tryCatch({ rsvg::rsvg_png(svg, file = out, width = round(width_in * dpi)); TRUE }, error = function(e) FALSE)
  if (ok) out else NULL
}

# One floating object: placed from the page's margins (offsets and size in EMU), in front of the text, the text not
# wrapped around it; `z` orders the objects of a page (the higher in front)
.rb_canvas_anchor <- function(box, id, name, z, graphic, frame = "<wp:cNvGraphicFramePr/>") {
  emu <- function(v) format(round(v * 914400), scientific = FALSE, trim = TRUE)
  paste0(
    '<w:r><w:drawing><wp:anchor distT="0" distB="0" distL="0" distR="0" simplePos="0" relativeHeight="', z,
    '" behindDoc="0" locked="0" layoutInCell="1" allowOverlap="1"><wp:simplePos x="0" y="0"/>',
    '<wp:positionH relativeFrom="margin"><wp:posOffset>', emu(box[["x"]]), "</wp:posOffset></wp:positionH>",
    '<wp:positionV relativeFrom="margin"><wp:posOffset>', emu(box[["y"]]), "</wp:posOffset></wp:positionV>",
    '<wp:extent cx="', emu(box[["w"]]), '" cy="', emu(box[["h"]]), '"/><wp:effectExtent l="0" t="0" r="0" b="0"/><wp:wrapNone/>',
    '<wp:docPr id="', id, '" name="', htmltools::htmlEscape(name, attribute = TRUE), '"/>', frame, graphic,
    "</wp:anchor></w:drawing></w:r>"
  )
}

.rb_canvas_ns_a <- "http://schemas.openxmlformats.org/drawingml/2006/main"

# A picture's graphic (the PNG, and the SVG Word draws instead when there is one)
.rb_canvas_picture <- function(box, png_id, svg_id, name) {
  emu <- function(v) format(round(v * 914400), scientific = FALSE, trim = TRUE)
  blip <- if (is.null(svg_id)) {
    sprintf('<a:blip r:embed="%s"/>', png_id)
  } else {
    sprintf(paste0('<a:blip r:embed="%s"><a:extLst><a:ext uri="{96DAC541-7B7A-43D3-8B79-37D633B846F1}">',
                   '<asvg:svgBlip xmlns:asvg="http://schemas.microsoft.com/office/drawing/2016/SVG/main" r:embed="%s"/></a:ext></a:extLst></a:blip>'),
            png_id, svg_id)
  }
  paste0(
    '<a:graphic xmlns:a="', .rb_canvas_ns_a, '"><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">',
    '<pic:pic xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:nvPicPr><pic:cNvPr id="0" name="',
    htmltools::htmlEscape(name, attribute = TRUE), '"/><pic:cNvPicPr/></pic:nvPicPr><pic:blipFill>', blip,
    '<a:stretch><a:fillRect/></a:stretch></pic:blipFill><pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="', emu(box[["w"]]),
    '" cy="', emu(box[["h"]]), '"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr></pic:pic></a:graphicData></a:graphic>'
  )
}

# A text box's graphic holding `content` (Word paragraphs and tables); `fill`: its fill and outline (.rb_text_fill())
.rb_canvas_textbox <- function(box, content, anchor, inset, fill = NULL) {
  emu <- function(v) format(round(v * 914400), scientific = FALSE, trim = TRUE)
  paste0(
    '<a:graphic xmlns:a="', .rb_canvas_ns_a, '"><a:graphicData uri="http://schemas.microsoft.com/office/word/2010/wordprocessingShape">',
    '<wps:wsp><wps:cNvSpPr txBox="1"/><wps:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="', emu(box[["w"]]), '" cy="', emu(box[["h"]]),
    '"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom>',
    if (is.null(fill)) "<a:noFill/><a:ln><a:noFill/></a:ln>" else .rb_text_fill_xml(fill), "</wps:spPr>",
    "<wps:txbx><w:txbxContent>", content, "</w:txbxContent></wps:txbx>",
    '<wps:bodyPr rot="0" vert="horz" wrap="square" lIns="', emu(inset[1]), '" tIns="', emu(inset[2]), '" rIns="', emu(inset[1]),
    '" bIns="', emu(inset[2]), '" anchor="', anchor, '" anchorCtr="0"><a:noAutofit/></wps:bodyPr></wps:wsp></a:graphicData></a:graphic>'
  )
}

# The written Word file with each canvas marker replaced by its floating object: pictures copied into the file and
# linked, text boxes written out, stacked in the items' order
.rb_docx_canvas <- function(file, specs) {
  if (!length(specs) || !requireNamespace("zip", quietly = TRUE)) return(invisible(FALSE))
  dir <- tempfile("docx_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(file, exdir = dir)
  read <- function(p) {
    x <- readChar(p, file.info(p)$size, useBytes = TRUE)
    Encoding(x) <- "UTF-8"
    x
  }
  write <- function(txt, p) writeChar(enc2utf8(txt), p, eos = NULL, useBytes = TRUE)
  doc_path <- file.path(dir, "word", "document.xml")
  rels_path <- file.path(dir, "word", "_rels", "document.xml.rels")
  types_path <- file.path(dir, "[Content_Types].xml")
  body <- read(doc_path)
  rels <- read(rels_path)
  types <- read(types_path)
  next_rel <- max(c(0, as.numeric(regmatches(rels, gregexpr('(?<=Id="rId)[0-9]+', rels, perl = TRUE))[[1]]))) + 1
  next_pr <- max(c(0, as.numeric(regmatches(body, gregexpr('(?<=<wp:docPr id=")[0-9]+', body, perl = TRUE))[[1]]))) + 1
  dir.create(file.path(dir, "word", "media"), showWarnings = FALSE)
  content_type <- c(png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif", svg = "image/svg+xml")
  add_media <- function(path, stem) {
    ext <- tolower(tools::file_ext(path))
    if (!ext %in% names(content_type)) ext <- "png"
    target <- paste0("media/", stem, ".", ext)
    file.copy(path, file.path(dir, "word", target), overwrite = TRUE)
    id <- paste0("rId", next_rel)
    next_rel <<- next_rel + 1
    rels <<- sub("</Relationships>", sprintf('<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="%s"/></Relationships>', id, target), rels, fixed = TRUE)
    if (!grepl(sprintf('Extension="%s"', ext), types, ignore.case = TRUE)) {
      types <<- sub("<Default ", sprintf('<Default Extension="%s" ContentType="%s"/><Default ', ext, content_type[[ext]]), types, fixed = TRUE)
    }
    id
  }
  # a link in a text box: its address as the file's relationship (officer does this only for the body's own text)
  add_links <- function(xml) {
    hits <- unique(regmatches(xml, gregexpr('<w:hyperlink r:id="[^"]*"', xml))[[1]])
    for (h in hits) {
      url <- sub('^<w:hyperlink r:id="([^"]*)"$', "\\1", h)
      if (grepl("^rId[0-9]+$", url)) next
      id <- paste0("rId", next_rel)
      next_rel <<- next_rel + 1
      rels <<- sub("</Relationships>", sprintf('<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink" Target="%s" TargetMode="External"/></Relationships>', id, url), rels, fixed = TRUE)
      xml <- gsub(h, sprintf('<w:hyperlink r:id="%s"', id), xml, fixed = TRUE)
    }
    xml
  }
  keys <- names(specs)
  for (k in seq_along(keys)) {
    spec <- specs[[k]]
    at <- regexpr(keys[k], body, fixed = TRUE)
    if (at < 0) next
    # the run holding the marker
    before <- substr(body, 1, at - 1)
    starts <- gregexpr("<w:r>|<w:r ", before)[[1]]
    start <- starts[length(starts)]
    end <- regexpr("</w:r>", substr(body, at, nchar(body)), fixed = TRUE)
    if (start < 0 || end < 0) next
    end <- at + end + nchar("</w:r>") - 2
    name <- spec$name %||% paste0("Item ", k)
    z <- 251658240 + k
    if (identical(spec$kind, "picture")) {
      stem <- sprintf("rbcanvas_%d", k)
      png_id <- add_media(spec$file, stem)
      svg_id <- if (!is.null(spec$svg)) add_media(spec$svg, paste0(stem, "_svg"))
      frame <- paste0('<wp:cNvGraphicFramePr><a:graphicFrameLocks xmlns:a="', .rb_canvas_ns_a, '" noChangeAspect="1"/></wp:cNvGraphicFramePr>')
      xml <- .rb_canvas_anchor(spec$box, next_pr, name, z, .rb_canvas_picture(spec$box, png_id, svg_id, name), frame)
    } else {
      xml <- .rb_canvas_anchor(spec$box, next_pr, name, z, .rb_canvas_textbox(spec$box, add_links(spec$xml), spec$anchor, spec$inset, spec$fill))
    }
    next_pr <- next_pr + 1
    body <- paste0(substr(body, 1, start - 1), xml, substr(body, end + 1, nchar(body)))
  }
  write(body, doc_path)
  write(rels, rels_path)
  write(types, types_path)
  out <- tempfile(fileext = ".docx")
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- c(files[files == "[Content_Types].xml"], files[files != "[Content_Types].xml"])
  zip::zip(out, files = files, root = dir, mode = "mirror")
  file.copy(out, file, overwrite = TRUE)
  unlink(out)
  invisible(TRUE)
}

# ---- HTML ----------------------------------------------------------------------------------------------------------

# A canvas in the HTML a PDF is printed from without Word or LibreOffice: a page-sized box, each item placed in it
.rb_canvas_html <- function(r, parts, i18n) {
  if (is.null(r)) return("")
  design <- parts$design
  inch <- function(v) paste0(round(v, 4), "in")
  items <- vapply(r$items %||% list(), function(it) {
    b <- it$block
    box <- it$box
    type <- b$type %||% "paragraph"
    pos <- paste0("left:", inch(box[["x"]]), ";top:", inch(box[["y"]]), ";width:", inch(box[["w"]]), ";height:", inch(box[["h"]]))
    x <- it$r
    if (type %in% c("chart", "table", "image")) {
      if (is.null(x) || identical(x$type, "error")) {
        inner <- paste0('<p class="err">', .rb_esc(.rb_error_text(i18n, x$message %||% "")), "</p>")
      } else if (identical(x$type, "table")) {
        inner <- paste0('<div class="tbl">', as.character(flextable::htmltools_value(x$value)), "</div>")
      } else {
        inner <- paste0('<img src="', .rb_data_uri(x$file), '" alt="', .rb_esc(b$alt %||% b$title %||% b$kind %||% ""), '">')
      }
      return(paste0('<div class="it" style="', pos, '">', inner, "</div>"))
    }
    lines <- .rb_deck_lines(list(role = it$role), b, design, parts$fields)
    if (is.null(lines) && is.null(it$fill)) return("")
    bg <- ""
    if (!is.null(it$fill)) {
      rgba <- function(hex, a) { v <- grDevices::col2rgb(paste0("#", hex)); sprintf("rgba(%d,%d,%d,%s)", v[1], v[2], v[3], round(a, 3)) }
      if (!is.null(it$fill$fill)) bg <- paste0(bg, ";background:", rgba(it$fill$fill, it$fill$opacity))
      if (!is.null(it$fill$outline)) bg <- paste0(bg, ";border:1pt solid #", it$fill$outline)
    }
    html <- if (is.null(lines)) "" else vapply(lines, function(l) {
      mark <- if (!is.null(l$kind)) paste0('<span style="display:inline-block;width:', l$hanging, 'pt">', .rb_marker(l$kind, l$level, l$num), "</span>") else ""
      runs <- if (length(l$runs)) .rb_html_runs(l$runs) else "&nbsp;"
      paste0('<p class="ln" style="text-align:', l$align, ";font-size:", l$size, "pt;font-family:'", l$family, "';color:", l$color,
             ";line-height:", round(l$line * 1.2, 2), ";margin-bottom:", l$after, "pt;padding-left:", l$left, "pt;text-indent:", -l$hanging,
             'pt">', mark, runs, "</p>")
    }, character(1))
    justify <- switch(b$valign %||% (if (identical(it$role, "title")) "middle" else "top"), middle = "center", bottom = "flex-end", "flex-start")
    paste0('<div class="it" style="', pos, ";padding:0.05in 0.1in;justify-content:", justify, bg, '">', paste(html, collapse = ""), "</div>")
  }, character(1))
  paste0('<div class="canvas" style="width:', inch(parts$page$text_width), ";height:", inch(r$height), '">', paste(items, collapse = ""), "</div>")
}
