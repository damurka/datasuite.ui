# Office files as themes and templates.
#
# A PowerPoint file (.pptx, .potx) or a Word file (.docx, .dotx) can give a report its theme: report_theme_from_file()
# reads the file's colours, fonts and (Word) text sizes into a design, as the ready-made themes are (report_themes()).
# The same file can then be the template the report is written from: a design whose `template` is the path of the file
# on disk is exported on top of it (export_deck() starts from the PowerPoint file's masters, layouts, background and
# slide size; a document from the Word file's styles, headers, footers and page setup). A template that cannot be
# opened is left out, with a warning: the export never fails because of it.

.rb_ooxml_ns <- c(
  a = "http://schemas.openxmlformats.org/drawingml/2006/main",
  p = "http://schemas.openxmlformats.org/presentationml/2006/main",
  w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
  rel = "http://schemas.openxmlformats.org/package/2006/relationships"
)

# What an Office file is, from what it holds: "pptx", "docx", or NULL (not an Office file, or not readable)
.rb_office_kind <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  files <- tryCatch(utils::unzip(path, list = TRUE)$Name, error = function(e) NULL, warning = function(w) NULL)
  if ("ppt/presentation.xml" %in% files) return("pptx")
  if ("word/document.xml" %in% files) return("docx")
  NULL
}

# ---- colours -------------------------------------------------------------------------------------------------------

.rb_rgb <- function(hex) as.numeric(grDevices::col2rgb(hex)) / 255
.rb_rgb_hex <- function(v) grDevices::rgb(pmin(1, pmax(0, v[1])), pmin(1, pmax(0, v[2])), pmin(1, pmax(0, v[3])))

# `a` moved towards `b` by `t` (0: a, 1: b)
.rb_mix <- function(a, b, t) .rb_rgb_hex(.rb_rgb(a) * (1 - t) + .rb_rgb(b) * t)

# Relative luminance (0 black, 1 white)
.rb_luminance <- function(hex) {
  v <- .rb_rgb(hex)
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * v)
}

# Office's colour changes: lumMod / lumOff (HSL lightness), shade (towards black), tint (towards white); values as
# fractions
.rb_color_mods <- function(hex, lum_mod = 1, lum_off = 0, shade = 1, tint = 1) {
  v <- .rb_rgb(hex)
  if (lum_mod != 1 || lum_off != 0) {
    mx <- max(v)
    mn <- min(v)
    l <- (mx + mn) / 2
    d <- mx - mn
    s <- if (d == 0) 0 else d / (1 - abs(2 * l - 1))
    h <- if (d == 0) 0 else if (mx == v[1]) ((v[2] - v[3]) / d) %% 6 else if (mx == v[2]) (v[3] - v[1]) / d + 2 else (v[1] - v[2]) / d + 4
    l <- min(1, max(0, l * lum_mod + lum_off))
    c <- (1 - abs(2 * l - 1)) * s
    x <- c * (1 - abs(h %% 2 - 1))
    m <- l - c / 2
    k <- floor(h) %% 6
    v <- switch(k + 1, c(c, x, 0), c(x, c, 0), c(0, c, x), c(0, x, c), c(x, 0, c), c(c, 0, x)) + m
  }
  if (shade != 1) v <- v * shade
  if (tint != 1) v <- v + (1 - v) * (1 - tint)
  .rb_rgb_hex(v)
}

# The colour a DrawingML colour node gives (srgbClr, sysClr, schemeClr, prstClr), its changes applied; `scheme` holds
# the theme's colours by name (dk1, lt1, ..., accent6) and `map` maps bg1 / tx1 / bg2 / tx2 to them
.rb_dml_color <- function(node, scheme, map = NULL) {
  if (is.null(node) || inherits(node, "xml_missing")) return(NA_character_)
  name <- xml2::xml_name(node)
  val <- xml2::xml_attr(node, "val")
  map <- map %||% c(bg1 = "lt1", tx1 = "dk1", bg2 = "lt2", tx2 = "dk2")
  hex <- switch(
    name,
    srgbClr = paste0("#", val),
    sysClr = {
      last <- xml2::xml_attr(node, "lastClr")
      paste0("#", if (!is.na(last)) last else if (identical(val, "window")) "FFFFFF" else "000000")
    },
    schemeClr = {
      key <- if (!is.na(val) && val %in% names(map)) map[[val]] else val
      scheme[[key]] %||% NA_character_
    },
    prstClr = tryCatch(.rb_rgb_hex(.rb_rgb(val)), error = function(e) NA_character_),
    NA_character_
  )
  if (is.na(hex) || !grepl("^#[0-9A-Fa-f]{6}$", hex)) return(NA_character_)
  mod <- function(tag) {
    v <- xml2::xml_attr(xml2::xml_find_first(node, paste0("a:", tag), .rb_ooxml_ns), "val")
    if (is.na(v)) NA_real_ else as.numeric(v) / 100000
  }
  or1 <- function(x, d) if (is.na(x)) d else x
  toupper(.rb_color_mods(hex, or1(mod("lumMod"), 1), or1(mod("lumOff"), 0), or1(mod("shade"), 1), or1(mod("tint"), 1)))
}

# ---- reading the file ----------------------------------------------------------------------------------------------

# A part's path from a relationship target, relative to the folder of the part that points to it
.rb_part_path <- function(from_dir, target) {
  if (startsWith(target, "/")) return(sub("^/", "", target))
  parts <- c(if (nzchar(from_dir)) strsplit(from_dir, "/", fixed = TRUE)[[1]], strsplit(target, "/", fixed = TRUE)[[1]])
  out <- character()
  for (p in parts) {
    if (p == "..") out <- utils::head(out, -1) else if (p != "." && nzchar(p)) out <- c(out, p)
  }
  paste(out, collapse = "/")
}

# The target (a path in the package) of the relationship `id` (or the first of `type`) of the part `part`
.rb_rel_target <- function(dir, part, id = NULL, type = NULL) {
  rels <- file.path(dir, dirname(part), "_rels", paste0(basename(part), ".rels"))
  if (!file.exists(rels)) return(NULL)
  xml <- xml2::read_xml(rels)
  nodes <- xml2::xml_find_all(xml, "//rel:Relationship", .rb_ooxml_ns)
  hit <- if (!is.null(id)) nodes[xml2::xml_attr(nodes, "Id") == id] else nodes[endsWith(xml2::xml_attr(nodes, "Type"), paste0("/", type))]
  if (!length(hit)) return(NULL)
  from <- dirname(part)
  .rb_part_path(if (from == ".") "" else from, xml2::xml_attr(hit[[1]], "Target"))
}

.rb_read_part <- function(dir, part) {
  if (is.null(part)) return(NULL)
  p <- file.path(dir, part)
  if (!file.exists(p)) return(NULL)
  tryCatch(xml2::read_xml(p), error = function(e) NULL)
}

# A theme part's colours (named: dk1, lt1, dk2, lt2, accent1..6, hlink, folHlink) and its heading and body fonts
.rb_theme_part <- function(theme) {
  scheme <- list()
  if (!is.null(theme)) {
    nodes <- xml2::xml_children(xml2::xml_find_first(theme, "//a:clrScheme", .rb_ooxml_ns))
    for (n in nodes) scheme[[xml2::xml_name(n)]] <- .rb_dml_color(xml2::xml_child(n, 1), list())
  }
  font <- function(which) {
    if (is.null(theme)) return(NA_character_)
    v <- xml2::xml_attr(xml2::xml_find_first(theme, sprintf("//a:fontScheme/a:%s/a:latin", which), .rb_ooxml_ns), "typeface")
    if (is.na(v) || !nzchar(v)) NA_character_ else v
  }
  list(scheme = scheme, major = font("majorFont"), minor = font("minorFont"))
}

#' A report theme from a PowerPoint or Word file
#'
#' Reads the colours and fonts of an Office theme or template (PowerPoint `.pptx` / `.potx`, Word `.docx` / `.dotx`)
#' into a report theme, as [report_themes()] gives them: the accent is the theme's first accent colour, the text colour
#' its first dark colour, the chart palette its six accent colours, the heading and body fonts its heading and body
#' fonts. A Word file's styles give the heading colour (Heading 1's), the fonts and the text sizes when they set them,
#' and its page size, orientation and margins. A PowerPoint file's master gives the title colour and fonts, the slide
#' size and the background colour.
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
#'   x, y, w, h)` (a band or bar), each also with `from` (`"master"`, `"layout"` or `"slide"`: where it was found); `title`, `subtitle`, `body` the style of that text, `list(x, y, w, h, fill,
#'   fill_opacity, color, font_size, bold, align, font)`, or `NULL`. Boxes are in inches from the slide's top-left
#'   corner; colours are `"#rrggbb"`. [export_deck()] draws a design's background and decor on its slides.
#' @export
report_theme_from_file <- function(path, name = NULL) {
  kind <- .rb_office_kind(path)
  if (is.null(kind)) .ds_abort(c("x" = "This is not a PowerPoint or Word file (.pptx, .potx, .docx, .dotx)."))
  dir <- tempfile("theme_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = dir)

  out <- report_default_design()
  label <- name %||% tools::file_path_sans_ext(basename(path))
  out$theme <- paste0("custom_", substr(unname(tools::md5sum(path)), 1, 10))
  out$name <- list(en = label, fr = label, pt = label)

  if (kind == "pptx") {
    master_part <- NULL
    pres <- .rb_read_part(dir, "ppt/presentation.xml")
    if (!is.null(pres)) {
      id <- xml2::xml_attr(xml2::xml_find_first(pres, "//p:sldMasterIdLst/p:sldMasterId", .rb_ooxml_ns), "r:id", ns = .rb_ooxml_ns)
      if (!is.na(id)) master_part <- .rb_rel_target(dir, "ppt/presentation.xml", id = id)
    }
    master <- .rb_read_part(dir, master_part)
    theme_part <- if (!is.null(master_part)) .rb_rel_target(dir, master_part, type = "theme")
    theme <- .rb_read_part(dir, theme_part %||% "ppt/theme/theme1.xml")
  } else {
    theme <- .rb_read_part(dir, .rb_rel_target(dir, "word/document.xml", type = "theme") %||% "word/theme/theme1.xml")
    master <- NULL
  }
  th <- .rb_theme_part(theme)
  scheme <- th$scheme
  get <- function(key, default) { v <- scheme[[key]]; if (is.null(v) || is.na(v)) default else v }
  dk1 <- get("dk1", "#000000")
  lt1 <- get("lt1", "#FFFFFF")
  dk2 <- get("dk2", dk1)
  accent1 <- get("accent1", out$accent)
  palette <- vapply(paste0("accent", 1:6), function(k) get(k, NA_character_), character(1))
  palette <- unname(palette[!is.na(palette)])
  heading_font <- th$major
  body_font <- th$minor
  heading_color <- NA_character_
  resolve_font <- function(v) {
    if (is.na(v) || !nzchar(v)) return(NA_character_)
    if (startsWith(v, "+mj")) return(th$major)
    if (startsWith(v, "+mn")) return(th$minor)
    v
  }

  if (kind == "pptx") {
    map <- NULL
    if (!is.null(master)) {
      cm <- xml2::xml_find_first(master, "//p:clrMap", .rb_ooxml_ns)
      if (!inherits(cm, "xml_missing")) map <- unlist(xml2::xml_attrs(cm))
      title <- xml2::xml_find_first(master, "//p:txStyles/p:titleStyle/a:lvl1pPr/a:defRPr", .rb_ooxml_ns)
      if (!inherits(title, "xml_missing")) {
        heading_color <- .rb_dml_color(xml2::xml_find_first(title, "a:solidFill/*", .rb_ooxml_ns), scheme, map)
        f <- resolve_font(xml2::xml_attr(xml2::xml_find_first(title, "a:latin", .rb_ooxml_ns), "typeface"))
        if (!is.na(f)) heading_font <- f
      }
      body <- xml2::xml_find_first(master, "//p:txStyles/p:bodyStyle/a:lvl1pPr/a:defRPr/a:latin", .rb_ooxml_ns)
      f <- resolve_font(xml2::xml_attr(body, "typeface"))
      if (!is.na(f)) body_font <- f
      # the master's background: its own fill, or a fill of the theme given a colour
      bg <- xml2::xml_find_first(master, "//p:cSld/p:bg/p:bgPr/a:solidFill/*", .rb_ooxml_ns)
      background <- .rb_dml_color(bg, scheme, map)
      if (is.na(background)) {
        ref <- xml2::xml_find_first(master, "//p:cSld/p:bg/p:bgRef", .rb_ooxml_ns)
        idx <- suppressWarnings(as.integer(xml2::xml_attr(ref, "idx")))
        if (!is.na(idx) && idx > 1000 && !is.null(theme)) {
          fills <- xml2::xml_children(xml2::xml_find_first(theme, "//a:bgFillStyleLst", .rb_ooxml_ns))
          if (length(fills) >= idx - 1000 && xml2::xml_name(fills[[idx - 1000]]) == "solidFill") {
            background <- .rb_dml_color(xml2::xml_child(ref, 1), scheme, map)
          }
        }
      }
      out["background"] <- list(if (!is.na(background) && toupper(background) != "#FFFFFF") tolower(background))
    } else {
      out["background"] <- list(NULL)
    }
    sz <- if (!is.null(pres)) xml2::xml_find_first(pres, "//p:sldSz", .rb_ooxml_ns)
    if (!is.null(sz) && !inherits(sz, "xml_missing")) {
      ratio <- as.numeric(xml2::xml_attr(sz, "cx")) / as.numeric(xml2::xml_attr(sz, "cy"))
      if (is.finite(ratio)) out$slide_size <- if (abs(ratio - 4 / 3) < abs(ratio - 16 / 9)) "4:3" else "16:9"
    }
    # the look the slides themselves have: logos, bands, the title's box and colours (R/report-slide-design.R)
    designs <- if (!is.null(pres)) tryCatch(.rb_slide_designs(dir, pres, theme, th, map), error = function(e) NULL)
    out$slide_designs <- list(title = designs$title, content = designs$content)
  } else {
    styles <- .rb_read_part(dir, "word/styles.xml")
    style <- function(id) {
      if (is.null(styles)) return(NULL)
      n <- xml2::xml_find_first(styles, sprintf("//w:style[@w:styleId='%s']", id), .rb_ooxml_ns)
      if (inherits(n, "xml_missing")) {
        # a style found by its name (the id is translated in some languages' Word)
        n <- xml2::xml_find_first(styles, sprintf("//w:style[w:name[translate(@w:val, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')='%s']]",
                                                  tolower(sub("([a-z])([0-9])", "\\1 \\2", id))), .rb_ooxml_ns)
      }
      if (inherits(n, "xml_missing")) NULL else n
    }
    defaults <- if (!is.null(styles)) xml2::xml_find_first(styles, "//w:docDefaults/w:rPrDefault/w:rPr", .rb_ooxml_ns)
    wattr <- function(node, path, attr) {
      if (is.null(node) || inherits(node, "xml_missing")) return(NA_character_)
      xml2::xml_attr(xml2::xml_find_first(node, path, .rb_ooxml_ns), attr, ns = .rb_ooxml_ns)
    }
    word_font <- function(node) {
      if (is.null(node)) return(NA_character_)
      explicit <- wattr(node, ".//w:rPr/w:rFonts|./w:rFonts", "w:ascii")
      if (!is.na(explicit) && nzchar(explicit)) return(explicit)
      themed <- wattr(node, ".//w:rPr/w:rFonts|./w:rFonts", "w:asciiTheme")
      if (is.na(themed)) return(NA_character_)
      if (startsWith(themed, "major")) th$major else th$minor
    }
    word_size <- function(node) {
      v <- suppressWarnings(as.numeric(wattr(node, ".//w:rPr/w:sz|./w:sz", "w:val")))
      if (is.na(v)) NA_real_ else v / 2
    }
    word_color <- function(node) {
      if (is.null(node)) return(NA_character_)
      c <- xml2::xml_find_first(node, ".//w:rPr/w:color", .rb_ooxml_ns)
      if (inherits(c, "xml_missing")) return(NA_character_)
      val <- xml2::xml_attr(c, "w:val", ns = .rb_ooxml_ns)
      if (!is.na(val) && grepl("^[0-9A-Fa-f]{6}$", val)) return(toupper(paste0("#", val)))
      tc <- xml2::xml_attr(c, "w:themeColor", ns = .rb_ooxml_ns)
      if (is.na(tc)) return(if (identical(val, "auto")) dk1 else NA_character_)
      key <- c(text1 = "dk1", text2 = "dk2", background1 = "lt1", background2 = "lt2", dark1 = "dk1", dark2 = "dk2", light1 = "lt1", light2 = "lt2")
      hex <- scheme[[if (tc %in% names(key)) key[[tc]] else tc]]
      if (is.null(hex) || is.na(hex)) return(NA_character_)
      shade <- xml2::xml_attr(c, "w:themeShade", ns = .rb_ooxml_ns)
      tint <- xml2::xml_attr(c, "w:themeTint", ns = .rb_ooxml_ns)
      toupper(.rb_color_mods(hex, shade = if (is.na(shade)) 1 else strtoi(shade, 16L) / 255, tint = if (is.na(tint)) 1 else strtoi(tint, 16L) / 255))
    }
    normal <- style("Normal")
    h1 <- style("Heading1")
    f <- word_font(normal)
    if (is.na(f)) f <- word_font(defaults)
    if (!is.na(f)) body_font <- f
    f <- word_font(h1)
    if (!is.na(f)) heading_font <- f
    heading_color <- word_color(h1)
    sizes <- c(body_size = word_size(normal), h1_size = word_size(h1), h2_size = word_size(style("Heading2")),
               title_size = word_size(style("Title")), caption_size = word_size(style("Caption")))
    if (is.na(sizes[["body_size"]])) sizes[["body_size"]] <- word_size(defaults)
    for (k in names(sizes)) if (!is.na(sizes[[k]]) && sizes[[k]] > 0) out[[k]] <- sizes[[k]]
    # the page: the nearest of the report's sizes and margins
    doc <- .rb_read_part(dir, "word/document.xml")
    sect <- if (!is.null(doc)) xml2::xml_find_first(doc, "//w:body/w:sectPr", .rb_ooxml_ns)
    if (!is.null(sect) && !inherits(sect, "xml_missing")) {
      pw <- suppressWarnings(as.numeric(wattr(sect, "w:pgSz", "w:w"))) / 1440
      ph <- suppressWarnings(as.numeric(wattr(sect, "w:pgSz", "w:h"))) / 1440
      if (is.finite(pw) && is.finite(ph)) {
        out$orientation <- if (pw > ph) "landscape" else "portrait"
        dims <- sort(c(pw, ph))
        gap <- vapply(.rb_page_sizes, function(s) sum(abs(sort(s) - dims)), numeric(1))
        out$size <- names(gap)[which.min(gap)]
      }
      side <- mean(suppressWarnings(as.numeric(c(wattr(sect, "w:pgMar", "w:left"), wattr(sect, "w:pgMar", "w:right")))) / 1440)
      if (is.finite(side)) out$margins <- c("narrow", "normal", "wide")[which.min(abs(c(0.5, 0.75, 1) - side))]
    }
  }

  if (is.na(heading_color) || identical(toupper(heading_color), toupper(dk1))) {
    # the second dark colour when it reads as a heading colour (dark, and not the text colour), else the accent
    heading_color <- if (.rb_luminance(dk2) < 0.3 && toupper(dk2) != toupper(dk1)) dk2 else accent1
  }
  out$accent <- accent1
  out$heading_color <- heading_color
  out$text_color <- dk1
  out$muted_color <- .rb_mix(.rb_mix(dk1, dk2, 0.25), if (.rb_luminance(lt1) > 0.5) lt1 else "#FFFFFF", 0.4)
  out$note_fill <- .rb_mix(accent1, "#FFFFFF", 0.9)
  out$note_border <- .rb_mix(accent1, "#FFFFFF", 0.65)
  if (!is.na(heading_font)) out$heading_font <- heading_font
  if (!is.na(body_font)) out$body_font <- body_font
  if (length(palette)) out$palette <- palette
  out$apply_palette <- TRUE
  for (k in c("accent", "heading_color", "text_color", "muted_color", "note_fill", "note_border")) out[[k]] <- tolower(out[[k]])
  out$palette <- tolower(out$palette)
  out$template_kind <- kind
  out
}

# ---- PowerPoint templates ------------------------------------------------------------------------------------------

# A PowerPoint template opened for a deck: list(x (the rpptx, its slides removed), layout, master) or NULL (no template,
# or one that cannot be opened: then with a warning). `size`: the deck's slide size (inches); a template of another size
# is scaled to it first (.rb_scale_template()), as PowerPoint's Slide Size > Scale does
.rb_deck_template <- function(path, size = NULL) {
  if (is.null(path) || !is.character(path) || !length(path) || !nzchar(path[1])) return(NULL)
  path <- path[1]
  fail <- function(why) {
    .ds_warn(c("!" = "The PowerPoint template could not be used; the deck is written without it.", "i" = why))
    NULL
  }
  if (!file.exists(path)) return(fail("The template file was not found."))
  kind <- .rb_office_kind(path)
  if (!identical(kind, "pptx")) return(fail("The template is not a PowerPoint file."))
  # a template (.potx) or a show (.ppsx) opened as a presentation
  dir <- tempfile("tpl_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = dir)
  types_path <- file.path(dir, "[Content_Types].xml")
  types <- readChar(types_path, file.info(types_path)$size, useBytes = TRUE)
  fixed <- gsub("presentationml\\.(template|slideshow)\\.main\\+xml", "presentationml.presentation.main+xml", types)
  fixed <- gsub("ms-powerpoint\\.(template|slideshow)\\.macroEnabled\\.main\\+xml", "ms-powerpoint.presentation.macroEnabled.main+xml", fixed)
  # a file without view settings (Google Slides writes none), which officer needs to write the deck
  props <- .rb_add_view_props(dir, fixed)
  changed <- !identical(props, types)
  fixed <- props
  if (!is.null(size) && .rb_scale_template(dir, size)) changed <- TRUE
  copy <- tempfile(fileext = ".pptx")
  if (changed) {
    writeChar(fixed, types_path, eos = NULL, useBytes = TRUE)
    files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    files <- c(files[files == "[Content_Types].xml"], files[files != "[Content_Types].xml"])
    zip::zip(copy, files = files, root = dir, mode = "mirror")
  } else {
    file.copy(path, copy)
  }
  x <- tryCatch(officer::read_pptx(copy), error = function(e) e)
  if (inherits(x, "error")) return(fail(conditionMessage(x)))
  while (length(x) > 0) x <- officer::remove_slide(x, index = 1)
  layouts <- officer::layout_summary(x)
  if (!nrow(layouts)) return(fail("The template has no slide layouts."))
  pick <- which(tolower(trimws(layouts$layout)) == "blank")
  if (!length(pick)) {
    # the layout with the fewest placeholders (the date, footer and slide number do not show on a slide)
    counts <- vapply(seq_len(nrow(layouts)), function(i) {
      p <- tryCatch(officer::layout_properties(x, layout = layouts$layout[i], master = layouts$master[i]), error = function(e) NULL)
      if (is.null(p)) return(Inf)
      sum(!p$type %in% c("dt", "ftr", "sldNum")) + 0.01 * nrow(p)
    }, numeric(1))
    pick <- which.min(counts)
  }
  list(x = x, layout = layouts$layout[pick[1]], master = layouts$master[pick[1]])
}

# An unzipped PowerPoint file scaled to a slide of `size` (inches), as PowerPoint's Slide Size > Scale does: the slide
# size, and in its masters and layouts every position and size (a:off, a:ext, a:chOff, a:chExt), line width and text
# size (sz, in hundredths of a point; text by the height's factor). The deck's slides are then placed on the builder's
# slide as they are in the editor. TRUE when it changed.
.rb_scale_template <- function(dir, size) {
  pres_path <- file.path(dir, "ppt", "presentation.xml")
  if (!file.exists(pres_path) || length(size) != 2 || !all(is.finite(size))) return(FALSE)
  pres <- readChar(pres_path, file.info(pres_path)$size, useBytes = TRUE)
  tag <- regmatches(pres, regexpr("<p:sldSz [^>]*>", pres))
  if (!length(tag)) return(FALSE)
  attr_of <- function(a) as.numeric(sub(sprintf('.* %s="([0-9]+)".*', a), "\\1", tag))
  from <- c(attr_of("cx"), attr_of("cy"))
  if (!all(is.finite(from)) || any(from <= 0)) return(FALSE)
  to <- round(size * 914400)
  fx <- to[1] / from[1]
  fy <- to[2] / from[2]
  if (abs(fx - 1) < 0.005 && abs(fy - 1) < 0.005) return(FALSE)
  new_tag <- sub(' cy="[0-9]+"', sprintf(' cy="%.0f"', to[2]), sub(' cx="[0-9]+"', sprintf(' cx="%.0f"', to[1]), tag))
  pres <- sub(tag, new_tag, pres, fixed = TRUE)
  writeChar(pres, pres_path, eos = NULL, useBytes = TRUE)
  # each number of an attribute in the given tags, times f
  scale_attrs <- function(xml, tags, attrs, f) {
    for (tag in tags) {
      m <- gregexpr(sprintf("<%s [^>]*>", tag), xml, perl = TRUE)
      regmatches(xml, m) <- lapply(regmatches(xml, m), function(nodes) vapply(nodes, function(node) {
        for (a in attrs) {
          at <- regexpr(sprintf('(?<= %s=")-?[0-9]+', a), node, perl = TRUE)
          if (at > 0) regmatches(node, at) <- sprintf("%.0f", as.numeric(regmatches(node, at)) * f)
        }
        node
      }, character(1)))
    }
    xml
  }
  parts <- c(list.files(file.path(dir, "ppt", "slideMasters"), pattern = "\\.xml$", full.names = TRUE),
             list.files(file.path(dir, "ppt", "slideLayouts"), pattern = "\\.xml$", full.names = TRUE))
  for (f in parts) {
    xml <- readChar(f, file.info(f)$size, useBytes = TRUE)
    xml <- scale_attrs(xml, c("a:off", "a:chOff"), "x", fx)
    xml <- scale_attrs(xml, c("a:off", "a:chOff"), "y", fy)
    xml <- scale_attrs(xml, c("a:ext", "a:chExt"), "cx", fx)
    xml <- scale_attrs(xml, c("a:ext", "a:chExt"), "cy", fy)
    xml <- scale_attrs(xml, "a:ln", "w", min(fx, fy))
    xml <- scale_attrs(xml, c("a:defRPr", "a:rPr", "a:endParaRPr"), "sz", fy)
    # text insets and indents (EMU)
    xml <- scale_attrs(xml, "a:bodyPr", c("lIns", "rIns"), fx)
    xml <- scale_attrs(xml, "a:bodyPr", c("tIns", "bIns"), fy)
    xml <- scale_attrs(xml, paste0("a:lvl", 1:9, "pPr"), c("marL", "indent"), fx)
    writeChar(xml, f, eos = NULL, useBytes = TRUE)
  }
  TRUE
}

# An unzipped PowerPoint file given view settings (ppt/viewProps.xml, its relationship and content type) when it has
# none; returns the content types, changed when the part was added
.rb_add_view_props <- function(dir, types) {
  path <- file.path(dir, "ppt", "viewProps.xml")
  if (file.exists(path)) return(types)
  writeChar(paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<p:viewPr xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ',
    'xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main">',
    '<p:normalViewPr><p:restoredLeft sz="15620"/><p:restoredTop sz="94660"/></p:normalViewPr>',
    '<p:gridSpacing cx="76200" cy="76200"/></p:viewPr>'), path, eos = NULL, useBytes = TRUE)
  rels_path <- file.path(dir, "ppt", "_rels", "presentation.xml.rels")
  if (file.exists(rels_path)) {
    rels <- readChar(rels_path, file.info(rels_path)$size, useBytes = TRUE)
    if (!grepl("relationships/viewProps\"", rels, fixed = TRUE)) {
      n <- max(c(0, suppressWarnings(as.numeric(regmatches(rels, gregexpr('(?<=Id="rId)[0-9]+', rels, perl = TRUE))[[1]]))), na.rm = TRUE) + 1
      rels <- sub("</Relationships>", sprintf(paste0('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/',
                                                     'relationships/viewProps" Target="viewProps.xml"/></Relationships>'), n), rels, fixed = TRUE)
      writeChar(rels, rels_path, eos = NULL, useBytes = TRUE)
    }
  }
  if (!grepl("/ppt/viewProps.xml", types, fixed = TRUE)) {
    types <- sub("</Types>", paste0('<Override PartName="/ppt/viewProps.xml" ',
                                    'ContentType="application/vnd.openxmlformats-officedocument.presentationml.viewProps+xml"/></Types>'),
                 types, fixed = TRUE)
  }
  types
}

# ---- Word templates ------------------------------------------------------------------------------------------------

# A Word template made ready for a document: a .docx copy of it (a .dotx opened as a document) with its body emptied,
# the styles the report writes with added where it has none, and a numbering part for lists. list(file, section) where
# `section` is the template's page (size and margins, inches) and whether it has its own header or footer; NULL when
# there is no template or it cannot be used (then with a warning).
.rb_docx_template <- function(path) {
  if (is.null(path) || !is.character(path) || !length(path) || !nzchar(path[1])) return(NULL)
  path <- path[1]
  fail <- function(why) {
    .ds_warn(c("!" = "The Word template could not be used; the document is written without it.", "i" = why))
    NULL
  }
  if (!file.exists(path)) return(fail("The template file was not found."))
  kind <- .rb_office_kind(path)
  if (!identical(kind, "docx")) return(fail("The template is not a Word file."))
  result <- tryCatch({
    dir <- tempfile("tpl_")
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    utils::unzip(path, exdir = dir)
    read <- function(p) { x <- readChar(p, file.info(p)$size, useBytes = TRUE); Encoding(x) <- "UTF-8"; x }
    write <- function(txt, p) writeChar(enc2utf8(txt), p, eos = NULL, useBytes = TRUE)
    types_path <- file.path(dir, "[Content_Types].xml")
    types <- read(types_path)
    types <- gsub("wordprocessingml\\.template\\.main\\+xml", "wordprocessingml.document.main+xml", types)
    types <- gsub("ms-word\\.template\\.macroEnabledTemplate\\.main\\+xml", "ms-word.document.macroEnabled.main+xml", types)

    own <- tempfile("own_")
    on.exit(unlink(own, recursive = TRUE), add = TRUE)
    utils::unzip(system.file("rmd", "report-template.docx", package = "datasuite.ui"), exdir = own)

    # the styles the report uses, from the package's template where the file has none of that name
    styles_path <- file.path(dir, "word", "styles.xml")
    if (file.exists(styles_path)) {
      theirs <- xml2::read_xml(styles_path)
      ours <- xml2::read_xml(file.path(own, "word", "styles.xml"))
      have_names <- tolower(xml2::xml_attr(xml2::xml_find_all(theirs, "//w:style/w:name", .rb_ooxml_ns), "w:val", ns = .rb_ooxml_ns))
      have_ids <- xml2::xml_attr(xml2::xml_find_all(theirs, "//w:style", .rb_ooxml_ns), "w:styleId", ns = .rb_ooxml_ns)
      root <- xml2::xml_root(theirs)
      for (s in xml2::xml_find_all(ours, "//w:style", .rb_ooxml_ns)) {
        nm <- tolower(xml2::xml_attr(xml2::xml_find_first(s, "w:name", .rb_ooxml_ns), "w:val", ns = .rb_ooxml_ns))
        if (is.na(nm) || nm %in% have_names) next
        id <- xml2::xml_attr(s, "w:styleId", ns = .rb_ooxml_ns)
        if (id %in% have_ids) xml2::xml_set_attr(s, "w:styleId", paste0(id, "Rb"))
        xml2::xml_add_child(root, s)
      }
      xml2::write_xml(theirs, styles_path)
    } else {
      file.copy(file.path(own, "word", "styles.xml"), styles_path)
      types <- sub("</Types>", '<Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/></Types>', types, fixed = TRUE)
    }
    rels_path <- file.path(dir, "word", "_rels", "document.xml.rels")
    rels <- read(rels_path)
    next_rel <- max(c(0, as.numeric(regmatches(rels, gregexpr('(?<=Id="rId)[0-9]+', rels, perl = TRUE))[[1]]))) + 1
    add_rel <- function(type, target) {
      rels <<- sub("</Relationships>", sprintf('<Relationship Id="rId%d" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/%s" Target="%s"/></Relationships>', next_rel, type, target), rels, fixed = TRUE)
      next_rel <<- next_rel + 1
    }
    if (!grepl("relationships/styles\"", rels, fixed = TRUE)) add_rel("styles", "styles.xml")
    # lists are numbered in numbering.xml
    if (!file.exists(file.path(dir, "word", "numbering.xml"))) {
      file.copy(file.path(own, "word", "numbering.xml"), file.path(dir, "word", "numbering.xml"))
      if (!grepl("/word/numbering.xml", types, fixed = TRUE)) {
        types <- sub("</Types>", '<Override PartName="/word/numbering.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"/></Types>', types, fixed = TRUE)
      }
      if (!grepl("relationships/numbering\"", rels, fixed = TRUE)) add_rel("numbering", "numbering.xml")
    }
    write(types, types_path)
    write(rels, rels_path)

    # the body emptied: one empty paragraph, then the template's own section (page, header, footer)
    doc_path <- file.path(dir, "word", "document.xml")
    doc <- read(doc_path)
    open <- regexpr("<w:body>", doc, fixed = TRUE)
    close <- regexpr("</w:body>", doc, fixed = TRUE)
    if (open < 0 || close < 0) stop("The file has no body.")
    inner <- substr(doc, open + nchar("<w:body>"), close - 1)
    sects <- gregexpr("(?s)<w:sectPr(?: [^>]*)?>.*?</w:sectPr>|<w:sectPr(?: [^>]*)?/>", inner, perl = TRUE)[[1]]
    sect <- if (sects[1] > 0) {
      k <- length(sects)
      substr(inner, sects[k], sects[k] + attr(sects, "match.length")[k] - 1)
    } else ""
    doc <- paste0(substr(doc, 1, open + nchar("<w:body>") - 1), "<w:p/>", sect, substr(doc, close, nchar(doc)))
    write(doc, doc_path)
    # different headers on even pages, but none given for them: every page gets the one header
    settings_path <- file.path(dir, "word", "settings.xml")
    if (file.exists(settings_path) && !grepl('<w:(header|footer)Reference [^>]*w:type="even"', sect)) {
      settings <- read(settings_path)
      write(gsub("<w:evenAndOddHeaders(?: [^>]*)?/>", "", settings, perl = TRUE), settings_path)
    }

    twips <- function(tag, attr) {
      v <- regmatches(sect, regexec(sprintf('<w:%s [^>]*w:%s="([0-9]+)"', tag, attr), sect))[[1]]
      if (length(v) == 2) as.numeric(v[2]) / 1440 else NA_real_
    }
    section <- list(
      width = twips("pgSz", "w"), height = twips("pgSz", "h"),
      landscape = grepl('w:orient="landscape"', sect, fixed = TRUE),
      top = twips("pgMar", "top"), bottom = twips("pgMar", "bottom"), left = twips("pgMar", "left"), right = twips("pgMar", "right"),
      header = twips("pgMar", "header"), footer = twips("pgMar", "footer"),
      header_footer = grepl("<w:(header|footer)Reference", sect)
    )

    out <- tempfile(fileext = ".docx")
    files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    files <- c(files[files == "[Content_Types].xml"], files[files != "[Content_Types].xml"])
    zip::zip(out, files = files, root = dir, mode = "mirror")
    officer::read_docx(out)
    list(file = out, section = section)
  }, error = function(e) e)
  if (inherits(result, "error")) return(fail(conditionMessage(result)))
  result
}
