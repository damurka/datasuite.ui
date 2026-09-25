# A report's design: its theme (fonts, colours, text sizes, chart palette), its page (size, orientation, margins, header
# and footer) and its cover page. The same values drive the builder's pages, the Word styles (.rb_docx_styles()) and the
# charts (.rb_theme_chart_options()), so what the builder shows and what the files contain agree.
#
# Design fields:
#   theme                        the preset it started from (report_themes())
#   accent, heading_color, text_color, muted_color, note_fill, note_border    colours
#   heading_font, body_font      fonts (report_fonts())
#   title_size, h1_size, h2_size, body_size, note_size, caption_size         points
#   line_spacing, paragraph_after   body text: line spacing (1 = single) and the space after a paragraph (points)
#   palette                      colours given, in order, to a chart's series; apply_palette = FALSE keeps the chart's own
#   size = "a4" | "letter" | "a3" | "chartbook" | "poster", orientation = "portrait" | "landscape", margins = "normal" | "narrow" | "wide"
#   cover, contents, page_numbers                                           TRUE / FALSE
#   header, footer               running text; may hold fields such as {country} (report_fields())
#   slide_size = "16:9" | "4:3"  a slide deck's slides (report_slide_size(); documents ignore it)
#   template                     NULL, or the path of a PowerPoint (deck) or Word (document) file the export is written
#                                on top of (see R/report-template.R and report_theme_from_file())
#
# Cover fields (project$cover):
#   layout = "band" | "full" | "photo" | "minimal", kicker, title (empty = the report's name), subtitle,
#   show_flag, logos (list of list(src = <data URL>, name)), photo (data URL), editors (list of list(name, role)),
#   date_mode = "month" | "today" | "custom", date (the custom text), reference

#' The fonts a report can use
#'
#' Typefaces made for print that come with Microsoft Office, so the page in the editor, the charts, the Word file and the
#' PDF all use the same letters, on any computer that opens the file: serif faces for reading text, then sans serif faces
#' for charts, tables and headings. Only those installed where the charts are drawn are given (all of them when that
#' cannot be told).
#' @param group `"all"`, `"serif"` or `"sans"`.
#' @return A character vector.
#' @export
report_fonts <- function(group = c("all", "serif", "sans")) {
  group <- match.arg(group)
  serif <- c("Cambria", "Georgia", "Garamond", "Palatino Linotype", "Book Antiqua", "Constantia", "Times New Roman")
  sans <- c("Calibri", "Arial", "Aptos", "Corbel", "Candara", "Franklin Gothic Book", "Gill Sans MT")
  fonts <- switch(group, serif = serif, sans = sans, c(serif, sans))
  have <- if (requireNamespace("systemfonts", quietly = TRUE)) {
    tryCatch(unique(systemfonts::system_fonts()$family), error = function(e) character())
  } else {
    character()
  }
  if (length(have) && any(fonts %in% have)) fonts[fonts %in% have] else fonts
}

#' The report themes
#'
#' The ready-made looks a report can start from. Each is a complete design (see [report_default_design()]).
#' @return A named list of designs; each also has `name`.
#' @export
report_themes <- function() {
  base <- list(
    size = "a4", orientation = "portrait", margins = "normal", cover = TRUE, contents = TRUE, page_numbers = TRUE,
    header = "{report_title}", footer = "",
    title_size = 30, h1_size = 18, h2_size = 13, body_size = 10.5, note_size = 9.5, caption_size = 8.5,
    line_spacing = 1.15, paragraph_after = 6,
    apply_palette = TRUE, slide_size = "16:9"
  )
  theme <- function(id, name, ...) c(list(theme = id, name = name), utils::modifyList(base, list(...)))
  # the app's themes first (report_register()), each over the same base
  own <- lapply(.ds_registered("themes", list()), function(th) utils::modifyList(base, th))
  c(own, list(
    ministry = theme(
      "ministry", "Ministry of Health",
      accent = "#1b6b45", heading_color = "#1b6b45", text_color = "#1f2328", muted_color = "#56606a",
      note_fill = "#eaf4ee", note_border = "#c5e0d0", heading_font = "Arial", body_font = "Arial",
      palette = c("#1b6b45", "#e0a526", "#1c4f9c", "#b3412f", "#5b8c3a", "#6b7780")
    ),
    minimal = theme(
      "minimal", "Minimal",
      accent = "#1f2328", heading_color = "#1f2328", text_color = "#2b3138", muted_color = "#6b7178",
      note_fill = "#f3f4f6", note_border = "#d7dce0", heading_font = "Calibri", body_font = "Calibri",
      palette = c("#2b3138", "#7a8691", "#1c4f9c", "#b9c0c7", "#a8480f", "#4f5a64"),
      title_size = 28, h1_size = 16, h2_size = 12
    ),
    formal = theme(
      "formal", "Formal",
      accent = "#1c3f6e", heading_color = "#1c3f6e", text_color = "#1f2328", muted_color = "#5c6670",
      note_fill = "#eef3fb", note_border = "#c7d6ee", heading_font = "Cambria", body_font = "Cambria",
      palette = c("#1c3f6e", "#8a6d1d", "#7d3f40", "#3f7a8c", "#6b4c9a", "#7a8691"),
      body_size = 11
    )
  ))
}

#' The default report design
#' @return A design (see the top of `R/report-theme.R` for its fields).
#' @export
report_default_design <- function() {
  themes <- report_themes()
  d <- themes[[.ds_registered("default_theme", "") %||% ""]] %||% themes[[1]]
  d$name <- NULL
  d
}

#' The default cover page
#' @return A cover (see the top of `R/report-theme.R` for its fields).
#' @export
report_default_cover <- function() {
  cover <- list(layout = "band", kicker = "", title = "", subtitle = "", show_flag = TRUE,
                logos = list(), photo = NULL, editors = list(), date_mode = "month", date = "", reference = "")
  utils::modifyList(cover, .ds_registered("cover", list()))
}

# A project's design with every field filled in; designs saved before themes existed had `font` = "serif" / "sans"
.rb_design <- function(design) {
  design <- design %||% list()
  if (is.null(design$heading_font) && identical(design$font, "sans")) design$heading_font <- "Calibri"
  if (is.null(design$heading_color) && !is.null(design$accent)) design$heading_color <- design$accent
  design$font <- NULL
  out <- .rb_merge(report_default_design(), design)
  out$palette <- unlist(out$palette) %||% report_default_design()$palette
  for (f in c("title_size", "h1_size", "h2_size", "body_size", "note_size", "caption_size", "line_spacing", "paragraph_after")) {
    out[[f]] <- as.numeric(out[[f]])
  }
  out
}

.rb_cover <- function(cover) .rb_merge(report_default_cover(), cover)

#' The size of a slide deck's slides, in inches
#'
#' @param design A design; its `slide_size` is `"16:9"` (13.333 x 7.5 in, the default) or `"4:3"` (10 x 7.5 in).
#' @return `c(width, height)` in inches.
#' @export
report_slide_size <- function(design = NULL) {
  if (identical(design$slide_size, "4:3")) c(10, 7.5) else c(13.333, 7.5)
}

# Field by field, `b` over `a` (modifyList() would merge lists of editors or logos item by item, and drop unnamed ones)
.rb_merge <- function(a, b) {
  for (name in names(b %||% list())) if (!is.null(b[[name]])) a[[name]] <- b[[name]]
  a
}

# ---- page geometry -------------------------------------------------------------------------------------------------

# Paper sizes, portrait, in inches: the usual ones, and the two the Countdown templates were designed on (the synthesis
# chartbook and the sub-national one-page poster, used in landscape)
.rb_page_sizes <- list(a4 = c(8.27, 11.69), letter = c(8.5, 11), a3 = c(11.69, 16.54), chartbook = c(13.93, 22), poster = c(17, 22))

#' The page of a report, in inches
#'
#' Page size, margins and the width and height left for content. Shared by the builder and the exported files.
#' @param design A design.
#' @return A list: `width`, `height`, `top`, `bottom`, `left`, `right`, `text_width`, `text_height`.
#' @export
report_page <- function(design = report_default_design()) {
  design <- .rb_design(design)
  dims <- .rb_page_sizes[[design$size %||% "a4"]] %||% .rb_page_sizes$a4
  w <- dims[[1]]
  h <- dims[[2]]
  if (identical(design$orientation, "landscape")) { tmp <- w; w <- h; h <- tmp }
  m <- switch(design$margins %||% "normal",
              narrow = c(top = 0.6, bottom = 0.6, side = 0.5),
              wide = c(top = 1, bottom = 1, side = 1),
              c(top = 0.8, bottom = 0.75, side = 0.75))
  list(width = w, height = h, top = unname(m["top"]), bottom = unname(m["bottom"]), left = unname(m["side"]),
       right = unname(m["side"]), text_width = w - 2 * unname(m["side"]), text_height = h - unname(m["top"]) - unname(m["bottom"]) - 0.6)
}

# ---- the theme in charts -------------------------------------------------------------------------------------------

# Chart options every chart of the report starts from: the theme's font and text colour
.rb_theme_chart_options <- function(design) {
  cd_chart_options(font_family = design$body_font, text_color = design$text_color)
}

# The theme's palette, given in order to the entries of a chart's colour / fill legend (a chart whose colours are a
# continuous scale, like a map, keeps its own)
.rb_palette_options <- function(p, design) {
  if (!isTRUE(design$apply_palette) || !length(design$palette) || !inherits(p, "ggplot")) return(NULL)
  built <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
  if (is.null(built)) return(NULL)
  for (scale in built$plot$scales$scales) {
    if (!any(c("colour", "fill") %in% scale$aesthetics) || !isTRUE(scale$is_discrete())) next
    limits <- scale$get_limits()
    limits <- as.character(limits[!is.na(limits)])
    if (!length(limits) || length(limits) > 12) return(NULL)
    colours <- rep_len(design$palette, length(limits))
    return(cd_chart_options(colors = stats::setNames(colours, limits)))
  }
  NULL
}

# The chart options a block was given in the builder (the chart customize panel's values)
.rb_block_options <- function(options) {
  if (is.null(options) || !length(options)) return(NULL)
  keep <- options[intersect(names(options), datasuite.ui::chart_option_fields())]
  keep <- Filter(function(v) !is.null(v) && !identical(v, ""), keep)
  keep <- lapply(keep, function(v) {
    if (is.list(v) && !is.null(names(v))) return(unlist(v))
    if (is.list(v)) return(vapply(v, function(z) if (is.null(z)) NA_real_ else as.numeric(z), numeric(1)))
    v
  })
  tryCatch(do.call(cd_chart_options, keep), error = function(e) NULL)
}

# ---- the theme in Word ---------------------------------------------------------------------------------------------

# Heading sizes (pt), as the editor's headingSize(): H1 and H2 from the theme, H3 between H2 and body text, H4 to H6 at
# body size (H5 italic, H6 in the muted colour)
.rb_heading_size <- function(level, design) {
  level <- as.integer(level %||% 1)
  if (level <= 1) return(design$h1_size)
  if (level == 2) return(design$h2_size)
  if (level == 3) return(round(design$h2_size + design$body_size) / 2)
  design$body_size
}

# Point sizes are kept to half points, as Word stores them
.rb_pt <- function(x) round(as.numeric(x) * 2) / 2

# The template's paragraph styles, redefined from the design: the file carries the theme as Word styles, so a heading
# typed in Word later looks like the others. The styles are edited in place (officer's docx_set_paragraph_style() would
# drop what makes a heading a heading, its outline level, which the contents page is built from).
.rb_docx_styles <- function(doc, design) {
  path <- file.path(doc$package_dir, "word", "styles.xml")
  xml <- xml2::read_xml(path)
  ns <- xml2::xml_ns(xml)
  set <- function(id, font, size, color, bold = FALSE, italic = FALSE, keep_next = FALSE, before = 0, after = 6,
                  line = 1.15, border = NULL, fill = NULL, box = NULL, align = NULL) {
    node <- xml2::xml_find_first(xml, sprintf("//w:style[@w:styleId='%s']", id), ns)
    if (inherits(node, "xml_missing")) {
      # a template's style found by its name (Word translates the ids)
      label <- c(Heading1 = "heading 1", Heading2 = "heading 2", Heading3 = "heading 3", Heading4 = "heading 4", Heading5 = "heading 5",
                 Heading6 = "heading 6", NotesBox = "notes box", TOCHeading = "toc heading")[id]
      if (is.na(label)) label <- tolower(id)
      node <- xml2::xml_find_first(xml, sprintf("//w:style[w:name[translate(@w:val, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')='%s']]", label), ns)
    }
    if (inherits(node, "xml_missing")) return(invisible())
    outline <- xml2::xml_attr(xml2::xml_find_first(node, "w:pPr/w:outlineLvl", ns), "val")
    for (old in xml2::xml_find_all(node, "w:pPr|w:rPr", ns)) xml2::xml_remove(old)
    twips <- function(pt) round(pt * 20)
    edge <- function(side, b) sprintf('<w:%s w:val="single" w:sz="%d" w:space="4" w:color="%s"/>', side, round(b$width * 8), sub("#", "", b$color))
    ppr <- paste0(
      if (keep_next) "<w:keepNext/>" else "",
      if (!is.null(border) || !is.null(box)) paste0("<w:pBdr>",
        if (!is.null(box)) paste0(edge("top", box), edge("left", box), edge("bottom", box), edge("right", box)) else edge("bottom", border),
        "</w:pBdr>") else "",
      if (!is.null(fill)) sprintf('<w:shd w:val="clear" w:color="auto" w:fill="%s"/>', sub("#", "", fill)) else "",
      sprintf('<w:spacing w:before="%d" w:after="%d" w:line="%d" w:lineRule="auto"/>', twips(before), twips(after), round(line * 240)),
      if (!is.null(box)) '<w:ind w:left="113" w:right="113"/>' else "",
      if (!is.null(align)) sprintf('<w:jc w:val="%s"/>', align) else "",
      if (!is.na(outline)) sprintf('<w:outlineLvl w:val="%s"/>', outline) else ""
    )
    rpr <- paste0(
      sprintf('<w:rFonts w:ascii="%1$s" w:hAnsi="%1$s" w:cs="%1$s" w:eastAsia="%1$s"/>', font),
      if (bold) "<w:b/><w:bCs/>" else '<w:b w:val="0"/><w:bCs w:val="0"/>',
      if (italic) "<w:i/><w:iCs/>" else '<w:i w:val="0"/><w:iCs w:val="0"/>',
      sprintf('<w:color w:val="%s"/>', sub("#", "", color)),
      sprintf('<w:sz w:val="%1$d"/><w:szCs w:val="%1$d"/>', round(.rb_pt(size) * 2))
    )
    wrap <- function(tag, inner) xml2::read_xml(sprintf('<w:%1$s xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">%2$s</w:%1$s>', tag, inner))
    xml2::xml_add_child(node, wrap("pPr", ppr))
    xml2::xml_add_child(node, wrap("rPr", rpr))
    invisible()
  }
  set("Normal", design$body_font, design$body_size, design$text_color, after = design$paragraph_after, line = design$line_spacing)
  set("Heading1", design$heading_font, design$h1_size, design$heading_color, bold = TRUE, keep_next = TRUE, before = 14, after = 6,
      line = 1, border = list(color = design$accent, width = 1.5))
  set("Heading2", design$heading_font, design$h2_size, design$text_color, bold = TRUE, keep_next = TRUE, before = 10, after = 4, line = 1)
  set("Heading3", design$heading_font, .rb_heading_size(3, design), design$text_color, bold = TRUE, keep_next = TRUE, before = 8, after = 3, line = 1)
  set("Heading4", design$heading_font, .rb_heading_size(4, design), design$text_color, bold = TRUE, keep_next = TRUE, before = 6, after = 2, line = 1)
  set("Heading5", design$heading_font, .rb_heading_size(5, design), design$text_color, bold = TRUE, italic = TRUE, keep_next = TRUE, before = 6, after = 2, line = 1)
  set("Heading6", design$heading_font, .rb_heading_size(6, design), design$muted_color, bold = TRUE, keep_next = TRUE, before = 6, after = 2, line = 1)
  set("NotesBox", design$body_font, design$note_size, design$text_color, after = 6, line = 1.1, fill = design$note_fill,
      box = list(color = design$note_border, width = 1))
  set("Caption", design$body_font, design$caption_size, design$muted_color, italic = TRUE, before = 2, after = 8, align = "center")
  set("TOCHeading", design$heading_font, design$h1_size, design$heading_color, bold = TRUE, after = 10, line = 1)
  xml2::write_xml(xml, path)
  doc
}

#' Two designs (or covers) merged
#'
#' Field by field, `b` over `a`: lists such as a cover's editors or logos are replaced whole, not merged item by item.
#' @param a,b Two designs or covers.
#' @return The merged list.
#' @export
report_merge <- function(a, b) .rb_merge(a, b)
