# Text in a report: fields and formatted text.
#
# Fields are words in braces, such as {country} or {anc4_latest}, typed anywhere in a report's text, header, footer or
# cover. They are filled in from the dataset when the report is shown or exported (report_fields()), so a report reused
# with next year's data says the right thing.
#
# Paragraphs and notes hold a small subset of HTML, which is what the builder's editor produces: <b>, <i>, <u>, <s>,
# <sub>, <sup>, <span style="color: ...; background-color: ...; font-family: ...; font-size: ...pt">, and <br> between
# lines. A block's own paragraph settings (indent_left / indent_right in cm, space_before / space_after in pt, line: the
# line spacing) override the theme's. A block with `list = "bullet"` or `"number"` is a list with one item
# per line. .rb_lines() reads it into lines of runs, which Word (.rb_docx_fpars()) and HTML (.rb_html_text()) are written from.

#' The fields a report's text can contain
#'
#' The report's own (title, date, editors, region, years, the chart below), then those the app registered
#' ([report_register()], `fields`).
#' @return A list of `list(key, group, label)`; `key` is the word written in braces.
#' @export
report_field_catalog <- function() {
  general <- list(
    list(key = "region", group = "report", label = "Region of the report"),
    list(key = "report_title", group = "report", label = "Report title"),
    list(key = "report_date", group = "report", label = "Report date"),
    list(key = "editors", group = "report", label = "Editors"),
    list(key = "reference", group = "report", label = "Reference"),
    list(key = "first_year", group = "data", label = "First year of data"),
    list(key = "latest_year", group = "data", label = "Latest year of data"),
    list(key = "chart_indicator", group = "chart", label = "Indicator of the chart below"),
    list(key = "chart_indicators", group = "chart", label = "Indicators of the charts below (to the next heading)"),
    list(key = "chart_year", group = "chart", label = "Year of the chart below")
  )
  c(general, .ds_registered("fields", list()))
}

# Month names in the app's languages (the date on a report is written in the report's language, whatever the locale)
.rb_months <- list(
  en = month.name,
  fr = c("janvier", "f\u00e9vrier", "mars", "avril", "mai", "juin", "juillet", "ao\u00fbt", "septembre", "octobre", "novembre",
         "d\u00e9cembre"),
  pt = c("janeiro", "fevereiro", "mar\u00e7o", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
)

#' The values of a report's fields
#'
#' @param context A report context ([report_context()]), or what [as_report_context()] turns into one.
#' @param project The report (`name`, `cover`).
#' @param date The date the report is dated.
#' @param lang The report's language (`"en"`, `"fr"`, `"pt"`): the date is written in it.
#' @return A named list of strings, one per field in [report_field_catalog()] that has a value.
#' @export
report_fields <- function(context, project, date = Sys.Date(), lang = "en") {
  context <- as_report_context(context)
  cover <- .rb_cover(project$cover)
  editors <- vapply(cover$editors %||% list(), function(e) e$name %||% "", character(1))
  editors <- editors[nzchar(editors)]
  month <- .rb_months[[if (lang %in% names(.rb_months)) lang else "en"]][as.integer(format(date, "%m"))]
  day <- as.integer(format(date, "%d"))
  year <- format(date, "%Y")
  date_text <- switch(cover$date_mode %||% "month",
                      today = if (lang == "pt") paste(day, "de", month, "de", year) else paste(day, month, year),
                      custom = cover$date %||% "",
                      if (lang == "pt") paste(month, "de", year) else paste(month, year))
  out <- list(
    report_title = project$name %||% "",
    report_date = date_text,
    editors = paste(editors, collapse = ", "),
    reference = cover$reference %||% "",
    # a report without a region is a national one
    region = if (is.character(project$region) && length(project$region) == 1 && nzchar(project$region)) project$region else switch(lang, fr = "National", pt = "Nacional", "National")
  )
  years <- tryCatch(context$years(), error = function(e) NULL)
  if (length(years)) {
    out$first_year <- as.character(min(years))
    out$latest_year <- as.character(max(years))
  }
  # the app's own fields (e.g. the country, values from the data)
  own <- tryCatch(context$fields(project, lang), error = function(e) NULL)
  for (k in names(own)) out[[k]] <- own[[k]]
  Filter(function(v) is.character(v) && length(v) == 1, out)
}

# Put the fields' values in `text`. `escape`: the text is HTML, so the values are escaped. An unknown field is left as typed.
#' Fill the fields that describe the chart below a text
#'
#' `{chart_indicator}` and `{chart_year}` in a text, heading or caption are the indicator (its name, in the report's
#' language) and the year of the first chart or table after it in the report, so a title above a chart follows the
#' chart when its indicator or year is changed. `{chart_indicators}` names the indicators of all the charts and tables
#' after it up to the next heading ("ANC 4 Visits and Measles 1"), for a heading above charts side by side. A text with
#' no chart after it keeps the field as typed.
#'
#' @param blocks The report's blocks, in order.
#' @param context A report context ([report_context()]), or what [as_report_context()] turns into one.
#' @param i18n The report's translations (`NULL`: English).
#' @return The blocks, their texts filled.
#' @export
report_chart_fields <- function(blocks, context = NULL, i18n = NULL) {
  if (!length(blocks)) return(blocks)
  years <- tryCatch(if (!is.null(context)) as_report_context(context)$years(), error = function(e) NULL)
  latest <- if (length(years)) as.character(max(years)) else ""
  is_data <- vapply(blocks, function(b) isTRUE(b$type %in% c("chart", "table")), logical(1))
  is_heading <- vapply(blocks, function(b) identical(b$type, "heading"), logical(1))
  for (i in seq_along(blocks)) {
    b <- blocks[[i]]
    texts <- intersect(c("text", "caption", "title"), names(b))
    texts <- texts[vapply(texts, function(k) is.character(b[[k]]) && any(grepl("{chart_", b[[k]], fixed = TRUE)), logical(1))]
    if (!length(texts)) next
    # a chart's own title means the chart itself; a text means the first chart after it
    j <- if (isTRUE(is_data[i])) i else utils::head(which(is_data & seq_along(blocks) > i), 1)
    if (!length(j)) next
    target <- blocks[[j]]
    # the charts up to the next heading (a chart's own title: the chart alone)
    group <- if (isTRUE(is_data[i])) list(target) else {
      stop_at <- utils::head(which(is_heading & seq_along(blocks) > i), 1)
      last <- if (length(stop_at)) stop_at - 1 else length(blocks)
      blocks[which(is_data & seq_along(blocks) > i & seq_along(blocks) <= last)]
    }
    names_ <- unique(unlist(lapply(group, function(g) if (is.character(g$indicator) && nzchar(g$indicator)) .ds_indicator_name(i18n, g$indicator))))
    values <- list(
      chart_indicators = if (length(names_)) .rb_join_and(names_, i18n),
      chart_indicator = if (!is.null(target$indicator) && nzchar(target$indicator)) .ds_indicator_name(i18n, target$indicator),
      chart_year = if (!is.null(target$year) && !is.na(suppressWarnings(as.integer(target$year)))) as.character(target$year) else latest
    )
    values <- Filter(function(v) !is.null(v) && nzchar(v), values)
    for (k in texts) blocks[[i]][[k]] <- .rb_fill(b[[k]], values)
  }
  blocks
}

# Names as a list in the report's language: "A", "A and B", "A, B and C"
.rb_join_and <- function(x, i18n = NULL) {
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(x[-length(x)], collapse = ", "), .rb_t(i18n, "lbl_rb_and", "and"), x[length(x)])
}

.rb_fill <- function(text, fields, escape = FALSE) {
  text <- text %||% ""
  if (!length(fields) || !grepl("{", text, fixed = TRUE)) return(text)
  hits <- regmatches(text, gregexpr("\\{[a-z0-9_]+\\}", text))[[1]]
  for (h in unique(hits)) {
    key <- substr(h, 2, nchar(h) - 1)
    value <- fields[[key]]
    if (is.null(value)) next
    if (escape) value <- htmltools::htmlEscape(value)
    text <- gsub(h, value, text, fixed = TRUE)
  }
  text
}

# ---- formatted text ------------------------------------------------------------------------------------------------

.rb_css_color <- function(style) {
  if (is.null(style) || is.na(style)) return(NA_character_)
  m <- regmatches(style, regexec("(?:^|;)\\s*color\\s*:\\s*([^;]+)", style, perl = TRUE))[[1]]
  if (length(m) < 2) return(NA_character_)
  .rb_hex(trimws(m[[2]]))
}

# One property of a CSS style attribute ("" or NA when it is not there)
.rb_css_value <- function(style, prop) {
  if (is.null(style) || is.na(style)) return(NA_character_)
  m <- regmatches(style, regexec(paste0("(?:^|;)\\s*", prop, "\\s*:\\s*([^;]+)"), style, perl = TRUE))[[1]]
  if (length(m) < 2) NA_character_ else trimws(m[[2]])
}

# A CSS font size in points (12pt, 16px)
.rb_css_size <- function(x) {
  if (is.na(x)) return(NA_real_)
  n <- suppressWarnings(as.numeric(sub("^([0-9.]+).*$", "\\1", x)))
  if (is.na(n)) return(NA_real_)
  if (grepl("px$", x)) n <- n * 0.75
  round(n * 2) / 2
}

.rb_hex <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_character_)
  rgb <- regmatches(x, regexec("rgba?\\(\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)", x))[[1]]
  if (length(rgb) == 4) return(grDevices::rgb(as.numeric(rgb[2]), as.numeric(rgb[3]), as.numeric(rgb[4]), maxColorValue = 255))
  ok <- tryCatch(grDevices::col2rgb(x), error = function(e) NULL)
  if (is.null(ok)) NA_character_ else grDevices::rgb(t(ok), maxColorValue = 255)
}

# Text (HTML subset) -> lines, each a list of runs: list(text, bold, italic, underline, color). A line that is a list
# item has the attributes "list" ("bullet" / "number"), "level" (1 = top) and "num" (its number in its list); a further
# paragraph of an item has "level" only. A line of a paragraph aligned in its own style (<p style="text-align: ...">)
# has "align", and a line of a heading (<h1> to <h6>) has "heading" (its level). `keep_empty`: an empty paragraph
# (<p></p>) is kept as an empty line (slides), instead of being dropped.
.rb_lines <- function(html, keep_empty = FALSE) {
  html <- html %||% ""
  if (!grepl("<", html, fixed = TRUE)) {
    # plain text (older reports, headings): a blank line or a line break separates lines
    lines <- strsplit(gsub("\r", "", html), "\n", fixed = TRUE)[[1]]
    if (!length(lines)) lines <- ""
    return(lapply(lines, function(l) list(list(text = l, bold = FALSE, italic = FALSE, underline = FALSE, color = NA_character_))))
  }
  doc <- xml2::read_html(paste0("<html><body><div id=\"rb\">", html, "</div></body></html>"), encoding = "UTF-8")
  root <- xml2::xml_find_first(doc, "//div[@id='rb']")
  lines <- list()
  current <- list()
  # the lists the text is in (kind and count of each), and the item whose first line comes next
  stack <- list()
  item <- NULL
  # the paragraph the text is in: its own alignment and heading level
  para <- list()
  flush <- function(force = FALSE) {
    if (length(current) || force) {
      line <- current
      if (!is.null(para$align)) attr(line, "align") <- para$align
      if (!is.null(para$heading)) attr(line, "heading") <- para$heading
      if (!is.null(item)) {
        attr(line, "list") <- item$kind
        attr(line, "level") <- item$level
        attr(line, "num") <- item$num
        item <<- NULL
      } else if (length(stack)) {
        attr(line, "level") <- length(stack)
      }
      lines[[length(lines) + 1]] <<- line
    }
    current <<- list()
  }
  walk <- function(node, fmt) {
    for (child in xml2::xml_contents(node)) {
      type <- xml2::xml_type(child)
      if (type == "text") {
        txt <- xml2::xml_text(child)
        txt <- gsub("[\r\n\t]+", " ", txt)
        if (nzchar(txt)) current[[length(current) + 1]] <<- c(list(text = gsub("\u00a0", " ", txt)), fmt)
        next
      }
      if (type != "element") next
      name <- tolower(xml2::xml_name(child))
      f <- fmt
      if (name %in% c("b", "strong")) f$bold <- TRUE
      if (name %in% c("i", "em")) f$italic <- TRUE
      if (name == "u") f$underline <- TRUE
      if (name %in% c("s", "strike", "del")) f$strike <- TRUE
      if (name == "sub") f$valign <- "subscript"
      if (name == "sup") f$valign <- "superscript"
      col <- .rb_css_color(xml2::xml_attr(child, "style"))
      if (is.na(col) && name == "font") col <- .rb_hex(xml2::xml_attr(child, "color"))
      if (!is.na(col)) f$color <- col
      style <- xml2::xml_attr(child, "style") %||% ""
      if (is.na(style)) style <- ""
      if (grepl("font-weight\\s*:\\s*(bold|[6-9]00)", style)) f$bold <- TRUE
      if (grepl("font-style\\s*:\\s*italic", style)) f$italic <- TRUE
      if (grepl("text-decoration[^;]*underline", style)) f$underline <- TRUE
      if (grepl("text-decoration[^;]*line-through", style)) f$strike <- TRUE
      if (grepl("vertical-align\\s*:\\s*sub", style)) f$valign <- "subscript"
      if (grepl("vertical-align\\s*:\\s*super", style)) f$valign <- "superscript"
      family <- .rb_css_value(style, "font-family")
      if (is.na(family) && name == "font") family <- xml2::xml_attr(child, "face")
      if (!is.na(family) && nzchar(family)) f$family <- trimws(gsub("[\"']", "", strsplit(family, ",", fixed = TRUE)[[1]][1]))
      size <- .rb_css_size(.rb_css_value(style, "font-size"))
      if (!is.na(size)) f$size <- size
      if (name == "a") {
        href <- xml2::xml_attr(child, "href")
        if (!is.na(href) && nzchar(href)) f$href <- href
      }
      mark <- .rb_css_value(style, "background-color")
      if (!is.na(mark)) f$highlight <- .rb_hex(mark)
      if (name == "br") {
        flush(force = TRUE)
      } else if (name %in% c("ul", "ol")) {
        if (length(current)) flush()
        stack[[length(stack) + 1]] <<- list(kind = if (name == "ol") "number" else "bullet", n = 0)
        walk(child, f)
        if (length(current)) flush()
        stack[[length(stack)]] <<- NULL
      } else if (name == "li" && length(stack)) {
        if (length(current)) flush()
        k <- length(stack)
        stack[[k]]$n <<- stack[[k]]$n + 1
        item <<- list(kind = stack[[k]]$kind, level = k, num = stack[[k]]$n)
        walk(child, f)
        if (length(current)) flush()
        item <<- NULL
      } else if (name %in% c("div", "p", "li", "h1", "h2", "h3", "h4", "h5", "h6")) {
        if (length(current)) flush()
        old <- para
        align <- tolower(.rb_css_value(style, "text-align"))
        if (!is.na(align) && align %in% c("left", "center", "right", "justify")) para$align <<- align
        if (grepl("^h[1-6]$", name)) para$heading <<- as.integer(substr(name, 2, 2))
        n_before <- length(lines)
        walk(child, f)
        if (length(current)) flush()
        # an empty paragraph is a blank line
        else if (keep_empty && name %in% c("p", "h1", "h2", "h3", "h4", "h5", "h6") && length(lines) == n_before) flush(force = TRUE)
        para <<- old
      } else {
        walk(child, f)
      }
    }
  }
  walk(root, list(bold = FALSE, italic = FALSE, underline = FALSE, strike = FALSE, valign = "baseline", color = NA_character_,
                  family = NA_character_, size = NA_real_, highlight = NA_character_, href = NA_character_))
  if (length(current)) flush()
  # a trailing <br> leaves an empty last line
  while (length(lines) > 1 && !length(lines[[length(lines)]])) lines[[length(lines)]] <- NULL
  if (!length(lines)) lines <- list(list())
  lines
}

.rb_plain <- function(html) {
  paste(vapply(.rb_lines(html), function(l) paste(vapply(l, function(r) r$text, character(1)), collapse = ""), character(1)), collapse = "\n")
}

# ---- Word ----------------------------------------------------------------------------------------------------------

# The marker of a list item, by level as the editor draws them: bullet, circle, square; 1. a. i.
.rb_marker <- function(kind, level, num) {
  k <- ((as.integer(level) - 1) %% 3) + 1
  if (identical(kind, "number")) {
    n <- as.integer(num)
    return(paste0(switch(k, n, letters[((n - 1) %% 26) + 1], tolower(as.character(utils::as.roman(n)))), "."))
  }
  c("\u2022", "\u25e6", "\u25aa")[k]
}

.rb_ftext <- function(run, text_color = "#2b3138") {
  given <- function(x) !is.null(x) && length(x) == 1 && !is.na(x)
  # a link: blue and underlined unless formatted otherwise
  link <- given(run$href) && grepl("^(https?://|mailto:)", run$href)
  if (link) {
    if (!given(run$color)) run$color <- "#1c4f9c"
    run$underline <- TRUE
  }
  # Word shades a run only together with a text colour
  if (given(run$highlight) && !given(run$color)) run$color <- text_color
  prop <- officer::fp_text_lite(
    bold = if (isTRUE(run$bold)) TRUE else NA,
    italic = if (isTRUE(run$italic)) TRUE else NA,
    underlined = if (isTRUE(run$underline)) TRUE else NA,
    strike = if (isTRUE(run$strike)) TRUE else NA,
    color = if (given(run$color)) run$color else NA,
    font.family = if (given(run$family)) run$family else NA,
    hansi.family = if (given(run$family)) run$family else NA,
    cs.family = if (given(run$family)) run$family else NA,
    font.size = if (given(run$size)) run$size else NA,
    shading.color = if (given(run$highlight)) run$highlight else NA,
    vertical.align = if (given(run$valign)) run$valign else "baseline"
  )
  if (link) return(officer::hyperlink_ftext(run$text, prop, run$href))
  officer::ftext(run$text, prop)
}

# ---- Word numbering ------------------------------------------------------------------------------------------------
# A list item's paragraph starts with a token naming its list, level and kind; .rb_docx_numbering() turns the tokens into
# Word numbering (each list its own, so it starts at 1; each level with its own format), which Word keeps renumbering
# when the file is edited.
.rb_lists <- new.env(parent = emptyenv())

.rb_num_token <- function(kind, level, new_list) {
  if (isTRUE(new_list) || is.null(.rb_lists$id)) {
    .rb_lists$n <- (.rb_lists$n %||% 0) + 1
    .rb_lists$id <- .rb_lists$n
  }
  officer::ftext(sprintf("@@RBN:%d:%d:%s@@", .rb_lists$id, max(1L, as.integer(level)), if (identical(kind, "number")) "number" else "bullet"))
}

# One level of a list's numbering: 1. a. i. or bullet, circle, square, then again from the fourth level
.rb_num_level <- function(i, kind) {
  k <- (i %% 3) + 1
  left <- 360 * (i + 1)
  if (identical(kind, "number")) {
    fmt <- c("decimal", "lowerLetter", "lowerRoman")[k]
    text <- sprintf("%%%d.", i + 1)
  } else {
    fmt <- "bullet"
    text <- c("\u2022", "\u25e6", "\u25aa")[k]
  }
  sprintf(paste0('<w:lvl w:ilvl="%d"><w:start w:val="1"/><w:numFmt w:val="%s"/><w:lvlText w:val="%s"/><w:lvlJc w:val="left"/>',
                 '<w:pPr><w:tabs><w:tab w:val="num" w:pos="%d"/></w:tabs><w:ind w:left="%d" w:hanging="360"/></w:pPr></w:lvl>'),
          i, fmt, text, left, left)
}

.rb_docx_numbering <- function(file) {
  if (!requireNamespace("zip", quietly = TRUE)) return(invisible(FALSE))
  dir <- tempfile("docx_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(file, exdir = dir)
  doc_path <- file.path(dir, "word", "document.xml")
  body <- paste(readLines(doc_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl("@@RBN:", body, fixed = TRUE)) return(invisible(FALSE))
  num_path <- file.path(dir, "word", "numbering.xml")
  numbering <- if (file.exists(num_path)) paste(readLines(num_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
  if (!nzchar(numbering)) return(invisible(FALSE))
  ids <- function(pattern) {
    m <- regmatches(numbering, gregexpr(pattern, numbering, perl = TRUE))[[1]]
    if (length(m)) max(as.integer(m)) else 0L
  }
  first_abstract <- ids('(?<=w:abstractNumId=")[0-9]+') + 1L
  first_num <- ids('(?<=<w:num w:numId=")[0-9]+') + 1L

  lists <- list()
  paras <- gregexpr("(?s)<w:p(?: [^>]*)?>.*?</w:p>", body, perl = TRUE)[[1]]
  starts <- as.integer(paras)
  ends <- starts + attr(paras, "match.length") - 1
  pieces <- character()
  last <- 1
  for (k in seq_along(starts)) {
    chunk <- substr(body, starts[k], ends[k])
    token <- regmatches(chunk, regexec("@@RBN:([0-9]+):([0-9]+):([a-z]+)@@", chunk))[[1]]
    if (length(token) == 4) {
      key <- token[2]
      level <- as.integer(token[3])
      if (is.null(lists[[key]])) lists[[key]] <- list(num = first_num + length(lists), kinds = character())
      if (is.na(lists[[key]]$kinds[level])) lists[[key]]$kinds[level] <- token[4]
      chunk <- sub("(?s)<w:r(?: [^>]*)?>(?:(?!</w:r>).)*@@RBN:[^@]*@@(?:(?!</w:r>).)*</w:r>", "", chunk, perl = TRUE)
      num_pr <- sprintf('<w:numPr><w:ilvl w:val="%d"/><w:numId w:val="%d"/></w:numPr>', level - 1L, lists[[key]]$num)
      # numPr comes before the paragraph's borders, shading, tabs, spacing and indents, as the schema orders them
      at <- regexpr("<w:(pBdr|shd|tabs|spacing|ind|jc|rPr)[ />]|</w:pPr>", chunk, perl = TRUE)
      if (at > 0) chunk <- paste0(substr(chunk, 1, at - 1), num_pr, substr(chunk, at, nchar(chunk)))
    }
    pieces <- c(pieces, substr(body, last, starts[k] - 1), chunk)
    last <- ends[k] + 1
  }
  body <- paste0(paste(pieces, collapse = ""), substr(body, last, nchar(body)))

  abstracts <- character()
  nums <- character()
  for (j in seq_along(lists)) {
    kinds <- lists[[j]]$kinds
    # a level not used takes the kind of the level above it
    for (i in seq_len(9)) if (i > length(kinds) || is.na(kinds[i])) kinds[i] <- if (i > 1) kinds[i - 1] else "bullet"
    aid <- first_abstract + j - 1L
    levels <- vapply(0:8, function(i) .rb_num_level(i, kinds[i + 1]), character(1))
    abstracts <- c(abstracts, sprintf('<w:abstractNum w:abstractNumId="%d"><w:multiLevelType w:val="hybridMultilevel"/>%s</w:abstractNum>', aid, paste(levels, collapse = "")))
    nums <- c(nums, sprintf('<w:num w:numId="%d"><w:abstractNumId w:val="%d"/></w:num>', lists[[j]]$num, aid))
  }
  # every abstractNum before the first num
  at <- regexpr("<w:num w:numId=", numbering, fixed = TRUE)
  if (at > 0) {
    numbering <- paste0(substr(numbering, 1, at - 1), paste(abstracts, collapse = ""), substr(numbering, at, nchar(numbering)))
  } else {
    numbering <- sub("</w:numbering>", paste0(paste(abstracts, collapse = ""), "</w:numbering>"), numbering, fixed = TRUE)
  }
  numbering <- sub("</w:numbering>", paste0(paste(nums, collapse = ""), "</w:numbering>"), numbering, fixed = TRUE)
  writeLines(body, doc_path, useBytes = TRUE)
  writeLines(enc2utf8(numbering), num_path, useBytes = TRUE)
  out <- tempfile(fileext = ".docx")
  files <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- c(files[files == "[Content_Types].xml"], files[files != "[Content_Types].xml"])
  zip::zip(out, files = files, root = dir, mode = "mirror")
  file.copy(out, file, overwrite = TRUE)
  unlink(out)
  invisible(TRUE)
}

# The paragraph properties of a block as the theme's style has them, with the block's own alignment / list indent. Word
# replaces a style's paragraph settings with the ones given here, so they repeat the style's spacing. The style is part of
# them: body_add_fpar(style = ) would drop the rest.
.rb_par <- function(design, type, align = "left", list_item = FALSE, last = TRUE, keep_next = FALSE, b = NULL, first = TRUE,
                    level = 1, continued = FALSE) {
  align <- if (align %in% c("left", "center", "right", "justify")) align else "left"
  num <- function(x, default) if (is.numeric(x) && length(x) == 1 && !is.na(x)) x else default
  cm <- 72 / 2.54
  left <- num(b$indent_left, 0) * cm
  right <- num(b$indent_right, 0) * cm
  after <- num(b$space_after, num(design$paragraph_after, 6))
  before <- num(b$space_before, 0)
  line <- num(b$line, num(design$line_spacing, 1.15))
  # the bullet at the margin, the text at a tab stop where the hanging indent ends
  level <- max(1, as.integer(level %||% 1))
  indent <- if (list_item) {
    list(padding.left = left + 18 * level, hanging = 18, tabs = officer::fp_tabs(officer::fp_tab(pos = (left + 18 * level) / 72, style = "left")))
  } else if (continued) {
    # a further paragraph of a list item, at the item's text
    list(padding.left = left + 18 * level)
  } else {
    list(padding.left = left)
  }
  if (type == "quote") {
    # a quotation: a line at the left in the accent colour, the text indented
    args <- list(text.align = align, line_spacing = line, padding.bottom = if (last) after else 2, padding.top = if (first) before + 2 else 0,
                 border.left = officer::fp_border(color = design$accent, width = 2), word_style = "Normal")
    indent$padding.left <- indent$padding.left + 10
  } else if (type == "pre") {
    args <- list(text.align = "left", line_spacing = 1, padding = 6, padding.bottom = 6, shading.color = "#f4f5f6",
                 border = officer::fp_border(color = "#e3e6e9", width = 0.75), word_style = "Normal")
    indent$padding.left <- 6 + indent$padding.left
  } else if (type == "note") {
    border <- officer::fp_border(color = design$note_border, width = 1)
    args <- list(text.align = align, padding = 6, padding.bottom = if (last) 6 else 2, shading.color = design$note_fill,
                 border = border, line_spacing = num(b$line, 1.1), word_style = "Notes Box")
    indent$padding.left <- 6 + indent$padding.left
  } else {
    args <- list(text.align = align, line_spacing = line, padding.bottom = if (last) after else 2,
                 padding.top = if (first) before else 0, word_style = "Normal")
  }
  args$padding.right <- right + if (type == "note") 6 else 0
  if (keep_next) args$keep_with_next <- TRUE
  do.call(officer::fp_par, c(args, indent))
}

# A paragraph, list, note, quote or preformatted block as Word paragraphs. `keep_next`: kept on the page of what follows
# (a short title before a chart).
.rb_docx_fpars <- function(b, design, fields, keep_next = FALSE) {
  type <- b$type
  if (identical(type, "pre")) {
    # as typed: every line kept, in a fixed-width font
    mono <- officer::fp_text_lite(font.family = "Consolas", hansi.family = "Consolas", font.size = .rb_pt(max(7, design$body_size - 1)), color = design$text_color)
    text <- gsub("\\r", "", .rb_fill(b$text %||% "", fields))
    runs <- list()
    for (l in strsplit(text, "\\n", fixed = FALSE)[[1]]) {
      if (length(runs)) runs <- c(runs, list(officer::run_linebreak()))
      runs <- c(runs, list(officer::ftext(if (nzchar(l)) l else " ", mono)))
    }
    if (!length(runs)) runs <- list(officer::ftext(" ", mono))
    return(list(do.call(officer::fpar, c(runs, list(fp_p = .rb_par(design, "pre", "left", b = b))))))
  }
  lines <- .rb_lines(.rb_fill(b$text, fields, escape = grepl("<", b$text %||% "", fixed = TRUE)))
  align <- b$align %||% "left"
  runs_of <- function(line) {
    lapply(line, function(r) {
      # a quotation is in italics, in the muted colour unless coloured
      if (identical(type, "quote")) {
        r$italic <- TRUE
        if (is.null(r$color) || is.na(r$color)) r$color <- design$muted_color
      }
      .rb_ftext(r, text_color = design$text_color)
    })
  }
  nested <- any(vapply(lines, function(l) !is.null(attr(l, "level")), logical(1)))
  if (nested) {
    # a list (in a list block, a note or a quote): one Word paragraph per item, the marker at the item's level; the lines
    # between items (plain paragraphs) together, as one paragraph with line breaks
    out <- list()
    plain <- list()
    started <- FALSE
    put_plain <- function(last) {
      if (!length(plain)) return()
      runs <- list()
      for (i in seq_along(plain)) {
        if (i > 1) runs <- c(runs, list(officer::run_linebreak()))
        runs <- c(runs, runs_of(plain[[i]]))
      }
      out[[length(out) + 1]] <<- do.call(officer::fpar, c(runs, list(fp_p = .rb_par(design, if (type == "list") "paragraph" else type, align, last = last, b = b, first = !length(out)))))
      plain <<- list()
    }
    for (i in seq_along(lines)) {
      line <- lines[[i]]
      level <- attr(line, "level")
      kind <- attr(line, "list")
      last <- i == length(lines)
      if (is.null(level)) {
        plain[[length(plain) + 1]] <- line
        if (last) put_plain(TRUE)
        next
      }
      put_plain(FALSE)
      ptype <- if (type == "list") "paragraph" else type
      if (!is.null(kind)) {
        new_list <- !started || (identical(as.integer(level), 1L) && identical(as.integer(attr(line, "num")), 1L))
        started <- TRUE
        runs <- c(list(.rb_num_token(kind, level, new_list)), runs_of(line))
        par <- .rb_par(design, ptype, align, TRUE, last, b = b, first = !length(out), level = level)
      } else {
        runs <- runs_of(line)
        if (!length(runs)) runs <- list(officer::ftext(""))
        par <- .rb_par(design, ptype, align, FALSE, last, b = b, first = !length(out), level = level, continued = TRUE)
      }
      out[[length(out) + 1]] <- do.call(officer::fpar, c(runs, list(fp_p = par)))
    }
    return(out)
  }
  if (!is.null(b$list) && b$list %in% c("bullet", "number")) {
    lapply(seq_along(lines), function(i) {
      runs <- c(list(.rb_num_token(b$list, 1L, i == 1)), lapply(lines[[i]], .rb_ftext, text_color = design$text_color))
      do.call(officer::fpar, c(runs, list(fp_p = .rb_par(design, type, align, TRUE, i == length(lines), b = b, first = i == 1))))
    })
  } else {
    runs <- list()
    for (i in seq_along(lines)) {
      if (i > 1) runs <- c(runs, list(officer::run_linebreak()))
      runs <- c(runs, runs_of(lines[[i]]))
    }
    if (!length(runs)) runs <- list(officer::ftext(""))
    list(do.call(officer::fpar, c(runs, list(fp_p = .rb_par(design, if (type == "list") "paragraph" else type, align, keep_next = keep_next, b = b)))))
  }
}

# ---- HTML ----------------------------------------------------------------------------------------------------------

.rb_html_runs <- function(line) {
  paste(vapply(line, function(r) {
    s <- htmltools::htmlEscape(r$text)
    if (isTRUE(r$bold)) s <- paste0("<b>", s, "</b>")
    if (isTRUE(r$italic)) s <- paste0("<i>", s, "</i>")
    if (isTRUE(r$underline)) s <- paste0("<u>", s, "</u>")
    if (isTRUE(r$strike)) s <- paste0("<s>", s, "</s>")
    if (identical(r$valign, "subscript")) s <- paste0("<sub>", s, "</sub>")
    if (identical(r$valign, "superscript")) s <- paste0("<sup>", s, "</sup>")
    if (!is.na(r$href %||% NA) && grepl("^(https?://|mailto:)", r$href)) s <- paste0("<a href=\"", htmltools::htmlEscape(r$href, attribute = TRUE), "\">", s, "</a>")
    css <- c(
      if (!is.na(r$color %||% NA)) paste0("color:", r$color),
      if (!is.na(r$highlight %||% NA)) paste0("background-color:", r$highlight),
      if (!is.na(r$family %||% NA)) paste0("font-family:'", r$family, "'"),
      if (!is.na(r$size %||% NA)) paste0("font-size:", r$size, "pt")
    )
    if (length(css)) s <- paste0("<span style=\"", paste(css, collapse = ";"), "\">", s, "</span>")
    s
  }, character(1)), collapse = "")
}

.rb_html_text <- function(b, fields) {
  if (identical(b$type, "pre")) {
    return(paste0('<pre class="pre">', htmltools::htmlEscape(.rb_fill(b$text %||% "", fields)), "</pre>"))
  }
  lines <- .rb_lines(.rb_fill(b$text, fields, escape = grepl("<", b$text %||% "", fixed = TRUE)))
  if (any(vapply(lines, function(l) !is.null(attr(l, "level")), logical(1)))) {
    # lists: each item with its marker, indented to its level
    items <- vapply(lines, function(l) {
      level <- attr(l, "level") %||% 0
      mark <- if (!is.null(attr(l, "list"))) paste0('<span class="mark">', .rb_marker(attr(l, "list"), level, attr(l, "num")), "</span>") else ""
      paste0('<div style="margin-left:', 18 * level, 'pt;text-indent:', if (nzchar(mark)) -14 else 0, 'pt">', mark, .rb_html_runs(l), "</div>")
    }, character(1))
    cls <- if (identical(b$type, "note")) "note" else if (identical(b$type, "quote")) "quote" else "p"
    return(paste0('<div class="', cls, '">', paste(items, collapse = ""), "</div>"))
  }
  align <- b$align %||% "left"
  items <- vapply(lines, .rb_html_runs, character(1))
  if (!is.null(b$list) && b$list %in% c("bullet", "number")) {
    tag <- if (b$list == "bullet") "ul" else "ol"
    body <- paste0("<", tag, ">", paste0("<li>", items, "</li>", collapse = ""), "</", tag, ">")
  } else {
    body <- paste(items, collapse = "<br>")
  }
  cls <- if (identical(b$type, "note")) "note" else if (identical(b$type, "quote")) "quote" else "p"
  num <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  css <- c(paste0("text-align:", align),
           if (num(b$indent_left)) paste0("margin-left:", b$indent_left, "cm"),
           if (num(b$indent_right)) paste0("margin-right:", b$indent_right, "cm"),
           if (num(b$space_before)) paste0("margin-top:", b$space_before, "pt"),
           if (num(b$space_after)) paste0("margin-bottom:", b$space_after, "pt"),
           if (num(b$line)) paste0("line-height:", b$line * 1.2))
  paste0("<div class=\"", cls, "\" style=\"", paste(css, collapse = ";"), "\">", body, "</div>")
}
