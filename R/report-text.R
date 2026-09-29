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

# ---- Word ----------------------------------------------------------------------------------------------------------

# ---- HTML ----------------------------------------------------------------------------------------------------------

